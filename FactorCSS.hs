--
-- This file is part of FactorCSS
-- Licensed under the MIT License,
--                http://www.opensource.org/licenses/mit-license
-- Copyright 2004 James Bursa <james@semichrome.net>
--

-- Factor a CSS stylesheet.
--
-- This tool takes a CSS stylesheet on input and produces an almost equivalent
-- stylesheet on output, but with rulesets split, combined, and reordered to
-- "factor out" common declarations. This helps reveal shared components. The
-- resulting stylesheet may also be smaller.
--
-- The only known case where the output is not equivalent to the input is when
-- the stylesheet depends on the order of rules (see CSS 2.1 6.4.1).

import Data.List
import System.Environment
import Tokeniser
import Parser
import CSS

-- Program entry function.
main :: IO ()
main = do
	args <- getArgs
	interact (run args)

-- Produce output from arguments and input.
run :: [String] -> String -> String
run args input
	| argument "help" args		= usage
	| null args			= go factor
	| argument "factor" args	= go factor
	| argument "explode" args	= go (concatMap explode)
	| argument "identity" args	= go id
	| argument "lex" args		= show (alexScanTokens input) ++ "\n"
	| argument "tree" args		= show stylesheet ++ "\n"
	| argument "statistics" args	= stats stylesheet ++ "\n"
	| otherwise			= usage
	where	go f = (unlines . show_stylesheet . process f) stylesheet
		stylesheet = (parser . alexScanTokens) input

-- Check an argument list for an argument in short or long form.
argument :: String -> [String] -> Bool
argument long args = elem short shorts || elem ("--" ++ long) args
		where	short = head long
			shorts = (concat . filter is_short) args
			is_short ('-':'-':_) = False
			is_short ('-':_) = True
			is_short _ = False

-- Usage help string.
usage :: String
usage = unlines ["Usage: factorcss [OPTION]... <FILE",
		"\"Factor out\" common declarations in a CSS stylesheet by splitting, reordering,",
		"and combining rulesets.",
		"The stylesheet is read from standard input, and the result is produced on",
		"standard output.",
		"",
		"Output mode:",
		"  -f, --factor      factor out common declarations (default)",
		"  -e, --explode     produce rulesets with one selector and declaration each",
		"  -i, --identity    just parse and output unmodified",
		"  -l, --lex         tokenise and display token stream",
		"  -t, --tree        parse and display parse tree",
		"  -s, --statistics  count rulesets, selectors, and declarations",
		"",
		"  -h, --help        display this help and exit"]

-- Process a Stylesheet.
process :: ([Statement] -> [Statement]) -> Stylesheet -> Stylesheet
process f (Stylesheet charset imports stmts) =
		Stylesheet charset imports (process_stmts f stmts)

-- Process a list of Statements.
process_stmts :: ([Statement] -> [Statement]) -> [Statement] -> [Statement]
process_stmts f stmts = (f . filter is_ruleset) stmts ++
		(map (process_media f) . filter is_media) stmts ++
		filter is_page stmts

-- Process a Media Statement.
process_media :: ([Statement] -> [Statement]) -> Statement -> Statement
process_media f (Media media stmts) = Media media (f stmts)

-- Factor a list of Ruleset Statements.
factor :: [Statement] -> [Statement]
factor = map implode_sel . groupBy eq_sel .
		sortBy cmp_sel .
		map implode_decl . groupBy eq_decl .
		sortBy cmp_decl . sortBy cmp_sel .
		concat . map explode

-- Explode a Ruleset Statement into an equivalent list of Ruleset Statements
-- with one Selector and one Declaration per Ruleset.
--
-- For example (in CSS syntax),
--   explode "h1, em { color: red; background-color: blue }" =
--       "h1 { color: red }
--        h1 { background-color: blue }
--        em { color: red }
--        em { background-color: blue }"
--
-- [length (explode (Ruleset sels decls)) == length sels * length decls]
explode :: Statement -> [Statement]
explode (Ruleset sels decls) =
		concat (map (\s -> map (\d -> Ruleset [s] [d]) decls) sels)

-- Compare two Ruleset Statements by the Selectors in each.
cmp_sel :: Statement -> Statement -> Ordering
cmp_sel (Ruleset sels0 decls0) (Ruleset sels1 decls1)
	| a < b		= LT
	| a == b	= EQ
	| a > b		= GT
	where	a = show_selectors sels0
		b = show_selectors sels1

-- Compare two Ruleset Statements by the first Declaration in each.
cmp_decl :: Statement -> Statement -> Ordering
cmp_decl (Ruleset sels0 (decl0:decls0)) (Ruleset sels1 (decl1:decls1))
	| a < b		= LT
	| a == b	= EQ
	| a > b		= GT
	where	a = show_declaration decl0
		b = show_declaration decl1

-- Compare two Ruleset Statements for equality by the Selectors in each.
eq_sel :: Statement -> Statement -> Bool
eq_sel z = (== EQ) . cmp_sel z

-- Compare two Ruleset Statements for equality by the first declaration in
-- each.
eq_decl :: Statement -> Statement -> Bool
eq_decl z = (== EQ) . cmp_decl z

-- Implode a list of Ruleset Statements with equal lists of Selectors to a
-- single equivalent Ruleset Statement.
--
-- For example,
--   implode_sel "h1, em { color: red }
--        h1, em { background-color: blue }" =
--       "h1, em { color: red; background-color: blue }"
implode_sel :: [Statement] -> Statement
implode_sel ((Ruleset sels decls):stmts) =
		Ruleset sels (concat (decls:(map declarations stmts)))

-- Implode a list of Ruleset Statements with equal lists of Declarations to a
-- single equivalent Ruleset Statement.
--
-- For example,
--   implode_decl "h1 { color: red }
--        em { color: red }" =
--       "h1, em { color: red }"
implode_decl :: [Statement] -> Statement
implode_decl ((Ruleset sels decls):stmts) =
		Ruleset (concat (sels:(map selectors stmts))) decls

-- Count rulesets, selectors, and declarations in a Stylesheet.
stats :: Stylesheet -> String
stats (Stylesheet charset imports stmts)
	| n == 0	= "0 rulesets"
	| otherwise	= "rulesets " ++ show n ++
			  ", selectors min " ++ show s0 ++
			  " max " ++ show s1 ++
			  " mean " ++ take 5 (
				show (fromIntegral sn / fromIntegral n)) ++
			  ", declarations min " ++ show d0 ++
			  " max " ++ show d1 ++
			  " mean " ++ take 5 (
				show (fromIntegral dn / fromIntegral n))
	where [n, s0, s1, sn, d0, d1, dn] = stats_stmts stmts

-- Count rulesets, selectors, and declarations in a list of Statements.
stats_stmts :: [Statement] -> [Int]
stats_stmts stmts = stats_rulesets (filter is_ruleset stmts ++
		(concatMap get_stmts (filter is_media stmts)))
		where	get_stmts (Media media stmts) = stmts

-- Count rulesets, selectors, and declarations in a list of Ruleset Statements.
stats_rulesets :: [Statement] -> [Int]
stats_rulesets stmts = [length stmts,
		minimum sel_lengths, maximum sel_lengths, sum sel_lengths,
		minimum decl_lengths, maximum decl_lengths, sum decl_lengths]
		where	sel_lengths = map (length . selectors) stmts
			decl_lengths = map (length . declarations) stmts

-- Extract the list of Selectors from a Ruleset Statement.
selectors :: Statement -> [Selector]
selectors (Ruleset s d) = s

-- Extract the list of Declarations from a Ruleset Statement.
declarations :: Statement -> [Declaration]
declarations (Ruleset s d) = d
