--
-- This file is part of FactorCSS
-- Licensed under the MIT License,
--                http://www.opensource.org/licenses/mit-license
-- Copyright 2005 James Bursa <james@semichrome.net>
--

-- Datatypes for CSS

module CSS where
import List
import Numeric

data Stylesheet
	= Stylesheet
		(Maybe String)		-- charset
		[(String, [String])]	-- imports (url, media)
		[Statement]
	deriving Show

data Statement
	= Ruleset [Selector] [Declaration]
	| Media [String] [Statement]
	| Page (Maybe String) [Declaration]
	deriving Show

type Selector
	= [(SimpleSelector, Combinator)]

type SimpleSelector
	= (Maybe String, [Detail])

data Detail
	= Id String
	| Class String
	| Attrib String
	| AttribEq String String
	| AttribInc String String
	| AttribDM String String
	| Pseudo String
	| PseudoFunc String String
	deriving Show

data Combinator
	= Ancestor
	| Preceded
	| Parent
	| NoMore
	deriving Show

type Declaration
	= (String, [Value], Bool)

data Value
	= Number Float
	| Percentage Float
	| Length String
	| Ems Float
	| Exs Float
	| Angle String
	| Time String
	| Freq String
	| StringV String
	| Ident String
	| Uri String
	| HexColour String
	| Slash
	| Comma
	| Function String [Value]
	deriving Show


is_ruleset :: Statement -> Bool
is_ruleset (Ruleset sels decls) = True
is_ruleset _ = False

is_media :: Statement -> Bool
is_media (Media media stmts) = True
is_media _ = False

is_page :: Statement -> Bool
is_page (Page page decls) = True
is_page _ = False


show_stylesheet :: Stylesheet -> [String]
show_stylesheet (Stylesheet c is ss) = show_charset c ++
		map show_import (reverse is) ++
		map show_statement ss

show_charset :: Maybe String -> [String]
show_charset Nothing = []
show_charset (Just c) = ["@charset " ++ c ++ ";"]

show_import :: (String, [String]) -> String
show_import (url, media) = "@import " ++ url ++ " " ++
		(concat . intersperse ", ") media ++ ";"

show_statement :: Statement -> String
show_statement (Ruleset ss ds) =
		show_selectors ss ++
		" { " ++
		(concat . intersperse "; " . map show_declaration) ds ++
		" }"
show_statement (Media media stmts) =
		"@media " ++ (concat . intersperse ", ") media ++
		" {\n" ++
		(unlines . map ('\t':) . map show_statement) stmts ++
		"}"
show_statement z = "(statement)"

show_selectors :: [Selector] -> String
show_selectors = concat . intersperse ", " . map show_selector

show_selector :: Selector -> String
show_selector [] = ""
show_selector ((ss, c):zs) = show_simple_selector ss ++
		show_combinator c ++
		show_selector zs

show_simple_selector :: SimpleSelector -> String
show_simple_selector (Just s, ds) = s ++ (concat . map show_detail) ds
show_simple_selector (Nothing, []) = "*"
show_simple_selector (Nothing, ds) = (concat . map show_detail) ds

show_detail :: Detail -> String
show_detail (Id i) = i
show_detail (Class c) = "." ++ c
show_detail (Attrib a) = "[" ++ a ++ "]"
show_detail (AttribEq a v) = "[" ++ a ++ "=" ++ v ++ "]"
show_detail (AttribInc a v) = "[" ++ a ++ "~=" ++ v ++ "]"
show_detail (AttribDM a v) = "[" ++ a ++ "|=" ++ v ++ "]"
show_detail (Pseudo p) = ":" ++ p
show_detail (PseudoFunc f p) = ":" ++ f ++ p ++ ")"

show_combinator :: Combinator -> String
show_combinator Ancestor = " "
show_combinator Preceded = " + "
show_combinator Parent = " > "
show_combinator NoMore = ""

show_declaration :: Declaration -> String
show_declaration (p, vs, i) = p ++ ": " ++ show_values vs ++
		if i then " !important" else ""

show_values :: [Value] -> String
show_values = concat . intersperse " " . map show_value

show_value :: Value -> String
show_value (Number n) = showFFloat Nothing n ""
show_value (Percentage p) = showFFloat Nothing p "%"
show_value (Length l) = l
show_value (Ems e) = showFFloat Nothing e "em"
show_value (Exs x) = showFFloat Nothing x "ex"
show_value (Angle a) = a
show_value (Time t) = t
show_value (Freq f) = f
show_value (StringV s) = s
show_value (Ident i) = i
show_value (Uri u) = u
show_value (HexColour c) = c
show_value Slash = "/"
show_value Comma = ","
show_value (Function f vs) = f ++ show_values vs ++ ")"
