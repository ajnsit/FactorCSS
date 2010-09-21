--
-- This file is part of FactorCSS
-- Licensed under the MIT License,
--                http://www.opensource.org/licenses/mit-license
-- Copyright 2005 James Bursa <james@semichrome.net>
--

-- Tokeniser for CSS 2.1
-- See CSS 2.1 G.2

{
module Tokeniser where
}

%wrapper "posn"

$h		= [0-9a-f]
$nonascii	= [\x80-\xff]
@unicode	= \\$h{1,6}(\r\n|[\ \t\r\n\f])?
@escape		= @unicode|\\[\x20-\x7e\x80-\xff]
@nmstart	= [_a-zA-Z]|$nonascii|@escape
@nmchar		= [\-_a-zA-Z0-9]|$nonascii|@escape
@nl		= \n|\r\n|\r|\f
@string1	= \"([\t\ \!\#\x24\x25&\x28-\x7e]|\\@nl|\'|$nonascii|@escape|\.)*\"
@string2	= \'([\t\ \!\#\x24\x25&\x28-\x7e]|\\@nl|\"|$nonascii|@escape|\.)*\'
@ident		= @nmstart@nmchar*
@name		= @nmchar+
@num		= [\-\053]?([0-9]+|[0-9]*"."[0-9]+)
@string		= @string1|@string2
@url		= ([\!\#\x24\x25&\x2a-\x7e]|$nonascii|@escape)*
$s		= [\ \t\r\n\f]
@w		= $s*
@range		= \?{1,6}|$h(\?{0,5}|$h(\?{0,4}|$h(\?{0,3}|$h(\?{0,2}|$h(\??|$h)))))
$all		= \x00-\xff
@comment	= \/\*$all#\x2a*\*+($all#[\/\x2a]$all#\x2a*\*+)*\/

tokens :-

$s+			{ \p s -> (S, p) }
@comment		;

"<!--"			;
"-->"			;
"~="			{ \p s -> (INCLUDES, p) }
"|="			{ \p s -> (DASHMATCH, p) }

@w"{"			{ \p s -> (LBRACE, p) }
@w"}"			{ \p s -> (RBRACE, p) }
@w"+"			{ \p s -> (PLUS, p) }
@w">"			{ \p s -> (GREATER, p) }
@w","			{ \p s -> (COMMA, p) }

@string			{ \p s -> (STRING s, p) }

@ident			{ \p s -> (IDENT s, p) }

"#"@name		{ \p s -> (HASH s, p) }

"@import"		{ \p s -> (IMPORT_SYM, p) }
"@page"			{ \p s -> (PAGE_SYM, p) }
"@media"		{ \p s -> (MEDIA_SYM, p) }
"@charset"		{ \p s -> (CHARSET_SYM, p) }

"!"@w"important"	{ \p s -> (IMPORTANT_SYM, p) }

@num em			{ \p s -> (EMS (readz (take (length s - 2) s) p), p) }
@num ex			{ \p s -> (EXS (readz (take (length s - 2) s) p), p) }
@num px			{ \p s -> (LENGTH s, p) }
@num cm			{ \p s -> (LENGTH s, p) }
@num mm			{ \p s -> (LENGTH s, p) }
@num in			{ \p s -> (LENGTH s, p) }
@num pt			{ \p s -> (LENGTH s, p) }
@num pc			{ \p s -> (LENGTH s, p) }
@num deg		{ \p s -> (ANGLE s, p) }
@num rad		{ \p s -> (ANGLE s, p) }
@num grad		{ \p s -> (ANGLE s, p) }
@num ms			{ \p s -> (TIME s, p) }
@num s			{ \p s -> (TIME s, p) }
@num Hz			{ \p s -> (FREQ s, p) }
@num kHz		{ \p s -> (FREQ s, p) }
@num @ident		{ \p s -> (DIMEN s, p) }
@num "%"		{ \p s -> (PERCENTAGE (readz (take (length s - 1) s) p), p) }
@num			{ \p s -> (NUMBER (readz s p), p) }

"url("@w@string@w")"	{ \p s -> (URI s, p) }
"url("@w@url@w")"	{ \p s -> (URI s, p) }
@ident"("		{ \p s -> (FUNCTION s, p) }

";"			{ \p s -> (SEMI, p) }
":"			{ \p s -> (COLON, p) }
"/"			{ \p s -> (SLASH, p) }
"-"			{ \p s -> (MINUS, p) }
"."			{ \p s -> (DOT, p) }
"*"			{ \p s -> (ASTERISK, p) }
"["			{ \p s -> (LBRAC, p) }
"]"			{ \p s -> (RBRAC, p) }
"="			{ \p s -> (EQUALS, p) }
")"			{ \p s -> (RPAREN, p) }
.			{ \p s -> (DELIM (head s), p) }


{
type TokenPosn = (Token, AlexPosn)

data Token =
	S		|
	INCLUDES	|
	DASHMATCH	|
	LBRACE		|
	PLUS		|
	GREATER		|
	COMMA		|
	STRING String	|
	IDENT String	|
	HASH String	|
	IMPORT_SYM	|
	PAGE_SYM	|
	MEDIA_SYM	|
	CHARSET_SYM	|
	IMPORTANT_SYM	|
	EMS Float	|
	EXS Float	|
	LENGTH String	|
	ANGLE String	|
	TIME String	|
	FREQ String	|
	DIMEN String	|
	PERCENTAGE Float |
	NUMBER Float	|
	URI String	|
	FUNCTION String |
	SEMI		|
	RBRACE		|
	COLON		|
	SLASH		|
	MINUS		|
	DOT		|
	ASTERISK	|
	LBRAC		|
	RBRAC		|
	EQUALS		|
	RPAREN		|
	DELIM Char
	deriving (Eq, Show)

readz :: Read a => String -> AlexPosn -> a
readz s (AlexPn _ l c) =
	if (null xs) || not (null (snd (head xs)))
	then error ("Parse error at " ++ "line " ++ show l ++
			", column " ++ show c ++
			" (when parsing \"" ++ s ++ "\")")
	else fst (head xs)
		where xs = reads s

}
