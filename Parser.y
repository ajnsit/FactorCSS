--
-- This file is part of FactorCSS
-- Licensed under the MIT License,
--                http://www.opensource.org/licenses/mit-license
-- Copyright 2004 James Bursa <james@semichrome.net>
--

-- Parser for CSS 2.1
-- See CSS 2.1 G.2

{
module Parser where
import Tokeniser
import CSS
}

%name parser
%tokentype { TokenPosn }
%token
	S		{ (S		, _) }
	INCLUDES	{ (INCLUDES	, _) }
	DASHMATCH	{ (DASHMATCH	, _) }
	LBRACE		{ (LBRACE	, _) }
	PLUS		{ (PLUS		, _) }
	GREATER		{ (GREATER	, _) }
	COMMA		{ (COMMA	, _) }
	STRING		{ (STRING $$	, _) }
	IDENT		{ (IDENT $$	, _) }
	HASH		{ (HASH $$	, _) }
	IMPORT_SYM	{ (IMPORT_SYM	, _) }
	PAGE_SYM	{ (PAGE_SYM	, _) }
	MEDIA_SYM	{ (MEDIA_SYM	, _) }
	CHARSET_SYM	{ (CHARSET_SYM	, _) }
	IMPORTANT_SYM	{ (IMPORTANT_SYM, _) }
	EMS		{ (EMS $$	, _) }
	EXS		{ (EXS $$	, _) }
	LENGTH		{ (LENGTH $$	, _) }
	ANGLE		{ (ANGLE $$	, _) }
	TIME		{ (TIME $$	, _) }
	FREQ		{ (FREQ $$	, _) }
	DIMEN		{ (DIMEN $$	, _) }
	PERCENTAGE	{ (PERCENTAGE $$, _) }
	NUMBER		{ (NUMBER $$	, _) }
	URI		{ (URI $$	, _) }
	FUNCTION	{ (FUNCTION $$	, _) }
	DELIM		{ (DELIM $$	, _) }
	SEMI		{ (SEMI		, _) }
	RBRACE		{ (RBRACE	, _) }
	COLON		{ (COLON	, _) }
	SLASH		{ (SLASH	, _) }
	MINUS		{ (MINUS	, _) }
	DOT		{ (DOT		, _) }
	LBRAC		{ (LBRAC	, _) }
	RBRAC		{ (RBRAC	, _) }
	ASTERISK	{ (ASTERISK	, _) }
	EQUALS		{ (EQUALS	, _) }
	RPAREN		{ (RPAREN	, _) }

%%

stylesheet	:: { Stylesheet }
		: charset s import_list s statement_list s
							{ Stylesheet $1 $3 $5 }

charset		:: { Maybe String }
		:					{ Nothing }
		| CHARSET_SYM s STRING s SEMI		{ Just $3 }

import_list	:: { [(String, [String])] }
		:					{ [] }
		| import_list s import			{ $3 : $1 }

import		:: { (String, [String]) }
		: IMPORT_SYM s STRING s medium_list SEMI s
							{ ($3, $5) }
		| IMPORT_SYM s URI s medium_list SEMI s	{ ($3, $5) }

statement_list	:: { [Statement] }
		:					{ [] }
		| statement_list s statement		{ $3 : $1 }

statement	:: { Statement }
		: ruleset				{ $1 }
		| media					{ $1 }
		| page					{ $1 }

medium_list	:: { [String] }
		:					{ [] }
		| medium_list_1				{ $1 }

medium_list_1	:: { [String] }
		: medium				{ [$1] }
		| medium_list_1 s COMMA s medium	{ $5 : $1 }

media		:: { Statement }
		: MEDIA_SYM s medium_list_1 LBRACE s ruleset_list RBRACE
							{ Media $3 $6 }

medium		:: { String }
		: IDENT					{ $1 }

ruleset_list	:: { [Statement] }
		:					{ [] }
		| ruleset_list s ruleset		{ $3 : $1 }

page		:: { Statement }
		: PAGE_SYM LBRACE s declaration_list_1 RBRACE
							{ Page Nothing $4 }
		| PAGE_SYM s pseudo_page LBRACE s declaration_list_1 RBRACE
							{ Page (Just $3) $6 }

declaration_list_1 :: { [Declaration] }
		:					{ [] }
		| declaration				{ [$1] }
		| declaration_list_1 s SEMI s		{ $1 }
		| declaration_list_1 s SEMI s declaration
							{ $5 : $1 }

pseudo_page	:: { String }
		: COLON IDENT				{ $2 }

ruleset		:: { Statement }
		: selector_list_1 LBRACE s declaration_list_1 RBRACE
							{ Ruleset $1 $4 }

selector_list_1	:: { [Selector] }
		: selector				{ [$1] }
		| selector_list_1 COMMA s selector	{ $4 : $1 }

selector	:: { Selector }
		: simple_selector			{ [($1, NoMore)] }
		| simple_selector combinator selector
							{ ($1, $2) : $3 }

combinator	:: { Combinator }
		: PLUS s				{ Preceded }
		| GREATER s				{ Parent }
		| S					{ Ancestor }

simple_selector	:: { SimpleSelector }
		: element_name detail_list_1		{ ($1, $2) }
		| element_name				{ ($1, []) }
		| detail_list_1				{ (Nothing, $1) }

element_name	:: { Maybe String }
		: IDENT					{ Just $1 }
		| ASTERISK				{ Nothing }

detail_list_1	:: { [Detail] }
		: HASH					{ [Id $1] }
		| class					{ [$1] }
		| attrib				{ [$1] }
		| pseudo				{ [$1] }
		| detail_list_1 HASH			{ Id $2 : $1 }
		| detail_list_1 class			{ $2 : $1 }
		| detail_list_1 attrib			{ $2 : $1 }
		| detail_list_1 pseudo			{ $2 : $1 }

class		:: { Detail }
		: DOT IDENT				{ Class $2 }

attrib		:: { Detail }
		: LBRAC s IDENT s RBRAC			{ Attrib $3 }
		| LBRAC s IDENT s EQUALS s IDENT s RBRAC
							{ AttribEq $3 $7 }
		| LBRAC s IDENT s EQUALS s STRING s RBRAC
							{ AttribEq $3 $7 }
		| LBRAC s IDENT s INCLUDES s IDENT s RBRAC
							{ AttribInc $3 $7 }
		| LBRAC s IDENT s INCLUDES s STRING s RBRAC
							{ AttribInc $3 $7 }
		| LBRAC s IDENT s DASHMATCH s IDENT s RBRAC
							{ AttribDM $3 $7 }
		| LBRAC s IDENT s DASHMATCH s STRING s RBRAC
							{ AttribDM $3 $7 }

pseudo		:: { Detail }
		: COLON IDENT				{ Pseudo $2 }
		| COLON FUNCTION s RPAREN		{ PseudoFunc $2 "" }
		| COLON FUNCTION s IDENT s RPAREN	{ PseudoFunc $2 $4 }

declaration	:: { Declaration }
		: property COLON s value_list_1		{ ($1, $4, False) }
		| property COLON s value_list_1 prio	{ ($1, $4, True) }

property	:: { String }
		: IDENT s				{ $1 }

prio		:: { Bool }
		: IMPORTANT_SYM				{ True }

value_list	:: { [Value] }
		:					{ [] }
		| value s value_list			{ $1 : $3 }

value_list_1	:: { [Value] }
		: value	s				{ [$1] }
		| value s value_list_1			{ $1 : $3 }

value		:: { Value }
		: NUMBER				{ Number $1 }
		| PERCENTAGE				{ Percentage $1 }
		| LENGTH				{ Length $1 }
		| EMS					{ Ems $1 }
		| EXS					{ Exs $1 }
		| ANGLE					{ Angle $1 }
		| TIME					{ Time $1 }
		| FREQ					{ Freq $1 }
		| STRING				{ StringV $1 }
		| IDENT					{ Ident $1 }
		| URI					{ Uri $1 }
		| HASH					{ HexColour $1 }
		| SLASH					{ Slash }
		| COMMA					{ Comma }
		| FUNCTION s value_list s RPAREN	{ Function $1 $3 }

s		:: {}
		:					{ }
		| s S					{ }

{
happyError :: [TokenPosn] -> a
happyError tks = error ("Parse error at " ++ lcn)
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c
			where
			AlexPn _ l c = snd tk
}
