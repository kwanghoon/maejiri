
{

--------------------------------------------------------------------------------
-- (c) 2011 Kwanghoon Choi
-- kwanghoon.choi@yonsei.ac.kr

-- A simple parser for the LF type theory where objects, types, and kinds
-- are available. The abstract syntax is as follows:

--   Kinds   K ::= Type | PI x:A.K
--   Types   A ::= a | A M | PI x:A1.A2
--   Objects M ::= x | c | M1 M2 | Lam x:A.M

-- The concrete sytanx that the parser is considering is explained by
-- several examples.

--   "/\\ x : a . Type",
--   "( ( ( /\\ x : a . Type ) ) )",
--   "/\\ x : t . /\\ y : t . Type",
--   "t -> Type",
--   "(t -> t) -> Type",
--   "/\\ x : t -> t . Type",
--   "(t m) -> Type",
--   "(/\\ z : t . t) -> Type",
--   "/\\ x : a -> b -> c . Type",
--   "(/\\ z : t . t m mm mmm) -> Type",
--   "(/\\ z : t . t (m mm mmm)) -> Type"
--------------------------------------------------------------------------------

module Parser where

import Data.Char
import AST

dummy_var_name = "$d"

}

%name parseprog
%partial parsekind K
%partial parsetype A
%partial parseterm M


%tokentype { Token }
%error { parseError }

%token
	Type	{ TokenType }
	Pi	{ TokenPi  }
	Lam	{ TokenLam  }
	var	{ TokenVar $$ }
	':'	{ TokenColon }
	'.'	{ TokenDot }
	'('	{ TokenOB }
	')'	{ TokenCB }
	arrow	{ TokenArrow }
	atType	{ TokenAT "type" }
	atTerm	{ TokenAT "term" }

%%

Program	: Decl				{ $1 }

K	: Type				{ Type }
	| Pi var ':'  A '.' K		{ PiK $2 $4  $6 }
	| '(' K ')'			{ $2 }
	| A1 arrow K			{ PiK dummy_var_name $1 $3 }

A	: Pi var ':' A '.' A		{ PiA $2 $4 $6 }
	| A1				{ $1 }
	| A1 arrow A			{ PiA dummy_var_name $1 $3 }

A1	: var				{ ConstA $1 }
	| '(' A ')'			{ $2 }
	| A1 var 			{ AppA $1 (ConstM $2) }
	| A1 '(' M ')' 			{ AppA $1 $3 }

M	: Lam var ':' A '.' M		{ Lam $2 $4 $6 }
	| M1				{ $1 }


M1	: var				{ ConstM $1 }
	| '(' M ')'			{ $2 }
	| M1 var			{ App $1 (ConstM $2) }
	| M1 '(' M ')'			{ App $1 $3 }

Decl	: TypeDeclaration 
	  TermDeclaration 		{ $1 ++ $2 }

TypeDeclaration
	: atType TyDecls		{ $2 }

TermDeclaration
	: atTerm TmDecls		{ $2 }

TyDecls	: var ':' K '.'			{ [HasKind (ConstA $1) $3] }
	| var ':' K '.' TyDecls		{ (HasKind (ConstA $1) $3) : $5 }

TmDecls	: var ':' A '.'			{ [HasType (ConstM $1) $3] }
	| var ':' A '.' TmDecls		{ (HasType (ConstM $1) $3) : $5 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data Token = TokenType
     	   | TokenPi
	   | TokenLam
	   | TokenVar String
	   | TokenColon
	   | TokenDot
	   | TokenOB
	   | TokenCB
	   | TokenArrow
	   | TokenAT String
	   deriving Show


lexer :: String -> [Token]
lexer [] = []
lexer (':':cs) = TokenColon : lexer cs
lexer (';':cs) = lexer (dropWhile (/= '\n') cs)
lexer ('.':cs) = TokenDot : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('/':'\\':cs) = TokenPi : lexer cs
lexer ('\\':cs) = TokenLam : lexer cs
lexer ('@':cs) = lexAt (lexVar cs)
lexer (c:cs) | isSpace c = lexer cs
      	     | isAlpha c = lexVar (c:cs)
--	     | isDigit c = lexNum (c:cs)
lexer (c:cs) = error [c]

-- lexNum cs = TokenInt (read num) : lexer rest
--         where (num,rest) = span isDigit cs

lexVar cs = case span isVarChar cs of
       	       ("Type", rest) -> TokenType : lexer rest
	       (var, rest) -> TokenVar var : lexer rest

lexAt ((TokenVar "type"):nextToks) = TokenAT "type" : nextToks
lexAt ((TokenVar "term"):nextToks) = TokenAT "term" : nextToks
lexAt _                            = error ['@']

isVarChar c = isAlpha c || isDigit c || c == '_' || c == '\''



}

