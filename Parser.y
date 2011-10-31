
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

import Char
import AST

dummy_var_name = "$d"

}

%name parser
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

%%

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
	   deriving Show


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
      	     | isAlpha c = lexVar (c:cs)
--	     | isDigit c = lexNum (c:cs)
lexer (':':cs) = TokenColon : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('/':'\\':cs) = TokenPi : lexer cs
lexer ('\\':cs) = TokenLam : lexer cs
lexer (c:cs) = error [c]

-- lexNum cs = TokenInt (read num) : lexer rest
--         where (num,rest) = span isDigit cs

lexVar cs = case span isVarChar cs of
       	       ("Type", rest) -> TokenType : lexer rest
	       (var, rest) -> TokenVar var : lexer rest

isVarChar c = isAlpha c || isDigit c || c == '_' || c == '\''


}

