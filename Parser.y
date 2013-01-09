
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
import Data.List
import AST

dummy_var_name = "$d"

-- type Env = [String]
-- data ParseResult a = Ok a Env | Failed String
-- type P a           = Env -> ParseResult a

-- thenP :: P a -> (a -> P b) -> P b
-- thenP m k = \env ->
--  case m env of
--     Ok a env1 -> k a env1
--     Failed s -> Failed s

-- returnP :: a -> P a
-- returnP a = \env -> Ok a env

-- addP :: String -> P ()
-- addP x = \env -> Ok () (x:env)

-- indexP :: String -> P (Maybe Int)
-- indexP x = \env -> Ok (elemIndex x env) env

-- parseprog toks = parseprog_ toks []
-- parsekind toks = parsekind_ toks []
-- parsetype toks = parsetype_ toks []
-- parseterm toks = parseterm_ toks []

-- %monad { P } { thenP } { returnP }

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
	'='	{ TokenEQ }
	arrow	{ TokenArrow }
	atType	{ TokenAT "type" }
	atTerm	{ TokenAT "term" }
	atDef	{ TokenAT "def" }

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

M	: Lam var ':' A '.' M		{ -- % addP $2
                                          -- `thenP` \ () -> 
                                          -- returnP ( Lam $2 $4 $6 )
                                          Lam $2 $4 $6
                                        }
	| M1				{ $1 }


M1	: var				{ -- % indexP $1
                                          -- `thenP` \ mayber ->
                                          -- returnP $
                                          -- case mayber of
                                          --  Nothing -> ConstM $1
                                          --  Just i  -> Var i
                                          ConstM $1
                                        }
	| '(' M ')'			{ $2 }
	| M1 var			{ App $1 (ConstM $2) }
	| M1 '(' M ')'			{ App $1 $3 }

Decl	: TypeDeclaration 
	  TermDeclaration
	  DefDeclaration 		{ $1 ++ $2 ++ $3 }

TypeDeclaration
	: atType TyDecls		{ $2 }

TermDeclaration
	: atTerm TmDecls		{ $2 }

DefDeclaration
	:				{ [] }
	| atDef DefDecls		{ $2 }

TyDecls	: var ':' K '.'			{ [HasKind (ConstA $1) $3] }
	| var ':' K '.' TyDecls		{ (HasKind (ConstA $1) $3) : $5 }

TmDecls	: var ':' A '.'			{ [HasType (ConstM $1) $3] }
	| var ':' A '.' TmDecls		{ (HasType (ConstM $1) $3) : $5 }

DefDecls: var '=' M '.'			{ [HasDef  (ConstM $1) $3] }
	| var '=' M '.' DefDecls	{ (HasDef  (ConstM $1) $3) : $5 }

{

-- parseError :: [Token] -> P a
parseError :: [Token] -> a
parseError toks = error ("Parse error at " ++
			 (concat 
			  $ intersperse " " 
			  $ map toStr 
			  $ take 30 
			  $ toks))

toStr :: Token -> String
toStr TokenType    = "Type"
toStr TokenPi      = "/\\"
toStr TokenLam     = "\\"
toStr (TokenVar s) = s
toStr TokenColon   = ":"
toStr TokenDot     = "."
toStr TokenOB      = "("
toStr TokenCB      = ")"
toStr TokenArrow   = "->"
toStr (TokenAT s)  = "@"++s
toStr TokenEQ      = "="
toStr tok          = show tok


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
	   | TokenEQ
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
lexer ('=':cs) = TokenEQ : lexer cs
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
lexAt ((TokenVar "def"):nextToks)  = TokenAT "def"  : nextToks
lexAt _                            = error ['@']

isVarChar c = isAlpha c || isDigit c || c == '_' || c == '\''

}

