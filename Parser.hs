{-# OPTIONS_GHC -w #-}
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

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t7 t8 t9 t10 t11
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11

action_0 (12) = happyShift action_5
action_0 (13) = happyShift action_18
action_0 (15) = happyShift action_14
action_0 (18) = happyShift action_19
action_0 (7) = happyGoto action_20
action_0 (9) = happyGoto action_17
action_0 _ = happyFail

action_1 (12) = happyShift action_5
action_1 (13) = happyShift action_18
action_1 (15) = happyShift action_14
action_1 (18) = happyShift action_19
action_1 (7) = happyGoto action_16
action_1 (9) = happyGoto action_17
action_1 _ = happyFail

action_2 (13) = happyShift action_13
action_2 (15) = happyShift action_14
action_2 (18) = happyShift action_15
action_2 (8) = happyGoto action_11
action_2 (9) = happyGoto action_12
action_2 _ = happyFail

action_3 (14) = happyShift action_8
action_3 (15) = happyShift action_9
action_3 (18) = happyShift action_10
action_3 (10) = happyGoto action_6
action_3 (11) = happyGoto action_7
action_3 _ = happyFail

action_4 (12) = happyShift action_5
action_4 _ = happyFail

action_5 _ = happyReduce_4

action_6 (1) = happyAccept
action_6 _ = happyFail

action_7 (15) = happyShift action_33
action_7 (18) = happyShift action_34
action_7 _ = happyReduce_16

action_8 (15) = happyShift action_32
action_8 _ = happyFail

action_9 _ = happyReduce_17

action_10 (14) = happyShift action_8
action_10 (15) = happyShift action_9
action_10 (18) = happyShift action_10
action_10 (10) = happyGoto action_31
action_10 (11) = happyGoto action_7
action_10 _ = happyFail

action_11 (1) = happyAccept
action_11 _ = happyFail

action_12 (15) = happyShift action_26
action_12 (18) = happyShift action_27
action_12 (20) = happyShift action_30
action_12 _ = happyReduce_9

action_13 (15) = happyShift action_29
action_13 _ = happyFail

action_14 _ = happyReduce_11

action_15 (13) = happyShift action_13
action_15 (15) = happyShift action_14
action_15 (18) = happyShift action_15
action_15 (8) = happyGoto action_22
action_15 (9) = happyGoto action_12
action_15 _ = happyFail

action_16 (1) = happyAccept
action_16 _ = happyFail

action_17 (15) = happyShift action_26
action_17 (18) = happyShift action_27
action_17 (20) = happyShift action_28
action_17 _ = happyFail

action_18 (15) = happyShift action_25
action_18 _ = happyFail

action_19 (12) = happyShift action_5
action_19 (13) = happyShift action_24
action_19 (15) = happyShift action_14
action_19 (18) = happyShift action_19
action_19 (7) = happyGoto action_21
action_19 (8) = happyGoto action_22
action_19 (9) = happyGoto action_23
action_19 _ = happyFail

action_20 (21) = happyAccept
action_20 _ = happyFail

action_21 (19) = happyShift action_46
action_21 _ = happyFail

action_22 (19) = happyShift action_45
action_22 _ = happyFail

action_23 (15) = happyShift action_26
action_23 (18) = happyShift action_27
action_23 (20) = happyShift action_44
action_23 _ = happyReduce_9

action_24 (15) = happyShift action_43
action_24 _ = happyFail

action_25 (16) = happyShift action_42
action_25 _ = happyFail

action_26 _ = happyReduce_13

action_27 (14) = happyShift action_8
action_27 (15) = happyShift action_9
action_27 (18) = happyShift action_10
action_27 (10) = happyGoto action_41
action_27 (11) = happyGoto action_7
action_27 _ = happyFail

action_28 (12) = happyShift action_5
action_28 (13) = happyShift action_18
action_28 (15) = happyShift action_14
action_28 (18) = happyShift action_19
action_28 (7) = happyGoto action_40
action_28 (9) = happyGoto action_17
action_28 _ = happyFail

action_29 (16) = happyShift action_39
action_29 _ = happyFail

action_30 (13) = happyShift action_13
action_30 (15) = happyShift action_14
action_30 (18) = happyShift action_15
action_30 (8) = happyGoto action_38
action_30 (9) = happyGoto action_12
action_30 _ = happyFail

action_31 (19) = happyShift action_37
action_31 _ = happyFail

action_32 (16) = happyShift action_36
action_32 _ = happyFail

action_33 _ = happyReduce_19

action_34 (14) = happyShift action_8
action_34 (15) = happyShift action_9
action_34 (18) = happyShift action_10
action_34 (10) = happyGoto action_35
action_34 (11) = happyGoto action_7
action_34 _ = happyFail

action_35 (19) = happyShift action_52
action_35 _ = happyFail

action_36 (13) = happyShift action_13
action_36 (15) = happyShift action_14
action_36 (18) = happyShift action_15
action_36 (8) = happyGoto action_51
action_36 (9) = happyGoto action_12
action_36 _ = happyFail

action_37 _ = happyReduce_18

action_38 _ = happyReduce_10

action_39 (13) = happyShift action_13
action_39 (15) = happyShift action_14
action_39 (18) = happyShift action_15
action_39 (8) = happyGoto action_50
action_39 (9) = happyGoto action_12
action_39 _ = happyFail

action_40 _ = happyReduce_7

action_41 (19) = happyShift action_49
action_41 _ = happyFail

action_42 (13) = happyShift action_13
action_42 (15) = happyShift action_14
action_42 (18) = happyShift action_15
action_42 (8) = happyGoto action_48
action_42 (9) = happyGoto action_12
action_42 _ = happyFail

action_43 (16) = happyShift action_47
action_43 _ = happyFail

action_44 (12) = happyShift action_5
action_44 (13) = happyShift action_24
action_44 (15) = happyShift action_14
action_44 (18) = happyShift action_19
action_44 (7) = happyGoto action_40
action_44 (8) = happyGoto action_38
action_44 (9) = happyGoto action_23
action_44 _ = happyFail

action_45 _ = happyReduce_12

action_46 _ = happyReduce_6

action_47 (13) = happyShift action_13
action_47 (15) = happyShift action_14
action_47 (18) = happyShift action_15
action_47 (8) = happyGoto action_56
action_47 (9) = happyGoto action_12
action_47 _ = happyFail

action_48 (17) = happyShift action_55
action_48 _ = happyFail

action_49 _ = happyReduce_14

action_50 (17) = happyShift action_54
action_50 _ = happyFail

action_51 (17) = happyShift action_53
action_51 _ = happyFail

action_52 _ = happyReduce_20

action_53 (14) = happyShift action_8
action_53 (15) = happyShift action_9
action_53 (18) = happyShift action_10
action_53 (10) = happyGoto action_60
action_53 (11) = happyGoto action_7
action_53 _ = happyFail

action_54 (13) = happyShift action_13
action_54 (15) = happyShift action_14
action_54 (18) = happyShift action_15
action_54 (8) = happyGoto action_59
action_54 (9) = happyGoto action_12
action_54 _ = happyFail

action_55 (12) = happyShift action_5
action_55 (13) = happyShift action_18
action_55 (15) = happyShift action_14
action_55 (18) = happyShift action_19
action_55 (7) = happyGoto action_58
action_55 (9) = happyGoto action_17
action_55 _ = happyFail

action_56 (17) = happyShift action_57
action_56 _ = happyFail

action_57 (12) = happyShift action_5
action_57 (13) = happyShift action_24
action_57 (15) = happyShift action_14
action_57 (18) = happyShift action_19
action_57 (7) = happyGoto action_58
action_57 (8) = happyGoto action_59
action_57 (9) = happyGoto action_23
action_57 _ = happyFail

action_58 _ = happyReduce_5

action_59 _ = happyReduce_8

action_60 _ = happyReduce_15

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn7
		 (Type
	)

happyReduce_5 = happyReduce 6 7 happyReduction_5
happyReduction_5 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PiK happy_var_2 happy_var_4  happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (PiK dummy_var_name happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 8 happyReduction_8
happyReduction_8 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PiA happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (PiA dummy_var_name happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn9
		 (ConstA happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  9 happyReduction_13
happyReduction_13 (HappyTerminal (TokenVar happy_var_2))
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (AppA happy_var_1 (ConstM happy_var_2)
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 9 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (AppA happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 6 10 happyReduction_15
happyReduction_15 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Lam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn11
		 (ConstM happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  11 happyReduction_19
happyReduction_19 (HappyTerminal (TokenVar happy_var_2))
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (App happy_var_1 (ConstM happy_var_2)
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 11 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (App happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 21 21 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenType -> cont 12;
	TokenPi -> cont 13;
	TokenLam -> cont 14;
	TokenVar happy_dollar_dollar -> cont 15;
	TokenColon -> cont 16;
	TokenDot -> cont 17;
	TokenOB -> cont 18;
	TokenCB -> cont 19;
	TokenArrow -> cont 20;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

parsekind tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

parsetype tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

parseterm tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 311 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
