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

import Data.Char
import AST

dummy_var_name = "$d"

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17

action_0 (27) = happyShift action_7
action_0 (7) = happyGoto action_23
action_0 (13) = happyGoto action_5
action_0 (14) = happyGoto action_6
action_0 _ = happyFail

action_1 (18) = happyShift action_20
action_1 (19) = happyShift action_21
action_1 (21) = happyShift action_16
action_1 (24) = happyShift action_22
action_1 (8) = happyGoto action_18
action_1 (10) = happyGoto action_19
action_1 _ = happyFail

action_2 (19) = happyShift action_15
action_2 (21) = happyShift action_16
action_2 (24) = happyShift action_17
action_2 (9) = happyGoto action_13
action_2 (10) = happyGoto action_14
action_2 _ = happyFail

action_3 (20) = happyShift action_10
action_3 (21) = happyShift action_11
action_3 (24) = happyShift action_12
action_3 (11) = happyGoto action_8
action_3 (12) = happyGoto action_9
action_3 _ = happyFail

action_4 (27) = happyShift action_7
action_4 (13) = happyGoto action_5
action_4 (14) = happyGoto action_6
action_4 _ = happyFail

action_5 _ = happyReduce_4

action_6 (28) = happyShift action_41
action_6 (15) = happyGoto action_40
action_6 _ = happyFail

action_7 (21) = happyShift action_39
action_7 (16) = happyGoto action_38
action_7 _ = happyFail

action_8 (1) = happyAccept
action_8 _ = happyFail

action_9 (21) = happyShift action_36
action_9 (24) = happyShift action_37
action_9 _ = happyReduce_17

action_10 (21) = happyShift action_35
action_10 _ = happyFail

action_11 _ = happyReduce_18

action_12 (20) = happyShift action_10
action_12 (21) = happyShift action_11
action_12 (24) = happyShift action_12
action_12 (11) = happyGoto action_34
action_12 (12) = happyGoto action_9
action_12 _ = happyFail

action_13 (1) = happyAccept
action_13 _ = happyFail

action_14 (21) = happyShift action_29
action_14 (24) = happyShift action_30
action_14 (26) = happyShift action_33
action_14 _ = happyReduce_10

action_15 (21) = happyShift action_32
action_15 _ = happyFail

action_16 _ = happyReduce_12

action_17 (19) = happyShift action_15
action_17 (21) = happyShift action_16
action_17 (24) = happyShift action_17
action_17 (9) = happyGoto action_25
action_17 (10) = happyGoto action_14
action_17 _ = happyFail

action_18 (1) = happyAccept
action_18 _ = happyFail

action_19 (21) = happyShift action_29
action_19 (24) = happyShift action_30
action_19 (26) = happyShift action_31
action_19 _ = happyFail

action_20 _ = happyReduce_5

action_21 (21) = happyShift action_28
action_21 _ = happyFail

action_22 (18) = happyShift action_20
action_22 (19) = happyShift action_27
action_22 (21) = happyShift action_16
action_22 (24) = happyShift action_22
action_22 (8) = happyGoto action_24
action_22 (9) = happyGoto action_25
action_22 (10) = happyGoto action_26
action_22 _ = happyFail

action_23 (29) = happyAccept
action_23 _ = happyFail

action_24 (25) = happyShift action_56
action_24 _ = happyFail

action_25 (25) = happyShift action_55
action_25 _ = happyFail

action_26 (21) = happyShift action_29
action_26 (24) = happyShift action_30
action_26 (26) = happyShift action_54
action_26 _ = happyReduce_10

action_27 (21) = happyShift action_53
action_27 _ = happyFail

action_28 (22) = happyShift action_52
action_28 _ = happyFail

action_29 _ = happyReduce_14

action_30 (20) = happyShift action_10
action_30 (21) = happyShift action_11
action_30 (24) = happyShift action_12
action_30 (11) = happyGoto action_51
action_30 (12) = happyGoto action_9
action_30 _ = happyFail

action_31 (18) = happyShift action_20
action_31 (19) = happyShift action_21
action_31 (21) = happyShift action_16
action_31 (24) = happyShift action_22
action_31 (8) = happyGoto action_50
action_31 (10) = happyGoto action_19
action_31 _ = happyFail

action_32 (22) = happyShift action_49
action_32 _ = happyFail

action_33 (19) = happyShift action_15
action_33 (21) = happyShift action_16
action_33 (24) = happyShift action_17
action_33 (9) = happyGoto action_48
action_33 (10) = happyGoto action_14
action_33 _ = happyFail

action_34 (25) = happyShift action_47
action_34 _ = happyFail

action_35 (22) = happyShift action_46
action_35 _ = happyFail

action_36 _ = happyReduce_20

action_37 (20) = happyShift action_10
action_37 (21) = happyShift action_11
action_37 (24) = happyShift action_12
action_37 (11) = happyGoto action_45
action_37 (12) = happyGoto action_9
action_37 _ = happyFail

action_38 _ = happyReduce_23

action_39 (22) = happyShift action_44
action_39 _ = happyFail

action_40 _ = happyReduce_22

action_41 (21) = happyShift action_43
action_41 (17) = happyGoto action_42
action_41 _ = happyFail

action_42 _ = happyReduce_24

action_43 (22) = happyShift action_64
action_43 _ = happyFail

action_44 (18) = happyShift action_20
action_44 (19) = happyShift action_21
action_44 (21) = happyShift action_16
action_44 (24) = happyShift action_22
action_44 (8) = happyGoto action_63
action_44 (10) = happyGoto action_19
action_44 _ = happyFail

action_45 (25) = happyShift action_62
action_45 _ = happyFail

action_46 (19) = happyShift action_15
action_46 (21) = happyShift action_16
action_46 (24) = happyShift action_17
action_46 (9) = happyGoto action_61
action_46 (10) = happyGoto action_14
action_46 _ = happyFail

action_47 _ = happyReduce_19

action_48 _ = happyReduce_11

action_49 (19) = happyShift action_15
action_49 (21) = happyShift action_16
action_49 (24) = happyShift action_17
action_49 (9) = happyGoto action_60
action_49 (10) = happyGoto action_14
action_49 _ = happyFail

action_50 _ = happyReduce_8

action_51 (25) = happyShift action_59
action_51 _ = happyFail

action_52 (19) = happyShift action_15
action_52 (21) = happyShift action_16
action_52 (24) = happyShift action_17
action_52 (9) = happyGoto action_58
action_52 (10) = happyGoto action_14
action_52 _ = happyFail

action_53 (22) = happyShift action_57
action_53 _ = happyFail

action_54 (18) = happyShift action_20
action_54 (19) = happyShift action_27
action_54 (21) = happyShift action_16
action_54 (24) = happyShift action_22
action_54 (8) = happyGoto action_50
action_54 (9) = happyGoto action_48
action_54 (10) = happyGoto action_26
action_54 _ = happyFail

action_55 _ = happyReduce_13

action_56 _ = happyReduce_7

action_57 (19) = happyShift action_15
action_57 (21) = happyShift action_16
action_57 (24) = happyShift action_17
action_57 (9) = happyGoto action_70
action_57 (10) = happyGoto action_14
action_57 _ = happyFail

action_58 (23) = happyShift action_69
action_58 _ = happyFail

action_59 _ = happyReduce_15

action_60 (23) = happyShift action_68
action_60 _ = happyFail

action_61 (23) = happyShift action_67
action_61 _ = happyFail

action_62 _ = happyReduce_21

action_63 (23) = happyShift action_66
action_63 _ = happyFail

action_64 (19) = happyShift action_15
action_64 (21) = happyShift action_16
action_64 (24) = happyShift action_17
action_64 (9) = happyGoto action_65
action_64 (10) = happyGoto action_14
action_64 _ = happyFail

action_65 (23) = happyShift action_76
action_65 _ = happyFail

action_66 (21) = happyShift action_39
action_66 (16) = happyGoto action_75
action_66 _ = happyReduce_25

action_67 (20) = happyShift action_10
action_67 (21) = happyShift action_11
action_67 (24) = happyShift action_12
action_67 (11) = happyGoto action_74
action_67 (12) = happyGoto action_9
action_67 _ = happyFail

action_68 (19) = happyShift action_15
action_68 (21) = happyShift action_16
action_68 (24) = happyShift action_17
action_68 (9) = happyGoto action_73
action_68 (10) = happyGoto action_14
action_68 _ = happyFail

action_69 (18) = happyShift action_20
action_69 (19) = happyShift action_21
action_69 (21) = happyShift action_16
action_69 (24) = happyShift action_22
action_69 (8) = happyGoto action_72
action_69 (10) = happyGoto action_19
action_69 _ = happyFail

action_70 (23) = happyShift action_71
action_70 _ = happyFail

action_71 (18) = happyShift action_20
action_71 (19) = happyShift action_27
action_71 (21) = happyShift action_16
action_71 (24) = happyShift action_22
action_71 (8) = happyGoto action_72
action_71 (9) = happyGoto action_73
action_71 (10) = happyGoto action_26
action_71 _ = happyFail

action_72 _ = happyReduce_6

action_73 _ = happyReduce_9

action_74 _ = happyReduce_16

action_75 _ = happyReduce_26

action_76 (21) = happyShift action_43
action_76 (17) = happyGoto action_77
action_76 _ = happyReduce_27

action_77 _ = happyReduce_28

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn8
		 (Type
	)

happyReduce_6 = happyReduce 6 8 happyReduction_6
happyReduction_6 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PiK happy_var_2 happy_var_4  happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  8 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (PiK dummy_var_name happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 6 9 happyReduction_9
happyReduction_9 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (PiA happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (PiA dummy_var_name happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn10
		 (ConstA happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  10 happyReduction_14
happyReduction_14 (HappyTerminal (TokenVar happy_var_2))
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (AppA happy_var_1 (ConstM happy_var_2)
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 10 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AppA happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 11 happyReduction_16
happyReduction_16 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Lam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn12
		 (ConstM happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  12 happyReduction_20
happyReduction_20 (HappyTerminal (TokenVar happy_var_2))
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (App happy_var_1 (ConstM happy_var_2)
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 12 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (App happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_2  13 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 ++ happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  14 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  15 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happyReduce 4 16 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ([HasKind (ConstA happy_var_1) happy_var_3]
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 16 happyReduction_26
happyReduction_26 ((HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((HasKind (ConstA happy_var_1) happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 17 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ([HasType (ConstM happy_var_1) happy_var_3]
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 5 17 happyReduction_28
happyReduction_28 ((HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((HasType (ConstM happy_var_1) happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenType -> cont 18;
	TokenPi -> cont 19;
	TokenLam -> cont 20;
	TokenVar happy_dollar_dollar -> cont 21;
	TokenColon -> cont 22;
	TokenDot -> cont 23;
	TokenOB -> cont 24;
	TokenCB -> cont 25;
	TokenArrow -> cont 26;
	TokenAT "type" -> cont 27;
	TokenAT "term" -> cont 28;
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

parseprog tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

parsekind tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

parsetype tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

parseterm tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

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
