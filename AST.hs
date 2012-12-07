
module AST where

import Data.Maybe
import Data.List

-- Basics
type X = String

type Info = [String]
  
-- Kinds 
data K = Type | PiK X A K
       deriving Show

-- Types
data A = ConstA String | AppA A M | PiA X A A  -- LamA X A A ???
       deriving Show                                    

-- Objects
data M = Var Int Int | ConstM String | App M M | Lam X A M
       deriving Show                                                 

-- Signatures
data Sig = HasKind A K
         | HasType M A
         | HasDef  M M
         deriving Show           
                  
-- Contexts                  
type Ctx = (ConstTypeEnv, ConstTermEnv, VarTermEnv, Level)

type ConstTypeEnv = [(String, K)]
type ConstTermEnv = [(String, A)]
type VarTermEnv   = [(X,A)]

fst4 (f,_,_,_) = f
snd4 (_,s,_,_) = s
thr4 (_,_,t,_) = t
for4 (_,_,_,f) = f

boundVars :: Ctx -> [X]
boundVars ctx = fst $ unzip $ thr4 $ ctx

type Level = Int -- binder levels

toCtx sigs = (typeDecls, termDecls, [], 0)
  where
    typeDecls = [(s,toDBIdxK k) | HasKind (ConstA s) k <- sigs]
    termDecls = [(s,toDBIdxA t) | HasType (ConstM s) t <- sigs]


addtype :: Ctx -> X -> A -> Ctx
addtype (cte1, cte2, vte, level) x a = (cte1, cte2, (x,a):vte, level)

inclevel :: Ctx -> Ctx
inclevel (cte1, cte2, vte, level) = (cte1', cte2', vte', level')
  where
    cte1'  = cte1
    cte2'  = cte2
    vte'   = [(x,shifttype 0 1 a)  | (x,a) <- vte]
    level' = level+1


sigmakindof s (tyctx, _, _, _) =
  case [ k | (t,k) <- tyctx, s==t ] of
    [k] -> Just k
    _   -> Nothing
    
sigmatypeof s (_, tmctx, _, _) =
  case [ t | (m,t) <- tmctx, s==m ] of
    [t] -> Just t
    _   -> Nothing
    
typeof i (_, _, vtenv, _) =
  if length vtenv > i
  then Just $ snd $ head (drop i vtenv)
  else Nothing
       
level (_, _, _, l) = l       

--------------------------------------------------------------------------------
-- Replace ConstA v with Var i j in the parsed tree using De Bruijn index
--------------------------------------------------------------------------------
toDBIdxK k = toDBIdxK' [] k

toDBIdxA t = toDBIdxA' [] t

toDBIdxM m = toDBIdxM' [] m
  
toDBIdxK' vars Type        = Type
toDBIdxK' vars (PiK x a k)
  = PiK x
    (toDBIdxA' vars a)
    (toDBIdxK' (x:vars) k)

toDBIdxA' vars (ConstA v) = ConstA v
toDBIdxA' vars (AppA a m)
  = AppA (toDBIdxA' vars a) (toDBIdxM' vars m)
toDBIdxA' vars (PiA x a b)
  = PiA x (toDBIdxA' vars a) (toDBIdxA' (x:vars) b)
    
toDBIdxM' vars (ConstM v)
  = if elem v vars
    then Var (fromJust (elemIndex v vars)) (length vars)
    else ConstM v
toDBIdxM' vars (App m1 m2)
  = App (toDBIdxM' vars m1) (toDBIdxM' vars m2)
toDBIdxM' vars (Lam x a m)
  = Lam x (toDBIdxA' vars a) (toDBIdxM' (x:vars) m)
                     

---------------------------------------------------------------------------------
-- Shifting
---------------------------------------------------------------------------------

shiftkind :: Int -> Int -> K -> K
shiftkind c d Type        = Type
shiftkind c d (PiK x a k) = PiK x (shifttype c d a) (shiftkind (c+1) d k)

shifttype :: Int -> Int -> A -> A
shifttype c d (ConstA s)  = ConstA s
shifttype c d (AppA a m)  = AppA (shifttype c d a) (shiftterm c d m)
shifttype c d (PiA x a b) = PiA x (shifttype c d a) (shifttype (c+1) d b)

shiftterm :: Int -> Int -> M -> M
shiftterm c d (Var k n)   = if k < c then Var k (n+d) else Var (k+d) (n+d)
shiftterm c d (ConstM s)  = ConstM s
shiftterm c d (App m1 m2) = App (shiftterm c d m1) (shiftterm c d m2)
shiftterm c d (Lam x a m) = Lam x (shifttype c d a) (shiftterm (c+1) d m)

---------------------------------------------------------------------------------
-- Substitution
---------------------------------------------------------------------------------

substkind :: Int -> M -> K -> K
substkind j s Type        = Type
substkind j s (PiK x a k) = PiK x (substtype j s a) (substkind (j+1) (shiftterm 0 1 s) k)

substtype :: Int -> M -> A -> A
substtype j s (ConstA ss) = ConstA ss
substtype j s (AppA a m)  = AppA (substtype j s a) (substterm j s m)
substtype j s (PiA x a b) = PiA x (substtype j s a) (substtype (j+1) (shiftterm 0 1 s) b)

substterm :: Int -> M -> M -> M
substterm j s (Var k n)   = if k == j then s else Var k n
substterm j s (ConstM ss) = ConstM ss
substterm j s (App m1 m2) = App (substterm j s m1) (substterm j s m2)
substterm j s (Lam x a m) = Lam x (substtype j s a) (substterm (j+1) (shiftterm 0 1 s) m)


---------------------------------------------------------------------------------
prSig (HasKind a k) =
  do { prType [] a; 
       putStr " : ";
       prKind [] k;
       putStrLn ""; 
     }
prSig (HasType m a) =
  do { prTerm [] m; 
       putStr " : ";
       prType [] a;
       putStrLn ""; 
     }
prSig (HasDef x m) =
  do { prTerm [] x; 
       putStr " : ";
       prTerm [] m;
       putStrLn ""; 
     }
  
---------------------------------------------------------------------------------
prKind :: Info -> K -> IO ()
prKind ctx Type        = putStr "Type"
prKind ctx (PiK x a k) = 
  let (ctx', x') = pickfreshname ctx x 
  in  do { putStr "PIK "; 
           putStr x';
           putStr ":";
           prType ctx a;
           putStr ".";
           prKind ctx' k }

prType :: Info -> A -> IO ()
prType ctx (ConstA c) = putStr c
prType ctx (AppA a m) = 
  do { putStr "(";
       prType ctx a;
       putStr " ";
       prTerm ctx m;
       putStr ")"; }
prType ctx (PiA x a1 a2) =
  let (ctx', x') = pickfreshname ctx x
  in  do { putStr "PIA ";
           putStr x';
           putStr ":";
           prType ctx a1;
           putStr ".";
           prType ctx' a2 }

prTerm :: Info -> M -> IO ()
prTerm ctx (ConstM c)  = putStr c
prTerm ctx (App m1 m2) =
  do { putStr "(";
       prTerm ctx m1;
       putStr " ";
       prTerm ctx m2;
       putStr ")"; }
prTerm ctx (Lam x a m) =
  let (ctx', x') = pickfreshname ctx x
  in  do { putStr "Lam ";
           putStr x';
           putStr ":";
           prType ctx a;
           putStr ".";
           prTerm ctx' m }
prTerm ctx (Var x n) = 
  if length ctx /= n then
--    do { putStr (index2name ctx x) }    
    do { putStr ("bad index: " ++ show x ++ " " ++ show ctx ++ " " ++ show n ) }
  else 
    do { putStr (index2name ctx x) }    

pickfreshname :: Info -> X -> (Info, X)
pickfreshname ctx x = 
  if elem x ctx 
  then pickfreshname ctx (x ++ "'")
  else (x:ctx, x)

index2name :: Info -> Int -> X
index2name ctx i =
  if length ctx > i 
  then head (drop i ctx)
  else error ("index2name: " ++ show i ++ " " ++ show ctx)
