
module LF where

import Data.Maybe
import AST

type Ctx = (ConstTypeEnv, ConstTermEnv, VarTermEnv)

toCtx sigs = (typeDecls, termDecls, [])
  where
    typeDecls = [(s,toDBIdxK k) | HasKind (ConstA s) k <- sigs]
    termDecls = [(s,toDBIdxA t) | HasType (ConstM s) t <- sigs]


type ConstTypeEnv = [(String, K)]
type ConstTermEnv = [(String, A)]
type VarTermEnv   = [A]

data Error = NotFound1   String ConstTypeEnv
           | NotFound2   String ConstTermEnv
           | NotFound3   Int [A]
           | NotType1    K A
           | NotType2    K A
           | NotType3    K A
           | NotType4    A Error
           | NotType5    A Error
           | NotMatched1 A A A
           | NotMatched2 A A String
           | NotPiK      K
           | NotPiA      A
           | AtError     String Error
           deriving Show

addtype (cte1, cte2, vte) a = (cte1, cte2, a:vte)

fst3 (f,_,_) = f
snd3 (_,s,_) = s
thr3 (_,_,t) = t

sigmakindof s (tyctx, _, _) =
  case [ k | (t,k) <- tyctx, s==t ] of
    [k] -> Just k
    _   -> Nothing
    
sigmatypeof s (_, tmctx, _) =
  case [ t | (m,t) <- tmctx, s==m ] of
    [t] -> Just t
    _   -> Nothing
    
typeof i (_, _, vtenv) =
  if length vtenv > i
  then Just $ head (drop i vtenv)
  else Nothing

kindeqv Type Type               = True
kindeqv (PiK x a k) (PiK y b l) = typeeqv a b && kindeqv k l
kindeqv _ _                     = False

typeeqv t1 t2 =
  let whnft1 = typewhnf t1
      whnft2 = typewhnf t2
  in  case (whnft1, whnft2) of
        (ConstA s, ConstA t)   -> s == t
        (PiA x a b, PiA y c d) -> typeeqv a c && typeeqv b d
        (AppA a m, AppA b n)   -> typeeqv a b && termeqv m n
        (_, _)                 -> False

termeqv m1 m2 =
  let whnfm1 = termwhnf m1
      whnfm2 = termwhnf m2
  in  case (whnfm1, whnfm2) of      
        (Var i n, Var j m)     -> i == j
        (ConstM s, ConstM t)   -> s == t
        (Lam x a m, Lam y b n) -> typeeqv a b   && termeqv m n
        (App m1 m2, App m3 m4) -> termeqv m1 m3 && termeqv m2 m4
        (_, _)                 -> False

termwhnf (Var i n)   = Var i n
termwhnf (ConstM s)  = ConstM s
termwhnf (Lam x a m) = Lam x a m
termwhnf (App m n)   = 
  case termwhnf m of
    (Lam x a l) -> termwhnf (shiftterm 0 (-1) (substterm 0 (shiftterm 0 1 n) l))
    m1          -> App m1 n
    
typewhnf (ConstA s)  = ConstA s
typewhnf (PiA x a b) = PiA x a b
typewhnf (AppA a m)  =
  case typewhnf a of
    (PiA x a1 b1) -> typewhnf (shifttype 0 (-1) (substtype 0 (shiftterm 0 1 m) b1))
    a1            -> AppA a1 m


---------------------------------------------------------------------------------
-- A checker for the LF type system
---------------------------------------------------------------------------------
    
typecheck :: Ctx -> A -> IO (Either K Error)
typecheck ctx (ConstA s) =
  case sigmakindof s ctx of
    Just k1 -> return (Left k1)
    Nothing -> return (Right (NotFound1 s (fst3 ctx)))
typecheck ctx (PiA x a b) =
  do { ra <- typecheck ctx a
     ; rb <- typecheck (addtype ctx (shifttype 0 1 a)) b
     ; case (ra, rb) of
         (Left Type, Left Type) -> return (Left Type)
         (Left Type, Left t)    -> return (Right (NotType1 t b))
         (Left Type, rt)        -> return rt
         (Left t,    Left Type) -> return (Right (NotType2 t a))
         (rt,        Left Type) -> return rt
         (Right err, _)         -> return (Right (NotType4 a err))         
         (_, Right err)         -> return (Right (NotType5 b err))
     }
typecheck ctx (AppA a m) =
  do { ra <- typecheck ctx a
     ; rm <- termcheck ctx m
     ; case ra of
         Left (PiK x b k) ->
           (case rm of
              Left b1 -> (if typeeqv b b1
                          then return $ Left (shiftkind 0 (-1)
                                     (substkind 0 (shiftterm 0 1 m) k))
                          else return $ Right (NotMatched1 b b1 a))
              Right s -> return (Right s))
         Left b -> return (Right (NotPiK b))
         ra     -> return ra
     }

termcheck :: Ctx -> M -> IO (Either A Error)
termcheck ctx (ConstM s) =
  case sigmatypeof s ctx of
    Nothing -> return $ Right (NotFound2 s (snd3 ctx))
    Just t  -> return $ Left t
termcheck ctx (Var i n) =
  case typeof i ctx of 
    Nothing -> return $ Right (NotFound3 i (thr3 ctx))
    Just t  -> return $ Left t
termcheck ctx (Lam x a m) = 
  do { ra <- typecheck ctx a
     ; rm <- termcheck (addtype ctx (shifttype 0 1 a)) m
     ; case ra of
         Left Type ->
           (case rm of
              Left b -> return $ Left (PiA x a b)
              rm     -> return rm)
         Left t    -> return $ Right (NotType3 t a)
         Right err -> return $ Right err
     }
termcheck ctx (App m1 m2) =
  do { rm1 <- termcheck ctx m1
     ; rm2 <- termcheck ctx m2
     ; case rm1 of
         Left (PiA x a1 b1) ->
           (case rm2 of
              Left a2 ->
                (if typeeqv a1 a2
                 then return $ Left (shifttype 0 (-1)
                                     (substtype 0
                                      (shiftterm 0 1 m2) b1))
                 else return $ Right (NotMatched2 a1 a2
                                      (show (App m1 m2))))
              rm2'    -> return $ rm2')
         Left t -> return $ Right (NotPiA t)
         rm1'   -> return $ rm1'
     }

ctxcheck :: Ctx -> IO ()
ctxcheck ctx@(kenv, tenv, venv) =
  takeWhile maxErr
     $ -- [loop (tyname, ???check ctx ki) | (tyname,ki) <- kenv]
       -- ++ 
       [loop (tmname, typecheck ctx ty) | (tmname,ty) <- tenv]
     
  where
    maxErr = 5
    
    loop (s,m) =
      do r <- m
         case r of
           Left  res -> return r
           Right err -> -- error $ "Error at " ++ s ++ " : " ++ show err 
                        return $ Right (AtError s err)

    takeWhile n []  = return ()
    takeWhile 0 ios = return ()
    takeWhile n (io:ios) = 
      do r <- io
         case r of
           Left _    -> takeWhile n ios
           Right msg -> do putStrLn (show msg)
                           takeWhile (n-1) ios
                           