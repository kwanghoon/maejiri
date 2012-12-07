
module LF where

import Data.Maybe
import AST
import Error

kindeqv Type Type level               = True
kindeqv (PiK x a k) (PiK y b l) level = typeeqv a b level && kindeqv k l (level+1)
kindeqv _ _ _                         = False

typeeqv t1 t2 level =
  let whnft1 = typewhnf t1
      whnft2 = typewhnf t2
  in  case (whnft1, whnft2) of
        (ConstA s, ConstA t)   -> s == t
        (PiA x a b, PiA y c d) -> typeeqv a c level && typeeqv b d (level+1)
        (AppA a m, AppA b n)   -> typeeqv a b level && termeqv m n level
        (_, _)                 -> False

termeqv m1 m2 level =
  let whnfm1 = termwhnf m1
      whnfm2 = termwhnf m2
  in  case (whnfm1, whnfm2) of      
--  Without the level things (Seems to work, but not sure its correctness.)
--        (Var i n, Var j m)     -> i == j || i /= j && (n-m)+j == i
--  With the level things (Should work, but seems to inefficient to maintain levels
        (Var i n, Var j m)     -> i == j && n == m
        (ConstM s, ConstM t)   -> s == t
        (Lam x a m, Lam y b n) -> typeeqv a b level && termeqv m n (level+1)
        (App m1 m2, App m3 m4) -> termeqv m1 m3 level && termeqv m2 m4 level
        (Lam x a m, _)         -> termeqv m
                                     (App (shiftterm 0 1 whnfm2)
                                          (Var 0 (1+level))) (level+1)
        (_, Lam y b n)         -> termeqv 
                                     (App (shiftterm 0 1 whnfm1)
                                          (Var 0 (1+level))) 
                                     n (level+1)
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
  let n = for4 ctx in
  case sigmakindof s ctx of
    Just k1 -> return (Left (shiftkind 0 n k1))
    Nothing -> return (Right (NotFound1 s (fst4 ctx) ctx))
typecheck ctx (PiA x a b) =
  do { ra <- typecheck ctx a
     ; let ctx' = addtype (inclevel ctx) x (shifttype 0 1 a)
     ; rb <- typecheck ctx' b
     ; case (ra, rb) of
         (Left Type, Left Type) -> return (Left Type)
         (Left Type, Left t)    -> return (Right (NotType1 t b ctx'))
         (Left Type, rt)        -> return rt
         (Left t,    Left Type) -> return (Right (NotType2 t a ctx))
         (rt,        Left Type) -> return rt
         (Right err, _)         -> return (Right (NotType4 a err ctx))         
         (_, Right err)         -> return (Right (NotType5 b err ctx'))
     }
typecheck ctx (AppA a m) =
  do { ra <- typecheck ctx a
     ; rm <- termcheck ctx m
     ; case ra of
         Left (PiK x b k) ->
           (case rm of
              Left b1 -> (if typeeqv b b1 (level ctx)
                          then return $ Left (shiftkind 0 (-1)
                                     (substkind 0 (shiftterm 0 1 m) k))
                          else return $ Right (NotMatched1 b b1 (AppA a m) ctx))
              Right s -> return (Right s))
         Left b -> return (Right (NotPiK b ctx))
         ra     -> return ra
     }

termcheck :: Ctx -> M -> IO (Either A Error)
termcheck ctx (ConstM s) =
  let n = for4 ctx in
  case sigmatypeof s ctx of
    Nothing -> return $ Right (NotFound2 s (snd4 ctx) ctx)
    Just t  -> return $ Left (shifttype 0 n t)
termcheck ctx (Var i n) =
  case typeof i ctx of 
    Nothing -> return $ Right (NotFound3 i (thr4 ctx) ctx)
    Just t  -> return $ Left t
termcheck ctx (Lam x a m) = 
  do { ra <- typecheck ctx a
     ; rm <- termcheck (addtype (inclevel ctx) x (shifttype 0 1 a)) m
     ; case ra of
         Left Type ->
           (case rm of
              Left b -> return $ Left (PiA x a b)
              rm     -> return rm)
         Left t    -> return $ Right (NotType3 t a ctx)
         Right err -> return $ Right err
     }
termcheck ctx (App m1 m2) =
  do { rm1 <- termcheck ctx m1
     ; rm2 <- termcheck ctx m2
     ; case rm1 of
         Left (PiA x a1 b1) ->
           (case rm2 of
              Left a2 ->
                (if typeeqv a1 a2 (level ctx)
                 then return $ Left (shifttype 0 (-1)
                                     (substtype 0
                                      (shiftterm 0 1 m2) b1))
                 else return $ Right (NotMatched2 a1 a2 (App m1 m2) ctx))
              rm2'    -> return $ rm2')
         Left t -> return $ Right (NotPiA t ctx)
         rm1'   -> return $ rm1'
     }

ctxcheck :: Ctx -> IO ()
ctxcheck ctx@(kenv, tenv, venv, level) =
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
                        return $ Right (AtError s err ctx)

    takeWhile n []  = return ()
    takeWhile 0 ios = return ()
    takeWhile n (io:ios) = 
      do r <- io
         case r of
           Left _    -> takeWhile n ios
           Right msg -> do putStrLn (show msg)
                           takeWhile (n-1) ios
