
module LF where

import Data.Maybe
import AST
import Error

kindeqv ctx Type Type level               
  = True
kindeqv ctx (PiK x a k) (PiK y b l) level 
  = typeeqv ctx a b level && kindeqv ctx k l (level+1)
kindeqv _ _ _ _                         
  = False

typeeqv ctx t1 t2 level =
  let whnft1 = typewhnf ctx t1
      whnft2 = typewhnf ctx t2
  in  case (whnft1, whnft2) of
        (ConstA s, ConstA t)   -> s == t
        (PiA x a b, PiA y c d) -> typeeqv ctx a c level 
                                  && typeeqv ctx b d (level+1)
        (AppA a m, AppA b n)   -> typeeqv ctx a b level 
                                  && termeqv ctx m n level
        (_, _)                 -> False

termeqv ctx m1 m2 level =
  let whnfm1 = termwhnf ctx m1
      whnfm2 = termwhnf ctx m2
  in  case (whnfm1, whnfm2) of      
--  Without the level things (Seems to work, but not sure its correctness.)
--        (Var i n, Var j m)     -> i == j || i /= j && (n-m)+j == i
--  With the level things (Should work, but seems to inefficient to maintain levels
        (Var i n, Var j m)     -> i == j && n == m
        (ConstM s, ConstM t)   -> s == t
        (Lam x a m, Lam y b n) -> typeeqv ctx a b level 
                                  && termeqv ctx m n (level+1)
        (App m1 m2, App m3 m4) -> termeqv ctx m1 m3 level 
                                  && termeqv ctx m2 m4 level
        (Lam x a m, _)         -> termeqv ctx m
                                     (App (shiftterm 0 1 whnfm2)
                                          (Var 0 (1+level))) (level+1)
        (_, Lam y b n)         -> termeqv ctx
                                     (App (shiftterm 0 1 whnfm1)
                                          (Var 0 (1+level))) 
                                     n (level+1)
        (_, _)                 -> False

termwhnf ctx (Var i n)   = Var i n
termwhnf ctx (ConstM s)  = 
  case defof s ctx of
       Just m  -> m
       Nothing -> ConstM s
termwhnf ctx (Lam x a m) = Lam x a m
termwhnf ctx (App m n)   = 
  case termwhnf ctx m of
    (Lam x a l) -> termwhnf ctx 
                     (shiftterm 0 (-1) (substterm 0 (shiftterm 0 1 n) l))
    m1          -> App m1 n
    
typewhnf ctx (ConstA s)  = ConstA s
typewhnf ctx (PiA x a b) = PiA x a b
typewhnf ctx (AppA a m)  =
  case typewhnf ctx a of
    (PiA x a1 b1) -> typewhnf ctx
                       (shifttype 0 (-1) (substtype 0 (shiftterm 0 1 m) b1))
    a1            -> AppA a1 m


---------------------------------------------------------------------------------
-- A checker for the LF type system
---------------------------------------------------------------------------------
    
typecheck :: Ctx -> A -> IO (Either K Error)
typecheck ctx (ConstA s) =
  let n = fif5 ctx in
  case sigmakindof s ctx of
    Just k1 -> return (Left (shiftkind 0 n k1))
    Nothing -> return (Right (NotFound1 s (fst5 ctx) ctx))
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
              Left b1 -> (if typeeqv ctx b b1 (level ctx)
                          then return $ Left (shiftkind 0 (-1)
                                     (substkind 0 (shiftterm 0 1 m) k))
                          else return $ Right (NotMatched1 b b1 (AppA a m) ctx))
              Right s -> return (Right s))
         Left b -> return (Right (NotPiK b ctx))
         ra     -> return ra
     }

termcheck :: Ctx -> M -> IO (Either A Error)
termcheck ctx (ConstM s) =
  let n = fif5 ctx in
  case sigmatypeof s ctx of
    Nothing -> return $ Right (NotFound2 s (snd5 ctx) ctx)
    Just t  -> return $ Left (shifttype 0 n t)
termcheck ctx (Var i n) =
  case typeof i ctx of 
    Nothing -> return $ Right (NotFound3 i (for5 ctx) ctx)
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
                (if typeeqv ctx a1 a2 (level ctx)
                 then return $ Left (shifttype 0 (-1)
                                     (substtype 0
                                      (shiftterm 0 1 m2) b1))
                 else return $ Right (NotMatched2 a1 a2 (App m1 m2) ctx))
              rm2'    -> return $ rm2')
         Left t -> return $ Right (NotPiA t ctx)
         rm1'   -> return $ rm1'
     }

maxErr = 5

ctxcheck :: Ctx -> IO Ctx
ctxcheck ctx =
  do errs1        <- tyenvCheck ctx (fst5 ctx)
     errs2        <- tmenvCheck ctx (snd5 ctx)
     (ctx',errs3) <- denvCheck  ctx (thr5 ctx)
     mapM_ printError $ take maxErr $ errs1 ++ errs2 ++ errs3
     return ctx'
    
-- Should check the well-formedness of declared kinds
tyenvCheck :: Ctx -> ConstTypeEnv -> IO [Error]
tyenvCheck ctx kenv = return []

tmenvCheck :: Ctx -> ConstTermEnv -> IO [Error]
tmenvCheck ctx []            = return []
tmenvCheck ctx ((x,ty):tenv) = 
  do e <- typecheck ctx ty
     case e of
       Left  ki  -> tmenvCheck ctx tenv
       Right err -> do { errs <- tmenvCheck ctx tenv
                       ; return (AtError x err ctx : errs) }
       
denvCheck  :: Ctx -> DefEnv -> IO (Ctx, [Error])
denvCheck ctx []           = return (ctx, [])
denvCheck ctx ((x,m):denv) = 
  do e <- termcheck ctx m
     case e of
       Left  ty  -> denvCheck (addtermtype ctx x ty) denv
       Right err -> do { (ctx',errs) <- denvCheck ctx denv
                       ; return (ctx', AtError x err ctx : errs) }
