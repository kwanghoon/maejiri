
module Test where

import Parser
import AST
import LF
import PCCAgent

-- main = getContents >>= print . parser . lexer


tests_kinds =
  [
    "/\\ x : a . Type",
    "( ( ( /\\ x : a . Type ) ) )",
    "/\\ x : t . /\\ y : t . Type",
    "t -> Type",
    "(t -> t) -> Type",
    "/\\ x : t -> t . Type",
    "(t m) -> Type",
    "(/\\ z : t . t) -> Type",
    "/\\ x : a -> b -> c . Type",
    "(/\\ z : t . t z z z) -> Type",
    "(/\\ z : t . t (z z z)) -> Type"
  ]
  
tests_types =
  [
    "/\\x:a. z",
    "a b c d",
    "a -> b -> c -> d",
    "/\\p:o./\\r:o.pf p -> pf r -> pf (and p r)"
  ]
  
tests_terms =
  [
    "x",
    "a b c",
    "a (b c) (d e) f",
    "\\x:b->a. \\y:b. x y",
    "\\x:a1->a2. \\y:a1. x y"
  ]
  
regression_kinds = mapM_ f tests_kinds
  where
    f s = do { let t = parser $ lexer $ s
             ; print t
             ; print $ toDBIdxK $ t
             }

regression_types = mapM_ f tests_types
  where
    f s = do { let t = parsetype $ lexer $ s
             ; print t
             ; print $ toDBIdxA $ t
             }

regression_terms = mapM_ f tests_terms
  where
    f s = do { let t = parseterm $ lexer $ s
             ; print t
             ; print $ toDBIdxM $ t
             }

sig = [ HasKind
        (toDBIdxA $ parsetype $ lexer $ t)
        (toDBIdxK $parsekind  $ lexer $ k)
      | (t,k) <- fol_kind_decls ]
      ++
      [ HasKind
        (toDBIdxA $parsetype $ lexer $ t)
        (toDBIdxK $parsekind  $ lexer $ k)
      | (t,k) <- policy_kind_decls ]
      ++
      [ HasType
        (toDBIdxM $parseterm $ lexer $ te)
        (toDBIdxA $parsetype $ lexer $ ty)
      | (te,ty) <- fol_type_decls ]
      ++
      [ HasType
        (toDBIdxM $parseterm $ lexer $ te)
        (toDBIdxA $parsetype $ lexer $ ty)
      | (te,ty) <- policy_type_decls ]
      ++
      [ HasType
        (toDBIdxM $parseterm $ lexer $ te)
        (toDBIdxA $parsetype $ lexer $ ty)
      | (te,ty) <- extra_decls ]
            
regression_pcc = mapM_ prSig sig

m = case toDBIdxM' ["b", "a"]
         $ parseterm $ lexer $ "\\b:t1. \\a:t2. b a" of
      Lam x a b -> substterm 0 (Var 1 2) b
      
pccagent_ctx =
  ([ (s,k) | HasKind (ConstA s) k <- sig],
   [ (s,t) | HasType (ConstM s) t <- sig],
   [])
  
regression_pcc1 = termcheck_term pf_mm 
  
typecheck_term pf_m =
  let ast = toDBIdxM $ parseterm $ lexer $ pf_m
  in
   do { putStr "Term: ";
      ; putStrLn (show ast);
      ; putStr "Type: "; 
      ; rast <- termcheck pccagent_ctx ast
      ; case rast of
          Left t  -> do { prType [] t; putStrLn "" }
          Right e -> putStrLn (show e)
      }          
  