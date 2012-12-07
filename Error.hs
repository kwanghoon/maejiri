
module Error where

import AST

data Error = NotFound1   String ConstTypeEnv Ctx
           | NotFound2   String ConstTermEnv Ctx 
           | NotFound3   Int [(X,A)] Ctx             
           | NotType1    K A Ctx                 
           | NotType2    K A Ctx                 
           | NotType3    K A Ctx                 
           | NotType4    A Error Ctx              
           | NotType5    A Error Ctx             
           | NotMatched1 A A A Ctx
           | NotMatched2 A A M Ctx
           | NotPiK      K Ctx
           | NotPiA      A Ctx
           | AtError     String Error Ctx
           deriving Show

printError (NotFound1 s ctye ctx)
 = putStr $ "NotFound1: " ++ s
printError (NotFound2 s ctme ctx)
 = putStr $ "NotFound2: " ++ s
printError (NotFound3 i xtys ctx)
 = pr (putStr $ "NotFound3: " ++ show i ++ " in ") 
      [ putStrLn (show $ length $ xtys) ]
      -- mapM_ (prType (boundVars ctx)) (snd $ unzip $ xtys)
printError (NotType1 ki ty ctx)   
 = pr (putStr $ "NotType1: ")
      [ prKind (boundVars ctx) ki
      , prType (boundVars ctx) ty ]
printError (NotType2 ki ty ctx)      
 = pr (putStr $ "NotType2: ")
      [ prKind (boundVars ctx) ki
      , prType (boundVars ctx) ty ]
printError (NotType3 ki ty ctx)      
 = pr (putStr $ "NotType3: ")
      [ prKind (boundVars ctx) ki
      , prType (boundVars ctx) ty ]
printError (NotType4 ty err ctx)      
 = pr (putStr $ "NotType4: ")
      [ prType (boundVars ctx) ty
      , printError err ]
printError (NotType5 ty err ctx)      
 = pr (putStr $ "NotType5: ")
      [ prType (boundVars ctx) ty 
      , printError err ]
printError (NotMatched1 ty1 ty2 ty3 ctx)      
 = pr (putStr $ "NotMatched1: ")
      [ prType (boundVars ctx) ty1
      , prType (boundVars ctx) ty2
      , prType (boundVars ctx) ty3 ]
printError (NotMatched2 ty1 ty2 tm ctx)      
 = pr (putStr $ "NotMatched2:")
      [ prType (boundVars ctx) ty1 
      , prType (boundVars ctx) ty2
      , prTerm (boundVars ctx) tm ]
printError (NotPiK ki ctx)      
 = pr (putStr $ "NotPiK: ")
      [ prKind (boundVars ctx) ki ]
printError (NotPiA ty ctx)      
 = pr (putStr $ "NotPiA: ")
      [ prType (boundVars ctx) ty ]
printError (AtError s err ctx) 
 = pr (putStr $ "Error at: ")
      [ putStr s
      , printError err ]


pr io ios =
  do io
     putStrLn ""
     mapM_ (\io -> do { putStr "   "; io; putStrLn "" } ) ios
