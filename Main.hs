
module Main where

import System.Exit
import System.IO
import System.Environment (getArgs)

import Parser
import AST
import LF

interactive =
  do text <- getContents
     putStrLn (show . parseprog.lexer $ text)

main = 
  do args <- getArgs
     process args
     
process [] = 
  do putStrLn "No input file"
     return ()

process ("-interactive":_) =
  do interactive
  
process (filename:_) = 
  do h    <- openFile filename ReadMode
     text <- hGetContents h
     let sigs = parseprog (lexer text)
     -- putStrLn (show sigs)
     shell (toCtx sigs)
     hClose h
     
shell ctx =     
  do putStr "MaeJi> "
     hFlush stdout
     line <- getLine
     doComm ctx line
     putStrLn ""
     shell ctx
     
doComm ctx (':':'t':' ':text) = 
  do let term = toDBIdxM $ parseterm $ lexer $ text
     res <- typecheck ctx term
     tychkResult res
  where
    tychkResult (Left ty)   = prType [] ty
    tychkResult (Right err) = putStr (show err)
    
doComm ctx (':':'q':_) = exitSuccess
  
doComm ctx (_) =
  do return ()
     
