
module Main where

import System.Exit
import System.IO
import System.Environment (getArgs)

import Parser
import AST
import LF
import Error

interactive =
  do text <- getContents
     putStrLn (show . parseprog.lexer $ text)
     
check []              = return()
check (filename:args) =
  do putStrLn (filename ++ ":")
     h    <- openFile filename ReadMode
     text <- hGetContents h
     putStrLn "\tParsing..."
     let sigs = parseprog (lexer text)
     let ctx  = toCtx sigs
     -- putStrLn (show sigs)
     putStrLn "\tChecking..."
     ctxcheck ctx
     hClose h
     check args
     
main = 
  do putStrLn "MaeJi LF Explorer (Ver. 0.1)"
     args <- getArgs
     process args
     
process [] = 
  do putStrLn "No input file"
     return ()

process ("-interactive":_) =
  do interactive
  
process ("-check":args) =
  do check args
     
process (filename:_) = 
  do h    <- openFile filename ReadMode
     text <- hGetContents h
     putStrLn "Parsing..."
     let sigs = parseprog (lexer text)
     let ctx  = toCtx sigs
     -- putStrLn (show sigs)
     putStrLn "Checking..."
     ctxcheck ctx
     shell ctx
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
     res <- termcheck ctx term
     tmchkResult res
  where
    tmchkResult (Left ty)   = prType [] ty
    tmchkResult (Right err) = printError err -- putStr (show err)
    
doComm ctx (':':'k':' ':text) = 
  do let ty = toDBIdxA $ parsetype $ lexer $ text
     res <- typecheck ctx ty
     tychkResult res
  where
    tychkResult (Left ki)   = prKind [] ki
    tychkResult (Right err) = printError err -- putStr (show err)
    
doComm ctx (':':'q':_) = exitSuccess
  
doComm ctx (_) =
  do return ()
     
