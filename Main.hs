
module Main where

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
     perform args
     
perform [] = 
  do putStrLn "No input file"
     return ()

perform ("-interactive":_) =
  do interactive
  
perform (filename:_) = 
  do h    <- openFile filename ReadMode
     text <- hGetContents h
     putStrLn text
     putStrLn (show . parseprog . lexer $ text)
--     putStrLn (show . lexer $ text)
     


  