module Main where

import System.Environment (getArgs)

import Alg
import Parser

run :: FilePath -> IO ()
run s = do
  code <- readFile s
  case parseExp code of
    Left err -> print err
    Right e  -> runStep [WJug (Inf e (const End))]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> print "Error"
    file:_ -> run file
