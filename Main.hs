module Main where

import System.Environment (getArgs)
import Lexer
import Parser

main :: IO ()
main = do
    args <- getArgs
    case args of
        [txt] -> do
            input <- readFile txt
            print (parse $ alexScanTokensInsensitive input)

