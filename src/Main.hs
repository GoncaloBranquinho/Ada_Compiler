module Main where

import System.Environment (getArgs)
import Lexer
import Parser
import PrintAST (printAST)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [txt] -> do
            input <- readFile txt
            printAST $ parse $ alexScanTokensInsensitive input

