module Main where

import System.Environment (getArgs)
import Lexer
import Parser
import Data.List
import Control.Monad.State
import SymbolTable
import PrintAST (printAST)
import IntermediateCode (transAST)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [txt] -> do
            input <- readFile txt
            let ast = parse $ alexScanTokensInsensitive input
            printAST ast
        [txt, "1"] -> do
            input <- readFile txt
            print $ fst $ evalState (buildSTProg $ parse $ alexScanTokensInsensitive input) emptyST
        [txt, "2"] -> do
            input <- readFile txt
            let scopes = fst $ snd $ snd $ evalState (buildSTProg $ parse $ alexScanTokensInsensitive input) emptyST
                size = length scopes
            putStrLn $ "A quantidade de âmbitos é (escolha um número de 0 a até ao total de âmbitos - 1, caso contrário mostra todos): " ++ show size
            optStr <- getLine
            let opt = read optStr :: Int
            print (if opt >= 0 && opt < size then [scopes !! opt] else scopes)
        [txt, "3"] -> do
            input <- readFile txt
            print $ evalState (buildSTProg $ parse $ alexScanTokensInsensitive input) emptyST
        [txt, "4"] -> do
            input <- readFile txt
            print $ fst $ snd $ evalState (buildSTProg $ parse $ alexScanTokensInsensitive input) emptyST

        [txt, "5"] -> do 
            input <- readFile txt 
            let table = snd $ snd $ evalState (buildSTProg $ parse $ alexScanTokensInsensitive input) emptyST 
            mapM_ print $ evalState ((transAST $ parse $ alexScanTokensInsensitive input) table) (0, 0, 0)
