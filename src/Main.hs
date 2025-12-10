module Main where

import System.Environment (getArgs)
import Lexer
import Parser
import Data.List
import Control.Monad.State
import SymbolTable
import PrintAST (printAST)
import IR 
import System.Exit (exitWith, ExitCode(..))
import qualified Data.Map.Strict as Map
import MemoryAllocator
import CodeGen
import Lexer (alexScanTokensInsensitive)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [txt] -> runCompiler txt


runCompiler :: FilePath -> IO ()
runCompiler file = do input <- readFile file
                      let ast = parse $ alexScanTokensInsensitive input
                      let (symtab,(errors,scopemem)) = evalState (buildSTProg ast) emptyST
                      writeFile (file ++ "AST.txt") (show ast)
                      writeFile (file ++ "Table.txt") (show (symtab,(errors,scopemem)))
                      if errors /= []
                        then exitWith (ExitFailure 1)
                        else do let (instr, scopesInfo, finishOrder, stringLits, floatLits,whileInfo) = evalState (transAST ast (symtab,scopemem)) emptyIR
                                let scopesInfoList = Map.toList scopesInfo
                                let (addresses, scopeMemoryInfo) = evalState (allocate scopesInfoList finishOrder stringLits floatLits) emptyMem
                                let mipsCode = evalState (transMips instr stringLits floatLits)  (0,[],addresses,scopeMemoryInfo,finishOrder,Map.empty,0,0)
                                --let mipsCode = runState (transMips code1 strs flts) (0,[],addresses,scopeInfo,finishOrder,Map.empty,0)
                                --putStr $ show $ sxt $ snd $ m
                                writeFile (file ++ "IR.txt")  ((unlines $ map show instr) ++ show whileInfo)
                                writeFile (file ++ "Addresses.txt") (show addresses)
                                writeFile (file ++ "Mips.txt") (intercalate "\n" mipsCode)
                                exitWith ExitSuccess


sxt :: (a, b, c, d, e, f, g) -> f
sxt (_, _, _, _, _, x, _) = x





