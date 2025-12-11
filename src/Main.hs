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
import MemoryAllocator (removeUnusedTemps)
import AnalyzeIR


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
                                let finalScopeInfoList = removeUnusedTemps scopesInfoList []
                                let (addresses, scopeMemoryInfo) = evalState (allocate scopesInfoList finishOrder stringLits floatLits) emptyMem
                                let (newScopesInfo,newStringsLits,newFloatLits, _) = evalState (analyzeInstr instr) (Map.empty, [], [], Map.empty)
                                let mipsCode = evalState (transMips instr stringLits floatLits)  (0,[],addresses,scopeMemoryInfo,finishOrder,Map.empty,0,0,whileInfo)
                                --let mipsCode = runState (transMips instr stringLits floatLits)  (0,[],addresses,scopeMemoryInfo,finishOrder,Map.empty,0,0,whileInfo)
                                --let k = show $ sxt $ snd $ mipsCode
                                                               --writeFile (file ++ "compare2.txt") (show (newStringsLits) ++ show newFloatLits)
                                --writeFile (file ++ "compare1.txt") (show (stringLits)  ++ show floatLits)
                                writeFile (file ++ "IR.txt")  ((unlines $ map show instr) ++ show scopesInfoList)
                                writeFile (file ++ "Addresses.txt") (show addresses)
                                writeFile (file ++ "Mips.txt") (intercalate "\n" mipsCode)
                                --writeFile (file ++ "Mips.txt") (k)
                                exitWith ExitSuccess


sxt :: (a, b, c, d, e, f, g, h, i) -> f
sxt (_, _, _, _, _, x, _, _, _) = x





