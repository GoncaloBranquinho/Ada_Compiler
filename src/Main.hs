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
import LivenessAnalysis
import System.FilePath (dropExtension)



main :: IO ()
main = do
    args <- getArgs
    case args of
        [txt] -> runCompiler txt


runCompiler :: FilePath -> IO ()
runCompiler file = do input <- readFile file
                      let baseName = dropExtension file
                      let ast = parse $ alexScanTokensInsensitive input
                      let (symtab,(errors,scopemem)) = evalState (buildSTProg ast) emptyST
                      writeFile (baseName ++ "AST.debugging") (show ast)
                      writeFile (baseName ++ "Table.debugging") (show (symtab,(errors,scopemem)))
                      if errors /= []
                        then exitWith (ExitFailure 1)
                        else do let (instr, scopesInfo, finishOrder, stringLits, floatLits,whileInfo) = evalState (transAST ast (symtab,scopemem)) emptyIR
                                let livenessAnalysisResult = evalState (prepareLA instr >>= \t0 -> iterateLA >>= \t1 -> callDeadCodeElim 1 instr >>= \t2 -> return t2) emptyLA
                                let (newScopesInfo,newStringsLits,newFloatLits, whileInfo) = evalState (analyzeInstr (sxt livenessAnalysisResult)) (Map.empty, [], [], Map.empty)
                                let scopesInfoList = Map.toList newScopesInfo
                                let (addresses, scopeMemoryInfo) = evalState (allocate scopesInfoList finishOrder newStringsLits newFloatLits) emptyMem
                                let mipsCode = evalState (transMips (sxt livenessAnalysisResult) newStringsLits newFloatLits)  (0,[],addresses,scopeMemoryInfo,finishOrder,Map.empty,0,0,whileInfo)
                                --let mipsCode = runState (transMips instr stringLits floatLits)  (0,[],addresses,scopeMemoryInfo,finishOrder,Map.empty,0,0,whileInfo)
                                writeFile (baseName ++ "IR.debugging") (unlines $ map show (instr))
                                writeFile (baseName ++ "IROptimized.debugging")  ((unlines $ map show (sxt (livenessAnalysisResult))))
                                writeFile (baseName ++ "Allocation.debugging") (show addresses)
                                writeFile (baseName ++ ".mips") (intercalate "\n" mipsCode)
                                exitWith ExitSuccess
                                {-
                                -- Sem liveness analysis
                                let scopesInfoList = Map.toList scopesInfo
                                let (addresses, scopeMemoryInfo) = evalState (allocate scopesInfoList finishOrder stringLits floatLits) emptyMem
                                let mipsCode = evalState (transMips instr stringLits floatLits)  (0,[],addresses,scopeMemoryInfo,finishOrder,Map.empty,0,0,whileInfo)
                                writeFile (baseName ++ "IR.debugging")  ((unlines $ map show instr) ++ show scopesInfoList)
                                writeFile (baseName ++ "Allocation.debugging") (show addresses)
                                writeFile (baseName ++ ".mips") (intercalate "\n" mipsCode)
                                exitWith ExitSuccess
                                -}

sxt' :: (a, b, c, d, e, f, g, h, i) -> f
sxt' (_, _, _, _, _, x, _, _, _) = x

sxt :: (a, b, c, d, e, f, g, h) -> h
sxt (_, _, _, _, _, _, _, x) = x





