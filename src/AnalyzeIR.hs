module AnalyzeIR where

import IR hiding (addTable, addSetFloat)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad (when)
import Data.Char (digitToInt)

type Label = String
type Info = (Table, [String], [Float], Map.Map Int [WhileInfo])



addSetString :: String -> State Info ()
addSetString str = do (table, strLits, fltLits, whileInfo) <- get
                      let strLits' = if elem str strLits then strLits else (strLits ++ [str])
                      put (table, strLits', fltLits, whileInfo)

addSetFloat :: Float -> State Info ()
addSetFloat flt = do (table, strLits, fltLits, whileInfo) <- get
                     let fltLits' = if elem flt fltLits then fltLits else (fltLits ++ [flt])
                     put (table, strLits, fltLits', whileInfo)



addTable :: String -> Int -> Bool -> State Info ()
addTable str offset isFloat = do (table, strLits, fltLits, whileInfo) <- get
                                 let scope = digitToInt $ head $ tail $ dropWhile (\x -> x/= '@') str
                                 let scopeList = Map.findWithDefault [] scope table
                                 let newScopeList = if (any (\(s,_,_) -> s == str) scopeList)
                                                      then scopeList
                                                      else (scopeList ++ [(str,offset,isFloat)])
                                 put (Map.insert scope newScopeList table, strLits, fltLits, whileInfo)


analyzeInstr :: [Instr] -> State Info (Table,[String],[Float],Map.Map Int [WhileInfo])
analyzeInstr [] = do (table, stringLits, floatLits, whileInfo) <- get
                     return (table,stringLits,floatLits,whileInfo)
analyzeInstr ((MOVEI t1 (TInt lit)):rest) = do addTable t1 4 False
                                               analyzeInstr rest
analyzeInstr ((MOVEI t1 (TString lit)):rest) = do addTable t1 4 False
                                                  addSetString lit
                                                  analyzeInstr rest
analyzeInstr ((MOVEI t1 (TDouble lit)):rest) = do if (head t1) == '_' then addTable t1 4 False else addTable t1 4 True
                                                  addSetFloat lit
                                                  analyzeInstr rest
analyzeInstr ((MOVE t t1 t2):rest) = do case t of
                                               "Float" -> if (head t1) == '_' then addTable t1 4 False else addTable t1 4 True
                                               _       -> addTable t1 4 False
                                        analyzeInstr rest
analyzeInstr ((PRINT t1):rest) = do addTable t1 4 False
                                    analyzeInstr rest
analyzeInstr ((READ t1 t2):rest) = do addTable t1 4 False
                                      addTable t2 4 False
                                      analyzeInstr rest
analyzeInstr ((DECL t1 t):rest) = do if t == "Float" then addTable t1 4 True else addTable t1 4 False
                                     analyzeInstr rest
analyzeInstr ((TOSTR t t1 t2):rest) = do addTable t1 4 False
                                         if t == "Float" && (head t2) /= '_' then addTable t2 4 True else addTable t2 4 False
                                         analyzeInstr rest
analyzeInstr ((OP binOp t1 t2 t3):rest) = do if convertedT == "Float" && (head t1) /= '_' then addTable t1 4 True else addTable t1 4 False
                                             case binOp of
                                                        POW "Float" -> do if (head t2) /= '_' then addTable t2 4 True else addTable t2 4 False
                                                                          addTable t3 4 False
                                                        _           -> do if convertedT == "Float" && (head t2) /= '_' then addTable t2 4 True else addTable t2 4 False
                                                                          if convertedT == "Float" && (head t3) /= '_' then addTable t3 4 True else addTable t3 4 False

                                             analyzeInstr rest
            where convertedT = (\x -> val x) binOp
analyzeInstr ((COND binOp t1 t2 _ _):rest) = do if convertedT == "Float" && (head t1) /= '_' then addTable t1 4 True else addTable t1 4 False
                                                if convertedT == "Float" && (head t2) /= '_' then addTable t2 4 True else addTable t2 4 False
                                                analyzeInstr rest
           where convertedT = (\x -> val x) binOp

analyzeInstr (_:rest) = analyzeInstr rest












