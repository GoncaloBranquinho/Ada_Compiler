module CodeGen where

import IR
import SymbolTable
import Control.Minad.State
import qualified Data.HashMap.Strict as HashMap


data Location = Reg Temp
              | Stack Offset
              | Global String



type Temp = String
type Offset = Int
type Count = (Int, Int, Int)
type Table = HashMap.HashMap String Location

newOffset :: Int -> State Count ()
newOffset n = do (offset, temp, dataCounter) <- get
              put (offset + n, temp, dataCounter)

newTemp :: State Count ()
newTemp = do (offset, temp, dataCounter) <- get
          put (offset, temp + 1, dataCounter)

popOffset :: Int -> State Count ()
popOffset n = do (offset, temp, dataCounter) <- get
              put (offset - n, temp, dataCounter)

popTemp :: State Count ()
popTemp = do (offset, temp, dataCounter) <- get
          put (offset, temp - 1, dataCounter)


nextLabel :: [Instr] -> String -> Bool
nextLabel (LABEL l1 :_) l2 = l1 == l2
nextLabel _ _ = False


transIR :: [Instr] -> State Count [String]
transIR [] = return [""]
transIR (COND EQ t1 t2 l1 l2:rest) | nextLabel rest l1 = ["beq " + t1 + ", " + t2 + ", " + l1] ++ transIR rest
                                   | nextLabel rest l2 = ["bne " + t1 + ", " + t2 + ", " + l2] ++ transIR rest
                                   | otherwise         = ...
-- falta escolhers registos
