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
          return (if (temp < 10) then ("t" ++ (show temp)) else ("s" ++ (show (temp - 10))))

popOffset :: Int -> State Count ()
popOffset n = do (offset, temp, dataCounter) <- get
              put (offset - n, temp, dataCounter)

popTemp :: State Count ()
popTemp = do (offset, temp, dataCounter) <- get
          put (offset, temp - 1, dataCounter)

nextLabel :: [Instr] -> String -> Bool
nextLabel (LABEL l1 :_) l2 = l1 == l2
nextLabel _ _ = False

transMips :: [Instr] -> State Count [String]
transMips instr = do fillData instr
                     return transIR instr

transIR :: [Instr] -> State Count [String]
transIR [] = return []
transIR ((COND EQ t1 t2 l1 l2):remainder) | nextLabel remainder l2 = ["beq " ++ t1 ++ ", " ++ t2 ++ ", " ++ l1] ++ (transIR remainder)
                                          | nextLabel remainder l1 = ["bne " ++ t1 ++ ", " ++ t2 ++ ", " ++ l2] ++ (transIR remainder)
                                          | otherwise              = ["beq " ++ t1 ++ ", " ++ t2 ++ ", " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
transIR ((COND NE t1 t2 l1 l2):remainder) | nextLabel remainder l2 = ["bne " ++ t1 ++ ", " ++ t2 ++ ", " ++ l1] ++ (transIR remainder)
                                          | nextLabel remainder l1 = ["beq " ++ t1 ++ ", " ++ t2 ++ ", " ++ l2] ++ (transIR remainder)
                                          | otherwise              = ["bne " ++ t1 ++ ", " ++ t2 ++ ", " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
transIR ((COND LT t1 t2 l1 l2):remainder) | nextLabel remainder l2 = ["blt " ++ t1 ++ ", " ++ t2 ++ ", " ++ l1] ++ (transIR remainder)
                                          | nextLabel remainder l1 = ["bge " ++ t1 ++ ", " ++ t2 ++ ", " ++ l2] ++ (transIR remainder)
                                          | otherwise              = ["blt " ++ t1 ++ ", " ++ t2 ++ ", " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
transIR ((COND LE t1 t2 l1 l2):remainder) | nextLabel remainder l2 = ["ble " ++ t1 ++ ", " ++ t2 ++ ", " ++ l1] ++ (transIR remainder)
                                          | nextLabel remainder l1 = ["bgt " ++ t1 ++ ", " ++ t2 ++ ", " ++ l2] ++ (transIR remainder)
                                          | otherwise              = ["ble " ++ t1 ++ ", " ++ t2 ++ ", " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
transIR ((LABEL l):remainder) = [l ++ ":"] ++ (transIR remainder)
transIR ((JUMP l):remainder) = ["j " ++ l] ++ (transIR remainder)
transIR ((OP ADD t1 t2 t3):remainder) = ["add " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3] ++ (transIR remainder)
transIR ((OP SUB t1 t2 t3):remainder) = ["sub " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3] ++ (transIR remainder)
transIR ((OP MUL t1 t2 t3):remainder) = ["mul " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3] ++ (transIR remainder)
transIR ((OP DIV t1 t2 t3):remainder) = ["div " ++ t1 ++ ", " ++ t2 ++ ", " ++ t3] ++ (transIR remainder)
transIR ((MOVE t1 t2):remainder) = ["move " ++ t1 ++ ", " ++ t2] ++ (transIR remainder)
transIR ((MOVEI t1 k):remainder) = ["li " ++ t1 ++ ", " ++ k] ++ (transIR remainder)
-- FALTA FAZER VERSOES DIFERENTES PARA FLOAT E INT!
-- falta escolhers registos
