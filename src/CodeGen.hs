module CodeGen where

import IR
import SymbolTable
import Control.Minad.State
import qualified Data.HashMap.Strict as HashMap


data Location = Reg Temp
              | Stack Offset
              | Heap Offset
              | Global String

type Temp = String
type Offset = Int
type Count = (Int, Int, Int, [String])
type Table = HashMap.HashMap String Location

newOffset :: Int -> State Count ()
newOffset n = do (offset, temp, dataCounter, dataList) <- get
              put (offset + n, temp, dataCounter, dataList)

newTemp :: State Count Temp
newTemp = do (offset, temp, dataCounter, dataList) <- get
          put (offset, temp + 1, dataCounter, dataList)
          return (if (temp < 10) then ("t" ++ (show temp)) else ("s" ++ (show (temp - 10))))

newData :: State Count Int
newData = do (offset, temp, dataCounter, dataList) <- get
             put (offset, temp, dataCounter+1, dataList)
             return (dataCounter)

addData :: String -> State Count ()
addData str = do (offset, temp, dataCounter, dataList) <- get
                 put (offset, temp, dataCounter, dataList ++ str)

popOffset :: Int -> State Count ()
popOffset n = do (offset, temp, dataCounter, dataList) <- get
              put (offset - n, temp, dataCounter, dataList)

popTemp :: State Count ()
popTemp = do (offset, temp, dataCounter, dataList) <- get
          put (offset, temp - 1, dataCounter, dataList)

nextLabel :: [Instr] -> String -> Bool
nextLabel (LABEL l1 :_) l2 = l1 == l2
nextLabel _ _ = False

transMips :: [Instr] -> [String] -> State Count [String]
transMips instr stringLiterals = do fillData stringLiterals
                                    code2 <- transIR instr
                                    (_,_,_,dataList) <- get
                                    return ([".data"] ++ [dataList] ++ [".text", "main:",code2])
 

fillData :: [String] -> State Count ()
fillData [] = return ()
fillData (str:remainder) = do dataCounter <- newData 
                              addData ["str" ++ show dataCounter ++ ": .asciiz" ++ str]
                              return fillData remainder

defaultStringSize :: String
defaultStringSize = 256

transIR :: [Instr] -> State Count [String]
transIR [] = return []
transIR ((COND EQ t t1 t2 l1 l2):remainder) | nextLabel remainder l2 = ["beq " ++ t1 ++ "," ++ t2 ++ "," ++ l1] ++ (transIR remainder)
                                          | nextLabel remainder l1 = ["bne " ++ t1 ++ "," ++ t2 ++ "," ++ l2] ++ (transIR remainder)
                                          | otherwise              = ["beq " ++ t1 ++ "," ++ t2 ++ "," ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
transIR ((COND NE t t1 t2 l1 l2):remainder) | nextLabel remainder l2 = ["bne " ++ t1 ++ "," ++ t2 ++ "," ++ l1] ++ (transIR remainder)
                                          | nextLabel remainder l1 = ["beq " ++ t1 ++ "," ++ t2 ++ "," ++ l2] ++ (transIR remainder)
                                          | otherwise              = ["bne " ++ t1 ++ "," ++ t2 ++ "," ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
transIR ((COND LT t t1 t2 l1 l2):remainder) | nextLabel remainder l2 = ["blt " ++ t1 ++ "," ++ t2 ++ "," ++ l1] ++ (transIR remainder)
                                          | nextLabel remainder l1 = ["bge " ++ t1 ++ "," ++ t2 ++ "," ++ l2] ++ (transIR remainder)
                                          | otherwise              = ["blt " ++ t1 ++ "," ++ t2 ++ "," ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
transIR ((COND LE t t1 t2 l1 l2):remainder) | nextLabel remainder l2 = ["ble " ++ t1 ++ "," ++ t2 ++ "," ++ l1] ++ (transIR remainder)
                                          | nextLabel remainder l1 = ["bgt " ++ t1 ++ "," ++ t2 ++ "," ++ l2] ++ (transIR remainder)
                                          | otherwise              = ["ble " ++ t1 ++ "," ++ t2 ++ "," ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
transIR ((LABEL l):remainder) = [l ++ ":"] ++ (transIR remainder)
transIR ((JUMP l):remainder) = ["j " ++ l] ++ (transIR remainder)
transIR ((OP ADD t t1 t2 t3):remainder) = ["add " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
transIR ((OP SUB t t1 t2 t3):remainder) = ["sub " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
transIR ((OP MUL t t1 t2 t3):remainder) = ["mul " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
transIR ((OP DIV t t1 t2 t3):remainder) = ["div " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
transIR ((MOVE t1 t2):remainder) = ["move " ++ t1 ++ "," ++ t2] ++ (transIR remainder)
transIR ((MOVEI t1 k):remainder) = ["li " ++ t1 ++ "," ++ k] ++ (transIR remainder)
transIR ((PRINT t1):remainder) STACK = ["li $v0,4"] ++ ["la $a0," ++ t1] ++ ["syscall"] ++ (transIR remainder)
transIR ((PRINT t1):remainder) HEAP = ["la $a0," ++ location] ++ ["lw " ++ t1 ++ ",0($a0)"] ++ ["la $a0,strbuf"] ++ [l ++ ":"] ++ ["lb $a1,0(" ++ t1 ++ ")"] ++ ["sb $a1,0($a0)"] ++ ["addi " ++ t1 ++ "," ++ t1 ++ ",1"] ++ ["addi $a0,$a0,1"] ++ ["bne $a1,$0,l"] ++ ["li $v0,4"] ++ ["la $a0,strbuf"] ++ ["syscall"] ++ (transIR remainder)
transIR ((READ t1 t2 l1 l2):remainder) = ["la " ++ t1 ++ "," ++ "strbuf"] ++ ["li $v0,8"] ++ ["move $a0," ++ t1] ++ ["li $a1,256"] ++ ["syscall"] ++ [l1 ++ ":"] ++ ["lb " ++ t2 ++ ",0($a0)"] ++ ["addi $a0,$a0,1"] ++ ["bne " ++ t2 ++ ",$0," ++ l1] ++ ["sub " ++ t2 ++ ",$a0," ++ t1] ++ ["move $a1,$a0"] ++ ["li $v0,9"] ++ ["move $a0," ++ t2] ++ ["syscall"] ++ [l2 ++ ":"] ++ ["lb $a0,0(" ++ t1 ++ ")"] ++ ["sb $a0,0($v0)"] ++ ["addi " ++ t1 ++ "," ++ t1 ++ ",1"] ++ ["addi $v0,$v0,1"] ++ ["bne " ++ t1 ++ ",$a1," ++ l2] ++ ["sub " ++ t1 ++ ",$v0," ++ t2] ++ ["la $a0," ++ location] ++ ["sw " ++ t1 ++ ",0($a0)"] ++ (transIR remainder)

-- FALTA FAZER VERSOES DIFERENTES PARA FLOAT E INT!
-- falta escolhers registos
