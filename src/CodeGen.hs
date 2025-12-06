module CodeGen where

import IR
import SymbolTable
import Control.Minad.State
import qualified Data.Map.Strict as Map
import Control.Monad.RWS (MonadState(get))
import GHC.Exts.Heap (GenClosure(value))


data Location = Reg Temp Type
              | Stack OFfset Type
              | Heap Offset
              | Global String

type Temp = String
type Offset = Int
type Count = (Int, Int, Int, [String], Table)
type Table = Map.Map String Location

newStackOffset :: Int -> State Count String
newStackOffset n = do (offset, temp, dataCounter, dataList, table) <- get
                      put (offset + n, temp, dataCounter, dataList, table)
                      return offset

newTemp :: State Count Temp
newTemp = do (offset, temp, dataCounter, dataList, table) <- get
             put (offset, temp + 1, dataCounter, dataList, table)
             return (if (temp < 10) then ("t" ++ (show temp)) else ("s" ++ (show (temp - 10))))

newData :: State Count Int
newData = do (offset, temp, dataCounter, dataList, table) <- get
             put (offset, temp, dataCounter+1, dataList, table)
             return (dataCounter)

addData :: String -> State Count ()
addData str = do (offset, temp, dataCounter, dataList, table) <- get
                 put (offset, temp, dataCounter, dataList ++ str, table)

popStackOffset :: Int -> State Count ()
popStackOffset n = do (offset, temp, dataCounter, dataList, table) <- get
                      put (offset - n, temp, dataCounter, dataList, table)

popTemp :: State Count ()
popTemp = do (offset, temp, dataCounter, dataList, table) <- get
             put (offset, temp - 1, dataCounter, dataList, table)

getTable :: State Count Table
getTable = do (_,_,_,_,table) <- get
              return table

addTable :: String -> Location -> State Count ()
addTable x y = do (offset, temp, dataCounter, dataList, table) <- get
                  put (offset, temp, dataCounter, dataList, HashMap.insert x y table) 

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


getAdress :: String -> String -> State Count String
getAdress str = do (_,_,_,_,table) <- get
                   let Just value =  HashMap.lookup str table of
                     Just value -> eturn value
                     as

transIR :: [Instr] -> State Count [String]
transIR [] = return []
transIR ((COND opT t1 t2 l1 l2):remainder) | nextLabel remainder l2 = case opT of EQ t -> case t of "Integer" -> ["beq " ++ t1' ++ "," ++ t2' ++ "," ++ l1'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.eq.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1t " ++ l1'] ++ (transIR remainder)
                                                                                  NE t -> case t of "Integer" -> ["bne " ++ t1' ++ "," ++ t2' ++ "," ++ l1'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.eq.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1f " ++ l1'] ++ (transIR remainder)
                                                                                  LT t -> case t of "Integer" -> ["blt " ++ t1' ++ "," ++ t2' ++ "," ++ l1'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.lt.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1t " ++ l1'] ++ (transIR remainder)
                                                                                  LE t -> case t of "Integer" -> ["ble " ++ t1' ++ "," ++ t2' ++ "," ++ l1'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.le.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1t " ++ l1'] ++ (transIR remainder)
                                           | nextLabel remainder l1 = case opT of EQ t -> case t of "Integer" -> ["bne " ++ t1' ++ "," ++ t2' ++ "," ++ l2'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.eq.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1f " ++ l2'] ++ (transIR remainder)
                                                                                  NE t -> case t of "Integer" -> ["beq " ++ t1' ++ "," ++ t2' ++ "," ++ l2'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.eq.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1t " ++ l2'] ++ (transIR remainder)
                                                                                  LT t -> case t of "Integer" -> ["bge " ++ t1' ++ "," ++ t2' ++ "," ++ l2'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.lt.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1f " ++ l2'] ++ (transIR remainder)
                                                                                  LE t -> case t of "Integer" -> ["bgt " ++ t1' ++ "," ++ t2' ++ "," ++ l2'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.le.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1f " ++ l2'] ++ (transIR remainder)
                                           | otherwise              = case opT of EQ t -> case t of "Integer" -> ["beq " ++ t1' ++ "," ++ t2' ++ "," ++ l1'] ++ ["j " ++ l2'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.eq.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1t " ++ t1'] ++ ["j " ++ t2'] ++ (transIR remainder)
                                                                                  NE t -> case t of "Integer" -> ["bne " ++ t1' ++ "," ++ t2' ++ "," ++ l1'] ++ ["j " ++ l2'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.eq.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1f " ++ l1'] ++ ["j " ++ l2'] ++ (transIR remainder)
                                                                                  LT t -> case t of "Integer" -> ["blt " ++ t1' ++ "," ++ t2' ++ "," ++ l1'] ++ ["j " ++ l2'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.lt.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1t " ++ l1'] ++ ["j " ++ l2'] ++ (transIR remainder)
                                                                                  LE t -> case t of "Integer" -> ["ble " ++ t1' ++ "," ++ t2' ++ "," ++ l1'] ++ ["j " ++ l2'] ++ (transIR remainder)
                                                                                                    "Float"   -> ["c.le.s, " ++ t1' ++ "," ++ t2'] ++ ["bc1t " ++ l1'] ++ ["j " ++ l2'] ++ (transIR remainder)
                                                                                  where t1' = getAdress t1
                                                                                  t2' = getAdress t2
                                                                                  l1' = getAdress l1
                                                                                  l2' = getAdress l2
transIR ((LABEL l):remainder) = [l ++ ":"] ++ (transIR remainder)
transIR ((JUMP l):remainder) = ["j " ++ l] ++ (transIR remainder)
transIR ((OP opT t1 t2 t3):remainder) = case opT of ADD t -> case t of "Integer" -> ["add " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
                                                                       "Float"   -> ["add.s " ++ f1 ++ "," ++ f2 ++ "," ++ f3] ++ (transIR remainder)
                                                    SUB t -> case t of "Integer" -> ["sub " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
                                                                       "Float"   -> ["sub.s " ++ f1 ++ "," ++ f2 ++ "," ++ f3] ++ (transIR remainder)
                                                    MUL t -> case t of "Integer" -> ["mul " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
                                                                       "Float"   -> ["mul.s " ++ f1 ++ "," ++ f2 ++ "," ++ f3] ++ (transIR remainder)
                                                    DIV t -> case t of "Integer" -> ["div " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
                                                                       "Float"   -> ["add.s " ++ f1 ++ "," ++ f2 ++ "," ++ f3] ++ (transIR remainder)
                                                    where t1' = getAdress t1
                                                          t2' = getAdress t2
                                                          l3' = getAdress l3
transIR ((MOVE t1 t2):remainder) = ["move " ++ t1 ++ "," ++ t2] ++ (transIR remainder)
transIR ((MOVEI t1 k):remainder) = ["li " ++ t1 ++ "," ++ k] ++ (transIR remainder)
transIR ((PRINT t1):remainder) STACK = ["li $v0,4"] ++ ["la $a0," ++ t1] ++ ["syscall"] ++ (transIR remainder)
transIR ((PRINT t1):remainder) HEAP = ["la $a0," ++ location] ++ ["lw " ++ t1 ++ ",0($a0)"] ++ ["la $a0,strbuf"] ++ [l ++ ":"] ++ ["lb $a1,0(" ++ t1 ++ ")"] ++ ["sb $a1,0($a0)"] ++ ["addi " ++ t1 ++ "," ++ t1 ++ ",1"] ++ ["addi $a0,$a0,1"] ++ ["bne $a1,$0,l"] ++ ["li $v0,4"] ++ ["la $a0,strbuf"] ++ ["syscall"] ++ (transIR remainder)
transIR ((READ t1 t2 l1 l2):remainder) = ["la " ++ t1 ++ "," ++ "strbuf"] ++ ["li $v0,8"] ++ ["move $a0," ++ t1] ++ ["li $a1,256"] ++ ["syscall"] ++ [l1 ++ ":"] ++ ["lb " ++ t2 ++ ",0($a0)"] ++ ["addi $a0,$a0,1"] ++ ["bne " ++ t2 ++ ",$0," ++ l1] ++ ["sub " ++ t2 ++ ",$a0," ++ t1] ++ ["move $a1,$a0"] ++ ["li $v0,9"] ++ ["move $a0," ++ t2] ++ ["syscall"] ++ [l2 ++ ":"] ++ ["lb $a0,0(" ++ t1 ++ ")"] ++ ["sb $a0,0($v0)"] ++ ["addi " ++ t1 ++ "," ++ t1 ++ ",1"] ++ ["addi $v0,$v0,1"] ++ ["bne " ++ t1 ++ ",$a1," ++ l2] ++ ["sub " ++ t1 ++ ",$v0," ++ t2] ++ ["la $a0," ++ location] ++ ["sw " ++ t1 ++ ",0($a0)"] ++ (transIR remainder)

-- FALTA FAZER VERSOES DIFERENTES PARA FLOAT E INT!
-- falta escolhers registos
-- falta fazer o decl
