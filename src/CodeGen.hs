module CodeGen where

import IR
import SymbolTable
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Control.Monad.RWS (MonadState(get))
import GHC.Exts.Heap (GenClosure(value))


type Temp = String
type Offset = Int
type Count = (Int, Int, [String], Table)
type Table = Map.Map String Location

newStackOffset :: Int -> State Count String
newStackOffset n = do (offset, dataCounter, dataList, table) <- get
                      put (offset + n, dataCounter, dataList, table)
                      return offset

newData :: State Count Int
newData = do (offset, dataCounter, dataList, table) <- get
             put (offset, dataCounter+1, dataList, table)
             return (dataCounter)

addData :: String -> State Count ()
addData str = do (offset, dataCounter, dataList, table) <- get
                 put (offset, dataCounter, dataList ++ str, table)

popStackOffset :: Int -> State Count ()
popStackOffset n = do (offset, dataCounter, dataList, table) <- get
                      put (offset - n, dataCounter, dataList, table)

getTable :: State Count Table
getTable = do (_,_,_,_,table) <- get
              return table


nextLabel :: [Instr] -> String -> Bool
nextLabel (LABEL l1 :_) l2 = l1 == l2
nextLabel _ _ = False

transMips :: [Instr] -> [String] -> State Count [String]
transMips instr strLit fltLit = do fillData strLit fltLit
                                    code2 <- transIR instr
                                    (_,_,dataList,_) <- get
                                    return ([".data"] ++ [dataList] ++ [".text", "main:",code2])


fillData :: [String] -> [Float] -> State Count ()
fillData [] [] = return ()
fillData [] (flt:remainder) = do dataCounter <- newData
                                 addData ["flt" ++ show dataCounter ++ ": .float " ++ flt]
                                 return fillData [] flt
fillData (str:remainder) flt = do dataCounter <- newData
                                  addData ["str" ++ show dataCounter ++ ": .asciiz " ++ "\"" ++ str ++ "\""]
                                  return fillData remainder flt

defaultStringSize :: String
defaultStringSize = 256


getAddress :: String -> String -> State Count String
getAddress str = do (_,_,_,_,table) <- get
                    let Just value =  HashMap.lookup str table
                    

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
                                                                                  where t1' = getAddress t1
                                                                                        t2' = getAddress t2
                                                                                        l1' = getAddress l1
                                                                                        l2' = getAddress l2
transIR ((LABEL l):remainder) = [l ++ ":"] ++ (transIR remainder)
transIR ((JUMP l):remainder) = ["j " ++ l] ++ (transIR remainder)
transIR ((OP opT t1 t2 t3):remainder) = case opT of ADD t    -> case t of "Integer" -> ["add " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
                                                                          "Float"   -> ["add.s " ++ f1 ++ "," ++ f2 ++ "," ++ f3] ++ (transIR remainder)
                                                    SUB t    -> case t of "Integer" -> ["sub " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
                                                                          "Float"   -> ["sub.s " ++ f1 ++ "," ++ f2 ++ "," ++ f3] ++ (transIR remainder)
                                                    MUL t    -> case t of "Integer" -> ["mul " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
                                                                          "Float"   -> ["mul.s " ++ f1 ++ "," ++ f2 ++ "," ++ f3] ++ (transIR remainder)
                                                    DIV t    -> case t of "Integer" -> ["div " ++ t1 ++ "," ++ t2 ++ "," ++ t3] ++ (transIR remainder)
                                                                          "Float"   -> ["add.s " ++ f1 ++ "," ++ f2 ++ "," ++ f3] ++ (transIR remainder)
                                                    POW t    -> case t of "Integer" -> [] ++ (transIR remainder)
                                                                          "Float"   -> [] ++ (transIR remainder)
                                                    CONCAT t -> case t of "Integer" -> [] ++ (transIR remainder)
                                                                          "Float"   -> [] ++ (transIR remainder)
                                                    where t1' = getAddress t1
                                                          t2' = getAddress t2
                                                          l3' = getAddress l3
transIR ((MOVE t t1 t2):remainder) = case t of "Integer" -> ["move " ++ t1' ++ "," ++ t2'] ++ (transIR remainder)
                                               "Float"   -> ["mov.s " ++ t1' ++ "," ++ t2'] ++ (transIR remainder)
                                               where t1' = getAddress t1
                                                     t2' = getAddress t2
transIR ((MOVEI t1 litT):remainder) = case litT of TInt t    -> ["li " ++ t1' ++ "," ++ (show t)] ++ (transIR remainder)
                                                   TFloat t  -> ["li.s " ++ t1' ++ "," ++ (show t)] ++ (transIR remainder)
                                                   TString t -> ["la " ++ t1' ++ "," ++ t] ++ (transIR remainder)
                                                   where t1' = getAddress t1
transIR ((PRINT t1):remainder) STACK = ["li $v0,4"] ++ ["la $a0," ++ t1'] ++ ["syscall"] ++ (transIR remainder)
                                       where t1' = getAddress t1
transIR ((PRINT t1):remainder) HEAP = ["la $a0," ++ location'] ++ ["lw " ++ t1' ++ ",0($a0)"] ++ ["la $a0,strbuf"] ++ [l' ++ ":"] ++ ["lb $a1,0(" ++ t1' ++ ")"] ++ ["sb $a1,0($a0)"] ++ ["addi " ++ t1' ++ "," ++ t1' ++ ",1"] ++ ["addi $a0,$a0,1"] ++ ["bne $a1,$0,l"] ++ ["li $v0,4"] ++ ["la $a0,strbuf"] ++ ["syscall"] ++ (transIR remainder)
                                      where t1'       = getAddress t1
                                            l'        = getAddress l
                                            location' = getAddress location
transIR ((READ t1 t2 l1 l2):remainder) = ["la " ++ t1' ++ "," ++ "strbuf"] ++ ["li $v0,8"] ++ ["move $a0," ++ t1'] ++ ["li $a1,256"] ++ ["syscall"] ++ [l1' ++ ":"] ++ ["lb " ++ t2' ++ ",0($a0)"] ++ ["addi $a0,$a0,1"] ++ ["bne " ++ t2' ++ ",$0," ++ l1'] ++ ["sub " ++ t2' ++ ",$a0," ++ t1'] ++ ["move $a1,$a0"] ++ ["li $v0,9"] ++ ["move $a0," ++ t2'] ++ ["syscall"] ++ [l2' ++ ":"] ++ ["lb $a0,0(" ++ t1' ++ ")"] ++ ["sb $a0,0($v0)"] ++ ["addi " ++ t1' ++ "," ++ t1' ++ ",1"] ++ ["addi $v0,$v0,1"] ++ ["bne " ++ t1' ++ ",$a1," ++ l2'] ++ ["sub " ++ t1' ++ ",$v0," ++ t2'] ++ ["la $a0," ++ location] ++ ["sw " ++ t1' ++ ",0($a0)"] ++ (transIR remainder)
                                         where t1' = getAddress t1
                                               t2' = getAddress t2
                                               l1' = getAddress l1
                                               l2' = getAddress l2
-- falta escolhers registos
-- falta fazer o decl
