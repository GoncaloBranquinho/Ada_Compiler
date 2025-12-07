module CodeGen where

import IR
import SymbolTable
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Control.Monad.RWS (MonadState(get))
import GHC.Exts.Heap (GenClosure(value))
import MemoryAllocator

type Offset = Int
type Counter = (Int, Int, [String], Addresses, ScpInfo, [Int])
type Addresses = Map.Map String [Location]


newStackOffset :: Int -> State Counter String
newStackOffset n = do (offset, dataCounter, dataList, table, scpInfo, order) <- get
                      put (offset + n, dataCounter, dataList, table, scpInfo, order)
                      return offset


popStackOffset :: Int -> State Counter ()
popStackOffset n = do (offset, dataCounter, dataList, table, scpInfo,order) <- get
                      put (offset - n, dataCounter, dataList, table, scpInfo, order)

newData :: State Counter Int
newData = do (offset, dataCounter, dataList, table, scpInfo, order) <- get
             put (offset, dataCounter + 1, dataList, table, scpInfo, order)
             return (dataCounter)

addData :: String -> State Counter ()
addData str = do (offset, dataCounter, dataList, table, scpInfo, order) <- get
                 put (offset, dataCounter, dataList ++ str, table, scpInfo, order)

getTable :: State Counter Addresses
getTable = do (_, _, _, table, _, _) <- get
              return table


nextLabel :: [Instr] -> String -> Bool
nextLabel ((LABEL l1):_) l2 = l1 == l2
nextLabel _ _ = False

transMips :: [Instr] -> [String] -> State Counter [String]
transMips instr strLit fltLit = do fillData strLit fltLit
                                   code2 <- transIR instr
                                   (_, _, dataList, _, _, _) <- get
                                   return ([".data"] ++ [dataList] ++ [".text", "main:",code2])


fillData :: [String] -> [Float] -> State Counter ()
fillData [] [] = return ()
fillData [] (flt:remainder) = do dataCounter <- newData
                                 addData ["flt" ++ show dataCounter ++ ": .float " ++ flt]
                                 return fillData [] flt
fillData (str:remainder) flt = do dataCounter <- newData
                                  addData ["str" ++ show dataCounter ++ ": .asciiz " ++ "\"" ++ str ++ "\""]
                                  return (fillData remainder flt)

getAddress :: String -> String -> State Counter Location
getAddress str t = do (_, _, _, _, table, _) <- get
                 let (Just value) = Map.lookup str table
                 if (length value) == 1 then return head value else case t of
                                                                         "Integer" -> return head value
                                                                         "Float"   -> return last value

free :: State Counter [String]
free = do (offset, dataCounter, dataList, table, scpInfo, order) <- get
          let scp = head order
          let Just (x, y, z) = Map.lookup scp scpInfo
          let order' = tail head
          put (offset, dataCounter, dataList, table, scpInfo, order')
          return ["addiu $sp, $sp, " ++ show z]

transIR :: [Instr] -> State Counter [String]
transIR [] = return []
transIR ((COND opT t1 t2 l1 l2):remainder) = do t1' <- getAddress t1 convertedT
                                                t2' <- getAddress t2 convertedT
                                                if nextLabel remainder l2 then case opT of
                                                                                             IR.EQ t -> case t of "Integer" -> ["beq " ++ t1' ++ ", " ++ t2' ++ ", " ++ l1] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.eq.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1t " ++ l1] ++ (transIR remainder)
                                                                                             IR.NE t -> case t of "Integer" -> ["bne " ++ t1' ++ ", " ++ t2' ++ ", " ++ l1] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.eq.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1f " ++ l1] ++ (transIR remainder)
                                                                                             IR.LT t -> case t of "Integer" -> ["blt " ++ t1' ++ ", " ++ t2' ++ ", " ++ l1] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.lt.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1t " ++ l1] ++ (transIR remainder)
                                                                                             IR.LE t -> case t of "Integer" -> ["ble " ++ t1' ++ ", " ++ t2' ++ ", " ++ l1] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.le.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1t " ++ l1] ++ (transIR remainder)
                                                else if nextLabel remainder l1 then case opT of
                                                                                             IR.EQ t -> case t of "Integer" -> ["bne " ++ t1' ++ ", " ++ t2' ++ ", " ++ l2] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.eq.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1f " ++ l2] ++ (transIR remainder)
                                                                                             IR.NE t -> case t of "Integer" -> ["beq " ++ t1' ++ ", " ++ t2' ++ ", " ++ l2] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.eq.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1t " ++ l2] ++ (transIR remainder)
                                                                                             IR.LT t -> case t of "Integer" -> ["bge " ++ t1' ++ ", " ++ t2' ++ ", " ++ l2] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.lt.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1f " ++ l2] ++ (transIR remainder)
                                                                                             IR.LE t -> case t of "Integer" -> ["bgt " ++ t1' ++ ", " ++ t2' ++ ", " ++ l2] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.le.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1f " ++ l2] ++ (transIR remainder)
                                                else case opT of
                                                                                             IR.EQ t -> case t of "Integer" -> ["beq " ++ t1' ++ ", " ++ t2' ++ ", " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.eq.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1t " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
                                                                                             IR.NE t -> case t of "Integer" -> ["bne " ++ t1' ++ ", " ++ t2' ++ ", " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.eq.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1f " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
                                                                                             IR.LT t -> case t of "Integer" -> ["blt " ++ t1' ++ ", " ++ t2' ++ ", " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.lt.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1t " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
                                                                                             IR.LE t -> case t of "Integer" -> ["ble " ++ t1' ++ ", " ++ t2' ++ ", " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
                                                                                                                  "Float"   -> ["c.le.s, " ++ t1' ++ ", " ++ t2'] ++ ["bc1t " ++ l1] ++ ["j " ++ l2] ++ (transIR remainder)
    where convertedT = (\x -> val x) opT

transIR ((LABEL l):remainder) = [l ++ ":"] ++ (transIR remainder)
transIR ((JUMP l):remainder) = ["j " ++ l] ++ (transIR remainder)
transIR ((OP opT t1 t2 t3):remainder) = do t1' <- getAddress t1 convertedT
                                           t2' <- getAddress t2 convertedT
                                           t3' <- getAddress t3 convertedT
                                           case opT of 
                                                    ADD t    -> case t of "Integer" -> ["add " ++ t1' ++ ", " ++ t2' ++ ", " ++ t3'] ++ (transIR remainder)
                                                                          "Float"   -> ["add.s " ++ t1' ++ ", " ++ t2' ++ ", " ++ t3'] ++ (transIR remainder)
                                                    SUB t    -> case t of "Integer" -> ["sub " ++ t1' ++ ", " ++ t2' ++ ", " ++ t3'] ++ (transIR remainder)
                                                                          "Float"   -> ["sub.s " ++ t1' ++ ", " ++ t2' ++ ", " ++ t3'] ++ (transIR remainder)
                                                    MULT t   -> case t of "Integer" -> ["mul " ++ t1' ++ ", " ++ t2' ++ ", " ++ t3'] ++ (transIR remainder)
                                                                          "Float"   -> ["mul.s " ++ f1' ++ ", " ++ f2' ++ ", " ++ f3'] ++ (transIR remainder)
                                                    DIV t    -> case t of "Integer" -> ["div " ++ t1' ++ ", " ++ t2' ++ ", " ++ t3'] ++ (transIR remainder)
                                                                          "Float"   -> ["add.s " ++ t1' ++ ", " ++ t2' ++ ", " ++ t3'] ++ (transIR remainder)
                                                    POW t    -> case t of "Integer" -> [] ++ (transIR remainder)
                                                                          "Float"   -> [] ++ (transIR remainder)
                                                    CONCAT t -> case t of "Integer" -> [] ++ (transIR remainder)
                                                                          "Float"   -> [] ++ (transIR remainder)
    where convertedT = (\x -> val x) opT

transIR ((MOVE t t1 t2):remainder) = do t1' <- getAddress t1 t
                                        t2' <- getAddress t2 t
                                        case t of "Integer" -> ["move " ++ t1' ++ ", " ++ t2'] ++ (transIR remainder)
                                                  "Float"   -> ["mov.s " ++ t1' ++ ", " ++ t2'] ++ (transIR remainder)

transIR ((MOVEI t1 (lit t)):remainder) = do t1' <- getAddress t1 convertedT
                                            t2' <- getAddress t2 convertedT
                                            case litT of TInt t    -> ["li " ++ t1' ++ ", " ++ (show t)] ++ (transIR remainder)
                                                         TFloat t  -> ["lwc1 " ++ t1' ++ ", " ++ t2'] ++ (transIR remainder)
                                                         TString t -> ["la " ++ t1' ++ ", " ++ t2'] ++ (transIR remainder)
    where convertedT = let litT = (lit t), ((\x -> case x of TInt y -> "Integer"; TFloat y -> "Float"; TString y -> "String") litT)


transIR (BEGIN:remainder) = transIR remainder
transIR (END:remainder) = do code1 <- free
                             code2 <- transIR remainder
                             return (code1 ++ code2)
transIR ((DECL id t):remainder) = do case t of 
                                         "String" -> transIR remainder -- falta alocar espaço na heap para id, antes de chamar transIR
                                         _        -> transIR remainder













transIR ((PRINT t1):remainder) STACK = do t1' <- getAddress t1 "String"
                                          ["li $v0, 4"] ++ ["la $a0, " ++ t1'] ++ ["syscall"] ++ (transIR remainder)
transIR ((PRINT t1):remainder) HEAP = do t1' <- getAddress t1 "String"
                                         t2' <- getAddress t2 "String"
                                         t3' <- getAddress t3 "String" -- ou "Address"? "HeapAddress"? "Heap"?"
                                         ["la $a0, " ++ t2'] ++ ["lw " ++ t1' ++ ", 0($a0)"] ++ ["la $a0, strbuf"] ++ [l ++ ":"] ++ ["lb $a1, 0(" ++ t1' ++ ")"] ++ ["sb $a1, 0($a0)"] ++ ["addi " ++ t1' ++ ", " ++ t1' ++ ", 1"] ++ ["addi $a0, $a0, 1"] ++ ["bne $a1, $0, l"] ++ ["li $v0, 4"] ++ ["la $a0, strbuf"] ++ ["syscall"] ++ (transIR remainder)

transIR ((READ t1 t2 l1 l2):remainder) = t1' <- getAddress t1 "String"
                                         t2' <- getAddress t2 "Integer"
                                         t3' <- getAddress t3 "String" -- ou "Address"? "HeapAddress"? "Heap"?"
                                         ["la " ++ t1' ++ ", " ++ "strbuf"] ++ ["li $v0, 8"] ++ ["move $a0, " ++ t1'] ++ ["li $a1, 256"] ++ ["syscall"] ++ [l1 ++ ":"] ++ ["lb " ++ t2' ++ ", 0($a0)"] ++ ["addi $a0, $a0, 1"] ++ ["bne " ++ t2' ++ ", $0, " ++ l1] ++ ["sub " ++ t2' ++ ", $a0, " ++ t1'] ++ ["move $a1, $a0"] ++ ["li $v0, 9"] ++ ["move $a0, " ++ t2'] ++ ["syscall"] ++ [l2 ++ ":"] ++ ["lb $a0, 0(" ++ t1' ++ ")"] ++ ["sb $a0, 0($v0)"] ++ ["addi " ++ t1' ++ ", " ++ t1' ++ ", 1"] ++ ["addi $v0, $v0, 1"] ++ ["bne " ++ t1' ++ ", $a1, " ++ l2] ++ ["sub " ++ t1' ++ ", $v0, " ++ t2'] ++ ["la $a0, " ++ t3'] ++ ["sw " ++ t1' ++ ", 0($a0)"] ++ (transIR remainder)
-- falta escolhers registos
-- falta fazer o decl
-- t1' t2' t3' nao sao ainda os registos, sao do tipo Location, e é preciso verificar se esta na stack, porque se for o caso precisamos de meter antes num registro (a ou v)
-- criar um data type para a heap, que nos diz onde esta cada variavel do tipo string
-- meter return onde falta 
