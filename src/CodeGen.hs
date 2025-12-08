module CodeGen where

import IR
import SymbolTable
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Control.Monad.RWS (MonadState(get))
import GHC.Exts.Heap (GenClosure(value))
import MemoryAllocator
import Distribution.Simple.Utils (xargs)

type Offset = Int
type Counter = (Int, Int, [String], Addresses, ScpInfo, [Int], Content)
type Addresses = Map.Map String [Location]
type Content = Map.Map String ValueInfo

data ValueInfo = Value | HeapP Location | DataP Location | Concat [ValueInfo]
        deriving (Show,Eq)



newStackOffset :: Int -> State Counter Int
newStackOffset n = do (offset, dataCounter, dataList, table, scpInfo, order,content) <- get
                      put (offset + n, dataCounter, dataList, table, scpInfo, order,content)
                      return offset


popStackOffset :: Int -> State Counter ()
popStackOffset n = do (offset, dataCounter, dataList, table, scpInfo,order,content) <- get
                      put (offset - n, dataCounter, dataList, table, scpInfo, order,content)

newData :: State Counter Int
newData = do (offset, dataCounter, dataList, table, scpInfo, order,content) <- get
             put (offset, dataCounter + 1, dataList, table, scpInfo, order,content)
             return (dataCounter)

addData :: String -> State Counter ()
addData str = do (offset, dataCounter, dataList, table, scpInfo, order,content) <- get
                 put (offset, dataCounter, dataList ++ [str], table, scpInfo, order,content)

getTable :: State Counter Addresses
getTable = do (_, _, _, table, _, _,_) <- get
              return table


nextLabel :: [Instr] -> String -> Bool
nextLabel ((LABEL l1):_) l2 = l1 == l2
nextLabel _ _ = False

transMips :: [Instr] -> [String] -> [Float] -> State Counter [String]
transMips instr strLit fltLit = do fillData strLit fltLit
                                   code2 <- transIR instr
                                   (_, _, dataList, _, _, _,_) <- get
                                   return ([".data"] ++ strBuf ++ strBufSize ++ powOvrFlwMsg ++ powNegExpMsg ++ floatOne ++ floatZero ++ dataList ++ [".text", "main:"] ++ code2 ++ readFun ++ printFun ++ putLineFun ++ powIntFun ++ powFloatFun ++ powOverflow ++ exitProgram)
    where strBuf       = ["string_buffer: .space 1024"]
          strBufSize   = ["string_buffer_size: .half 1024"]
          powOvrFlwMsg = ["pow_overflow_str: .asciiz \"Erro: overflow na funcao exponencial!\n\""]
          powNegExpMsg = ["pow_negative_exp_str: .asciiz \"Erro: expoente negativo na funcao exponencial!\n\""]
          floatOne     = ["float_one: .float 1.0"]
          floatZero    = ["float_zero: .float 0.0"]
          readFun      = ["read:\nsw $a2, -4($sp)\nsw $a3, -8($sp)\nlw $0, -12($sp)\nli $a2, 10\nli $a3, 0\nread_start:\nla $a0, string_buffer\nlh $a1, string_buffer_size\nli $v0, 8\nsyscall\nmove $a1, $a0\nread_check_size:\nlb $v1, 0($a0)\naddi $a0, $a0, 1\nbne $v1, $0, read_check_size\nsub $v1, $a0, $a1\nli $v0, 9\nmove $a0, $v1\nsyscall\nsub $v0, $v0, $a3\nread_move_to_heap:\nlb $a0, 0($a1)\nsb $a0, 0($v0)\naddi $a1, $a1, 1\naddi $v0, $v0, 1\nbne $a0, $0, read_move_to_heap\nlw $a0, -12($sp)\nbne $a0, $0, read_not_first\nsub $a3, $v0, $v1\nsw $a3, -16($sp)\nli $a3, 0\nread_not_first:\nadd $a0, $a0, $v1\nsubi $a0, $a0, 1\nsw $a0, -12($sp)\nlb $a0, -2($v0)\nsw $a3 -20($sp)\naddi $a3, $v1, 3\nsra $a3, $a3, 2\nsll $a3, $a3, 2\naddi $a3, $a3, 1\nsub $a3, $a3, $v1\nlw $v1, -20($sp)\nadd $a3, $a3, $v1\nbne $a0, $a2, read_start\nsb $0, -2($v0)\nlw $a3, -16($sp)\nmove $v0, $a3\nlw $v1, -12($sp)\nlw $a2, -4($sp)\nlw $a3, -8($sp)\nsubi $v1, $v1, 1\nsw $v0, 0($a2)\nsw $v1, 0($a3)\njr $ra"]
          printFun     = ["print:\nla $v1, string_buffer\nmove $a0, $v1\nlh $v0, string_buffer_size\nsubi $v0, $v0, 1\nprint_move_to_buffer:\nbne $v0, $0, print_not_out_of_bounds\nsb $0, 0($v1)\nli $v0, 4\nsyscall\nlh $v0, string_buffer_size\nsubi $v0, $v0, 1\nsub $v1, $v1, $v0\nprint_not_out_of_bounds:\nlb $a3, 0($a2)\nsb $a3, 0($v1)\naddi $a2, $a2, 1\naddi $v1, $v1, 1\nsubi $v0, $v0, 1\nbne $a3, $0, print_move_to_buffer\nli $v0, 4\nsyscall\njr $ra"]
          putLineFun   = ["put_line:\nla $a0, string_buffer\nli $a1, 10\nsw $a1, 0($a0)\nli $v0, 4\nsyscall\njr $ra"]
          powIntFun    = ["pow_int:\nmove $fp, $sp\nli $v0, -2\nli $v1, 31\nbne $v0, $a0, pow_int_negative_max\nbne $v1, $a1, pow_int_negative_max\nli $v0, 0x80000000\nj pow_int_end\npow_int_negative_max:\nli $v0, 1\nli $v1, 1\nbeq $a1, $0, pow_int_end\nbge $a1, $0, pow_int_non_negative_exp\nli $v0, 4\nla $a0, pow_negative_exp_str\nsyscall\nj program_end\npow_int_non_negative_exp:\nbeq $a0, $0, pow_int_end\nand $a3, $a1, $v0\nbge $a0, $0, pow_int_positive\nsub $a0, $0, $a0\naddiu $a3, $a3, 1\npow_int_positive:\nsrl $a3, $a3, 1\nbeq $a0, $v0, pow_int_end\npow_int_heatup:\nsw $a0, -4($sp)\nsubiu $sp, $sp, 4\nmult $a0, $a0\nsll $v1, $v1, 1\nmfhi $a2\nmflo $a0\nbne $a2, $0, pow_int_cooldown_further\nblt $a0, $0, pow_int_cooldown_further\nblt $v1, $a1, pow_int_heatup\nbne $v1, $a1, pow_int_cooldown_further\npow_int_cooldown:\nmult $v0, $a0\nmfhi $a2\nmflo $v0\nbne $a2, $0, pow_overflow\nblt $v0, $0, pow_overflow\nsub $a1, $a1, $v1\nbeq $a1, $0, pow_int_end\npow_int_cooldown_further:\nsrl $v1, $v1, 1\naddiu $sp, $sp, 4\nbeq $v1, $0, pow_overflow\nbgt $v1, $a1, pow_int_cooldown_further\nlw $a0, -4($sp)\nj pow_int_cooldown\npow_int_end:\nbeq $a3, $0, pow_int_end_positive\nsub $v0, $0, $v0\npow_int_end_positive:\nmove $sp, $fp\njr $ra"]
          powFloatFun  = ["pow_float:\nmove $fp, $sp\nlwc1 $f0, float_one\nli $v1, 1\nbeq $a1, $0, pow_float_end\nbge $a1, $0, pow_float_non_negative_exp\ndiv.s $f12, $f0, $f12\nsub $a1, $0, $a1\npow_float_non_negative_exp:\nlwc1 $f1, float_zero\nc.eq.s $f12, $f1\nbc1t pow_float_end\nli $v0, 1\nand $a3, $a1, $v0\nc.lt.s $f12, $f1\nbc1f pow_float_positive\nneg.s $f12, $f12\naddiu $a3, $a3, 1\npow_float_positive:\nsrl $a3, $a3, 1\nc.eq.s $f12, $f0\nbc1t pow_float_end\npow_float_heatup:\ns.s $f12, -4($sp)\nsubiu $sp, $sp, 4\nmul.s $f12, $f12, $f12\nsll $v1, $v1, 1\nmfc1 $a2, $f12\nli $v0, 0x7F800000\nbeq $a2, $v0, pow_float_cooldown_further\nli $v0, 0xFF800000\nbeq $a2, $v0, pow_float_cooldown_further\nli $v0, 0xFF000000\nand $a2, $a2, $v0\nbne $a2, $v0, pow_float_valid_1\nmfc1 $v0, $f12\nli $a2, 0x00FFFFFF\nand $v0, $v0, $a2\nbeq $v0, $0, pow_float_valid_1\nj pow_overflow\npow_float_valid_1:\nblt $v1, $a1, pow_float_heatup\nbne $v1, $a1, pow_float_cooldown_further\npow_float_cooldown:\nmul.s $f0, $f0, $f12\nmfc1 $a2, $f0\nli $v0, 0x7F800000\nbeq $a2, $v0, pow_overflow\nli $v0, 0xFF800000\nbeq $a2, $v0, pow_overflow\nli $v0, 0xFF000000\nand $a2, $a2, $v0\nbne $a2, $v0, pow_float_valid_2\nmfc1 $v0, $f0\nli $a2, 0x00FFFFFF\nand $v0, $v0, $a2\nbeq $v0, $0, pow_float_valid_2\nj pow_overflow\npow_float_valid_2:\nsub $a1, $a1, $v1\nbeq $a1, $0, pow_float_end\npow_float_cooldown_further:\nsrl $v1, $v1, 1\naddiu $sp, $sp, 4\nbeq $v1, $0, pow_overflow\nbgt $v1, $a1, pow_float_cooldown_further\nl.s $f12, -4($sp)\nj pow_float_cooldown\npow_float_end:\nbeq $a3, $0, pow_float_end_positive\nneg.s $f0, $f0\npow_float_end_positive:\nmove $sp, $fp\njr $ra"]
          powOverflow  = ["pow_overflow:\nli $v0, 4\nla $a0, pow_overflow_str\nsyscall\nj program_end"]
          exitProgram  = ["program_end:\nli $v0, 10\nsyscall"]

fillData :: [String] -> [Float] -> State Counter ()
fillData [] [] = return ()
fillData [] (flt:remainder) = do dataCounter <- newData
                                 addData ("flt" ++ show dataCounter ++ ": .float " ++ (show flt))
                                 fillData [] remainder
fillData (str:remainder) flt = do dataCounter <- newData
                                  addData ("str" ++ show dataCounter ++ ": .asciiz " ++ "\"" ++ str ++ "\"")
                                  fillData remainder flt

getLocation :: String -> String -> State Counter Location
getLocation str t = do (_, _, _,table,_,_,_) <- get
                       let (Just value) = Map.lookup str table
                       if (length value) == 1 
                       then return (head value)
                       else case t of
                                   "Integer" -> return (head value)
                                   "Float"   -> return (last value)

getAddress :: Location -> State Counter String
getAddress loc = case loc of
                          RegI n   -> return n
                          RegF n   -> return n
                          Stack n  -> return (show n)
                          Global n -> return n

changeContent :: String -> ValueInfo -> State Counter ()
changeContent id val = do (offset, dataCounter, dataList, table, scpInfo, order, content) <- get
                          let newContent = Map.insert id val content
                          put (offset,dataCounter,dataList,table,scpInfo,order,content)

getContent :: String -> State Counter ValueInfo
getContent id = do (_,_,_,_,_,_,content) <- get
                   return (content Map.! id)


free :: State Counter [String]
free = do (offset, dataCounter, dataList, table, scpInfo, order, content) <- get
          let scp = head order
          let (x, y, z) = Map.findWithDefault (0,0,0) scp scpInfo
          let order' = tail order
          put (offset, dataCounter, dataList, table, scpInfo, order', content)
          return (if z == 0 then [] else ["addiu $sp, $sp, " ++ show z])

transIR :: [Instr] -> State Counter [String]
transIR [] = return []
transIR ((COND opT t1 t2 l1 l2):remainder) = do t1'  <- getLocation t1 convertedT
                                                t2'  <- getLocation t2 convertedT
                                                t1'' <- getAddress t1'
                                                t2'' <- getAddress t2'
                                                let instrAllocate = case (convertedT, t1', t2') of
                                                                                                ("Integer", Stack _, Stack _) -> ["lw $a0, " ++ t1'' ++ "($sp)"] ++ ["lw $a1, " ++ t2'' ++ "($sp)"]
                                                                                                ("Integer", Stack _, RegI _)  -> ["lw $a0, " ++ t1'' ++ "($sp)"] ++ ["move $a1, " ++ t2'']
                                                                                                ("Integer", RegI _, Stack _)  -> ["move $a0, " ++ t1''] ++ ["lw $a1, " ++ t2'' ++ "($sp)"]
                                                                                                ("Integer", RegI _, RegI _)   -> ["move $a0, " ++ t1''] ++ ["move $a1, " ++ t2'']
                                                                                                ("Float", Stack _, Stack _)   -> ["l.s $f12, " ++ t1'' ++ "($sp)"] ++ ["l.s $f13, " ++ t2'' ++ "($sp)"]
                                                                                                ("Float", Stack _, RegI _)    -> ["l.s $f12, " ++ t1'' ++ "($sp)"] ++ ["mov.s $f13, " ++ t2'']
                                                                                                ("Float", RegI _, Stack _)    -> ["mov.s $f12, " ++ t1''] ++ ["l.s $f13, " ++ t2'' ++ "($sp)"]
                                                                                                ("Float", RegI _, RegI _)     -> ["mov.s $f12, " ++ t1''] ++ ["mov.s $f13, " ++ t2'']
                                                let instrExecute = if nextLabel remainder l2
                                                                   then case (opT, convertedT) of
                                                                                               (IR.EQ _, "Integer") -> ["beq $a0, $a1, " ++ l1]
                                                                                               (IR.EQ _, "Float")   -> ["c.eq.s $f12, $f13"] ++ ["bc1t " ++ l1]
                                                                                               (IR.NE _, "Integer") -> ["bne $a0, $a1, " ++ l1]
                                                                                               (IR.NE _, "Float")   -> ["c.eq.s $f12, $f13"] ++ ["bc1f " ++ l1]
                                                                                               (IR.LT _, "Integer") -> ["blt $a0, $a1, " ++ l1]
                                                                                               (IR.LT _, "Float")   -> ["c.lt.s $f12, $f13"] ++ ["bc1t " ++ l1]
                                                                                               (IR.LE _, "Integer") -> ["ble $a0, $a1, " ++ l1]
                                                                                               (IR.LE _, "Float")   -> ["c.le.s $f12, $f13"] ++ ["bc1t " ++ l1]
                                                                   else if nextLabel remainder l1
                                                                   then case (opT, convertedT) of
                                                                                               (IR.EQ _, "Integer") -> ["bne $a0, $a1, " ++ l2]
                                                                                               (IR.EQ _, "Float")   -> ["c.eq.s $f12, $f13"] ++ ["bc1f " ++ l2]
                                                                                               (IR.NE _, "Integer") -> ["beq $a0, $a1, " ++ l2]
                                                                                               (IR.NE _, "Float")   -> ["c.eq.s $f12, $f13"] ++ ["bc1t " ++ l2]
                                                                                               (IR.LT _, "Integer") -> ["bge $a0, $a1, " ++ l2]
                                                                                               (IR.LT _, "Float")   -> ["c.lt.s $f12, $f13"] ++ ["bc1f " ++ l2]
                                                                                               (IR.LE _, "Integer") -> ["bgt $a0, $a1, " ++ l2]
                                                                                               (IR.LE _, "Float")   -> ["c.le.s $f12, $f13"] ++ ["bc1f " ++ l2]
                                                                   else case (opT, convertedT) of
                                                                                               (IR.EQ _, "Integer") -> ["beq $a0, $a1, " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.EQ _, "Float")   -> ["c.eq.s $f12, $f13"] ++ ["bc1t " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.NE _, "Integer") -> ["bne $a0, $a1, " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.NE _, "Float")   -> ["c.eq.s $f12, $f13"] ++ ["bc1f " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.LT _, "Integer") -> ["blt $a0, $a1, " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.LT _, "Float")   -> ["c.lt.s $f12, $f13"] ++ ["bc1t " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.LE _, "Integer") -> ["ble $a0, $a1, " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.LE _, "Float")   -> ["c.le.s $f12, $f13"] ++ ["bc1t " ++ l1] ++ ["j " ++ l2]
                                                code1 <- transIR remainder
                                                return (instrAllocate ++ instrExecute ++ code1)
    where convertedT = (\x -> val x) opT

transIR ((LABEL l):remainder) = do code1 <- transIR remainder
                                   return ([l ++ ":"] ++ code1)
transIR ((JUMP l):remainder) = do code1 <- transIR remainder
                                  return (["j " ++ l] ++ code1)
transIR ((OP opT t1 t2 t3):remainder) = do t1' <- getLocation t1 convertedT
                                           t2' <- getLocation t2 convertedT
                                           t3' <- getLocation t3 convertedT
                                           t1'' <- getAddress t1'
                                           t2'' <- getAddress t2'
                                           t3'' <- getAddress t3'
                                           case opT of
                                                    CONCAT -> do t2'''  <- getContent t2''
                                                                 t3'''  <- getContent t3''
                                                                 t2'''' <- case t2''' of
                                                                                      Concat xs -> return xs
                                                                                      _         -> return [t2''']
                                                                 t3'''' <- case t3''' of
                                                                                      Concat xs -> return xs
                                                                                      _         -> return [t2''']
                                                                 changeContent t1'' (Concat t2'''' t3'''')
                                                    _      -> do changeContent t1'' Value
                                           let instrExecute = case opT of
                                                                       ADD _  -> case (convertedT, t1', t2', t3') of
                                                                                                                  ("Integer", Stack _, Stack _, Stack _) -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["add.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, Stack _, RegI _)  -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["add.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, RegI _, Stack _)  -> ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["add.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, RegI _, RegI _)   -> ["add.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", RegI _, Stack _, Stack _)  -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["add.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ "$f13"]
                                                                                                                  ("Integer", RegI _, Stack _, RegI _)   -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["add.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ t3'']
                                                                                                                  ("Integer", RegI _, RegI _, Stack _)   -> ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["add.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$f13"]
                                                                                                                  ("Integer", RegI _, RegI _, RegI _)    -> ["add.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                                                                  ("Float", Stack _, Stack _, Stack _)   -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["add " ++ "$a2" ++ ", " ++ "$a0" ++ ", " ++ "$a1"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, Stack _, RegI _)    -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["add " ++ "$a2" ++ ", " ++ "$a0" ++ ", " ++ t3''] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, RegI _, Stack _)    -> ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["add " ++ "$a2" ++ ", " ++ t2'' ++ ", " ++ "$a1"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, RegI _, RegI _)     -> ["add " ++ "$a2" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", RegI _, Stack _, Stack _)    -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["add " ++ t1'' ++ ", " ++ "$a0" ++ ", " ++ "$a1"]
                                                                                                                  ("Float", RegI _, Stack _, RegI _)     -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["add " ++ t1'' ++ ", " ++ "$a0" ++ ", " ++ t3'']
                                                                                                                  ("Float", RegI _, RegI _, Stack _)     -> ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["add " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$a1"]
                                                                                                                  ("Float", RegI _, RegI _, RegI _)      -> ["add " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                       SUB _  -> case (convertedT, t1', t2', t3') of
                                                                                                                  ("Integer", Stack _, Stack _, Stack _) -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["sub.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, Stack _, RegI _)  -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["sub.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, RegI _, Stack _)  -> ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["sub.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, RegI _, RegI _)   -> ["sub.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", RegI _, Stack _, Stack _)  -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["sub.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ "$f13"]
                                                                                                                  ("Integer", RegI _, Stack _, RegI _)   -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["sub.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ t3'']
                                                                                                                  ("Integer", RegI _, RegI _, Stack _)   -> ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["sub.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$f13"]
                                                                                                                  ("Integer", RegI _, RegI _, RegI _)    -> ["sub.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                                                                  ("Float", Stack _, Stack _, Stack _)   -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["sub " ++ "$a2" ++ ", " ++ "$a0" ++ ", " ++ "$a1"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, Stack _, RegI _)    -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["sub " ++ "$a2" ++ ", " ++ "$a0" ++ ", " ++ t3''] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, RegI _, Stack _)    -> ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["sub " ++ "$a2" ++ ", " ++ t2'' ++ ", " ++ "$a1"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, RegI _, RegI _)     -> ["sub " ++ "$a2" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", RegI _, Stack _, Stack _)    -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["sub " ++ t1'' ++ ", " ++ "$a0" ++ ", " ++ "$a1"]
                                                                                                                  ("Float", RegI _, Stack _, RegI _)     -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["sub " ++ t1'' ++ ", " ++ "$a0" ++ ", " ++ t3'']
                                                                                                                  ("Float", RegI _, RegI _, Stack _)     -> ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["sub " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$a1"]
                                                                                                                  ("Float", RegI _, RegI _, RegI _)      -> ["sub " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                       MULT _ -> case (convertedT, t1', t2', t3') of
                                                                                                                  ("Integer", Stack _, Stack _, Stack _) -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["mul.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, Stack _, RegI _)  -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["mul.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, RegI _, Stack _)  -> ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["mul.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, RegI _, RegI _)   -> ["mul.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", RegI _, Stack _, Stack _)  -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["mul.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ "$f13"]
                                                                                                                  ("Integer", RegI _, Stack _, RegI _)   -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["mul.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ t3'']
                                                                                                                  ("Integer", RegI _, RegI _, Stack _)   -> ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["mul.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$f13"]
                                                                                                                  ("Integer", RegI _, RegI _, RegI _)    -> ["mul.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                                                                  ("Float", Stack _, Stack _, Stack _)   -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["mult $a0, $a1"] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, Stack _, RegI _)    -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["mult $a0" ++ ", " ++ t3''] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, RegI _, Stack _)    -> ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["mult "++ t2'' ++ ", " ++ "$a1"] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, RegI _, RegI _)     -> ["mult " ++ t2'' ++ ", " ++ t3''] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", RegI _, Stack _, Stack _)    -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["mult $a0, $a1"] ++ ["mflo " ++ t1'']
                                                                                                                  ("Float", RegI _, Stack _, RegI _)     -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["mult $a0, " ++ t3''] ++ ["mflo " ++ t1'']
                                                                                                                  ("Float", RegI _, RegI _, Stack _)     -> ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["mult " ++ t2'' ++ ", " ++ "$a1"] ++ ["mflo " ++ t1'']
                                                                                                                  ("Float", RegI _, RegI _, RegI _)      -> ["mult " ++ t2'' ++ ", " ++ t3''] ++ ["mflo " ++ t1'']
                                                                       DIV _  -> case (convertedT, t1', t2', t3') of
                                                                                                                  ("Integer", Stack _, Stack _, Stack _) -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["div.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, Stack _, RegI _)  -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["div.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, RegI _, Stack _)  -> ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["div.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, RegI _, RegI _)   -> ["div.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Integer", RegI _, Stack _, Stack _)  -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["div.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ "$f13"]
                                                                                                                  ("Integer", RegI _, Stack _, RegI _)   -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["div.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ t3'']
                                                                                                                  ("Integer", RegI _, RegI _, Stack _)   -> ["l.s $f13, " ++ t3'' ++ "($sp)"] ++ ["div.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$f13"]
                                                                                                                  ("Integer", RegI _, RegI _, RegI _)    -> ["div.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                                                                  ("Float", Stack _, Stack _, Stack _)   -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["div $a0, $a1"] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, Stack _, RegI _)    -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["div $a0" ++ ", " ++ t3''] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, RegI _, Stack _)    -> ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["div "++ t2'' ++ ", " ++ "$a1"] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, RegI _, RegI _)     -> ["div " ++ t2'' ++ ", " ++ t3''] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($sp)"]
                                                                                                                  ("Float", RegI _, Stack _, Stack _)    -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["div $a0, $a1"] ++ ["mflo " ++ t1'']
                                                                                                                  ("Float", RegI _, Stack _, RegI _)     -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["div $a0, " ++ t3''] ++ ["mflo " ++ t1'']
                                                                                                                  ("Float", RegI _, RegI _, Stack _)     -> ["lw $a1, " ++ t3'' ++ "($sp)"] ++ ["div " ++ t2'' ++ ", " ++ "$a1"] ++ ["mflo " ++ t1'']
                                                                                                                  ("Float", RegI _, RegI _, RegI _)      -> ["div " ++ t2'' ++ ", " ++ t3''] ++ ["mflo " ++ t1'']
                                                                       POW t  -> case (convertedT, t1', t2', t3') of
                                                                                                                  ("Integer", Stack _, Stack _, Stack _) -> ["l.s $f12, " ++ t1'' ++ "($sp)"] ++ ["l.s $f13, " ++ t2'' ++ "($sp)"] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t3'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, Stack _, RegI _)  -> ["l.s $f12, " ++ t1'' ++ "($sp)"] ++ ["l.s $f13, " ++ t2'' ++ "($sp)"] ++ ["jal pow_float"] ++ ["mov.s " ++ t3'' ++ ", $f0"]
                                                                                                                  ("Integer", Stack _, RegI _, Stack _)  -> ["l.s $f12, " ++ t1'' ++ "($sp)"] ++ ["mov.s $f13, " ++ t2''] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t3'' ++ "($sp)"]
                                                                                                                  ("Integer", Stack _, RegI _, RegI _)   -> ["l.s $f12, " ++ t1'' ++ "($sp)"] ++ ["mov.s $f13, " ++ t2''] ++ ["jal pow_float"] ++ ["mov.s " ++ t3'' ++ ", $f0"]
                                                                                                                  ("Integer", RegI _, Stack _, Stack _)  -> ["mov.s $f12, " ++ t1''] ++ ["l.s $f13, " ++ t2'' ++ "($sp)"] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t3'' ++ "($sp)"]
                                                                                                                  ("Integer", RegI _, Stack _, RegI _)   -> ["mov.s $f12, " ++ t1''] ++ ["l.s $f13, " ++ t2'' ++ "($sp)"] ++ ["jal pow_float"] ++ ["mov.s " ++ t3'' ++ ", $f0"]
                                                                                                                  ("Integer", RegI _, RegI _, Stack _)   -> ["mov.s $f12, " ++ t1''] ++ ["mov.s $f13, " ++ t2''] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t3'' ++ "($sp)"]
                                                                                                                  ("Integer", RegI _, RegI _, RegI _)    -> ["mov.s $f12, " ++ t1''] ++ ["mov.s $f13, " ++ t2''] ++ ["jal pow_float"] ++ ["mov.s " ++ t3'' ++ ", $f0"]
                                                                                                                  ("Float", Stack _, Stack _, Stack _)   -> ["lw $a0, " ++ t1'' ++ "($sp)"] ++ ["lw $a1, " ++ t2'' ++ "($sp)"] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t3'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, Stack _, RegI _)    -> ["l.s $f12, " ++ t1'' ++ "($sp)"] ++ ["l.s $f13, " ++ t2'' ++ "($sp)"] ++ ["jal pow_float"] ++ ["mov.s " ++ t3'' ++ ", $f0"]
                                                                                                                  ("Float", Stack _, RegI _, Stack _)    -> ["lw $a0, " ++ t1'' ++ "($sp)"] ++ ["move $a1, " ++ t2''] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t3'' ++ "($sp)"]
                                                                                                                  ("Float", Stack _, RegI _, RegI _)     -> ["lw $a0, " ++ t1'' ++ "($sp)"] ++ ["move $a1, " ++ t2''] ++ ["jal pow_int"] ++ ["move " ++ t3'' ++ ", $v0"]
                                                                                                                  ("Float", RegI _, Stack _, Stack _)    -> ["move $a0, " ++ t1''] ++ ["lw $a1, " ++ t2'' ++ "($sp)"] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t3'' ++ "($sp)"]
                                                                                                                  ("Float", RegI _, Stack _, RegI _)     -> ["move $a0, " ++ t1''] ++ ["lw $a1, " ++ t2'' ++ "($sp)"] ++ ["jal pow_int"] ++ ["move " ++ t3'' ++ ", $v0"]
                                                                                                                  ("Float", RegI _, RegI _, Stack _)     -> ["move $a0, " ++ t1''] ++ ["move $a1, " ++ t2''] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t3'' ++ "($sp)"]
                                                                                                                  ("Float", RegI _, RegI _, RegI _)      -> ["move $a0, " ++ t1''] ++ ["move $a1, " ++ t2''] ++ ["jal pow_int"] ++ ["move " ++ t3'' ++ ", $v0"]
                                                                       CONCAT -> []
                                           instrNext <- transIR remainder
                                           return (instrExecute ++ instrNext)
    where convertedT = (\x -> val x) opT

transIR ((MOVE t t1 t2):remainder) = do t1'   <- getLocation t1 t
                                        t2'   <- getLocation t2 t
                                        t1''  <- getAddress t1'
                                        t2''  <- getAddress t2'
                                        t2''' <- getContent t2''
                                        changeContent t1'' t2'''
                                        let instrExecute = case (t, t1', t2') of
                                                           ("Float", Stack _, Stack _) -> ["l.s $f12, " ++ t2'' ++ "($sp)"] ++ ["s.s $f12, " ++ t1'' ++ "($sp)"]
                                                           ("Float", Stack _, RegF _)  -> ["s.s " ++ t2'' ++ ", " ++ t1'' ++ "($sp)"]
                                                           ("Float", RegF _, Stack _)  -> ["l.s " ++ t1'' ++ ", " ++ t2'' ++ "($sp)"]
                                                           ("Float", RegF _, RegF _)   -> ["mov.s " ++ t1'' ++ ", " ++ t2'']
                                                           (_, Stack _, Stack _)       -> ["lw $a0, " ++ t2'' ++ "($sp)"] ++ ["sw $a0, " ++ t1'' ++ "($sp)"]
                                                           (_, Stack _, RegF _)        -> ["sw " ++ t2'' ++ ", " ++ t1'' ++ "($sp)"]
                                                           (_, RegF _, Stack _)        -> ["lw " ++ t1'' ++ ", " ++ t2'' ++ "($sp)"]
                                                           (_, RegF _, RegF _)           -> ["move " ++ t1'' ++ ", " ++ t2'']
                                        instrNext <- transIR remainder
                                        return (instrExecute ++ instrNext)

transIR ((MOVEI t1 litT):remainder) = do t1' <- getLocation t1 convertedT
                                         let extractedT = case litT of TInt t -> (show t); TDouble t -> (show t); TString t -> t;
                                         t2' <- if (convertedT == "Integer") then (return (RegI "")) else (getLocation extractedT convertedT)
                                         t1'' <- getAddress t1'
                                         t2'' <- if (convertedT == "Integer") then (return extractedT) else (getAddress t2')
                                         let typeOfValue = if convertedT == "Integer" then Value else (if convertedT == "Float" then (DataP t2') else (Concat [DataP t2']))
                                         changeContent t1'' typeOfValue
                                         let instrExecute = case (litT, t1') of 
                                                                             (TInt t, Stack _)    -> ["li $a0, " ++ t2''] ++ ["sw $a0, " ++ t1'' ++ "($sp)"]
                                                                             (TInt t, RegI _)     -> ["li " ++ t1'' ++ ", " ++ t2'']
                                                                             (TDouble t, Stack _) -> ["lwc1 $f12, " ++ t2''] ++ ["s.s $f12, " ++ t1'' ++ "($sp)"]
                                                                             (TDouble t, RegF _)  -> ["lwc1 " ++ t1'' ++ ", " ++ t2'']
                                                                             (TString t, Stack _) -> ["la $a0, " ++ t2''] ++ ["sw $a0, " ++ t1'' ++ "($sp)"]
                                                                             (TString t, RegI _)  -> ["la " ++ t1'' ++ ", " ++ t2'']
                                         instrNext <- transIR remainder
                                         return (instrExecute ++ instrNext)
    where convertedT = (\x -> case x of TInt y -> "Integer"; TDouble y -> "Float"; TString y -> "String") litT


transIR (END:remainder) = do code1 <- free
                             code2 <- transIR remainder
                             return (code1 ++ code2)

transIR ((READ t1 t2 l1 l2):remainder) = do t1' <- getLocation t1 "String"
                                            t2' <- getLocation t2 "Integer"
                                            t1'' <- getAddress t1'
                                            t2'' <- getAddress t2'
                                            changeContent t1'' (Concat [HeapP t1'])
                                            changeContent t2'' Value t2'
                                            let instrExecute = case (t1', t2') of
                                                                               (Stack _, Stack _) -> ["addi $a2, $sp, " ++ t1] ++ ["addi $a3, $sp, " ++ t2] ++ ["jal read"]
                                                                               (Stack _, RegI _)  -> ["subi $a2, $sp, 24"] ++ ["addi $a3, $sp, " ++ t2] ++ ["jal read"] ++ ["lw " ++ t1 ++ ", -24($sp)"]
                                                                               (RegI _, Stack _)  -> ["addi $a2, $sp, " ++ t1] ++ ["subi $a3, $sp, 28"] ++ ["jal read"] ++ ["lw " ++ t2 ++ ", -28($sp)"]
                                                                               (RegI _, RegI _)   -> ["subi $a2, $sp, 24"] ++ ["subi $a3, $sp, 28"] ++ ["jal read"] ++ ["lw " ++ t1 ++ ", -24($sp)"] ++ ["lw " ++ t2 ++ ", -28($sp)"]
                                            instrNext <- transIR remainder
                                            return (instrExecute ++ instrNext)

--transIR ((PRINT t1):remainder) HEAP = do t1' <- getLocation t1 "String"
  --                                       t1'' <- getAddress t1'


{-
transIR ((PRINT t1):remainder) STACK = do t1' <- getLocation t1 "String"
                                          ["li $v0, 4"] ++ ["la $a0, " ++ t1'] ++ ["syscall"] ++ (transIR remainder)
transIR ((PRINT t1):remainder) HEAP = do t1' <- getLocation t1 "String"
                                         t2' <- getLocation t2 "String"
                                         t3' <- getLocation t3 "String" -- ou "Address"? "HeapAddress"? "Heap"?"
                                         ["la $a0, " ++ t2'] ++ ["lw " ++ t1' ++ ", 0($a0)"] ++ ["la $a0, strbuf"] ++ [l ++ ":"] ++ ["lb $a1, 0(" ++ t1' ++ ")"] ++ ["sb $a1, 0($a0)"] ++ ["addi " ++ t1' ++ ", " ++ t1' ++ ", 1"] ++ ["addi $a0, $a0, 1"] ++ ["bne $a1, $0, l"] ++ ["li $v0, 4"] ++ ["la $a0, strbuf"] ++ ["syscall"] ++ (transIR remainder)





-- falta fazer o decl
-- t1' t2' t3' nao sao ainda os registos, sao do tipo Location, e é preciso verificar se esta na stack, porque se for o caso precisamos de meter antes num registro (a ou v)
-- meter return onde falta





PRINT:

extra: ["li $a1, 0"]; ["li $a1, 0"]

stack t1: ["lw $a0, " ++ t1 ++ "($sp)"] ++ ["jal print"]

registro t1: ["move $a0, " ++ t1] ++ ["jal print"]




READ:

stack t1, stack t2: ["addi $a2, $sp, " ++ t1] ++ ["addi $a3, $sp, " ++ t2] ++ ["jal read"]

registro t1, stack t2: ["subi $a2, $sp, 24"] ++ ["addi $a3, $sp, " ++ t2] ++ ["jal read"] ++ ["lw " ++ t1 ++ ", -24($sp)"]

stack t1, registro t2: ["addi $a2, $sp, " ++ t1] ++ ["subi $a3, $sp, 28"] ++ ["jal read"] ++ ["lw " ++ t2 ++ ", -28($sp)"]

registro t1, registro t2: ["subi $a2, $sp, 24"] ++ ["subi $a3, $sp, 28"] ++ ["jal read"] ++ ["lw " ++ t1 ++ ", -24($sp)"] ++ ["lw " ++ t2 ++ ", -28($sp)"]




POW_FLOAT:

stack t1, stack t2, stack t3: ["l.s $f12, " ++ t1 ++ "($sp)"] ++ ["l.s $f13, " ++ t2 ++ "($sp)"] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t3 ++ "($sp)"]

registro t1, stack t2, stack t3: ["mov.s $f12, " ++ t1] ++ ["l.s $f13, " ++ t2 ++ "($sp)"] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t3 ++ "($sp)"]

stack t1, registro t2, stack t3: ["l.s $f12, " ++ t1 ++ "($sp)"] ++ ["mov.s $f13, " ++ t2] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t3 ++ "($sp)"]

registro t1, registro t2, stack t3: ["mov.s $f12, " ++ t1] ++ ["mov.s $f13, " ++ t2] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t3 ++ "($sp)"]

stack t1, stack t2, registro t3: ["l.s $f12, " ++ t1 ++ "($sp)"] ++ ["l.s $f13, " ++ t2 ++ "($sp)"] ++ ["jal pow_float"] ++ ["mov.s " ++ t3 ++ ", $f0"]

registro t1, stack t2, registro t3: ["mov.s $f12, " ++ t1] ++ ["l.s $f13, " ++ t2 ++ "($sp)"] ++ ["jal pow_float"] ++ ["mov.s " ++ t3 ++ ", $f0"]

stack t1, registro t2, registro t3: ["l.s $f12, " ++ t1 ++ "($sp)"] ++ ["mov.s $f13, " ++ t2] ++ ["jal pow_float"] ++ ["mov.s " ++ t3 ++ ", $f0"]

registro t1, registro t2, registro t3: ["mov.s $f12, " ++ t1] ++ ["mov.s $f13, " ++ t2] ++ ["jal pow_float"] ++ ["mov.s " ++ t3 ++ ", $f0"]




POW_INT

stack t1, stack t2, stack t3: ["lw $a0, " ++ t1 ++ "($sp)"] ++ ["lw $a1, " ++ t2 ++ "($sp)"] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t3 ++ "($sp)"]

registro t1, stack t2, stack t3: ["move $a0, " ++ t1] ++ ["lw $a1, " ++ t2 ++ "($sp)"] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t3 ++ "($sp)"]

stack t1, registro t2, stack t3: ["lw $a0, " ++ t1 ++ "($sp)"] ++ ["move $a1, " ++ t2] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t3 ++ "($sp)"]

registro t1, registro t2, stack t3: ["move $a0, " ++ t1] ++ ["move $a1, " ++ t2] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t3 ++ "($sp)"]

stack t1, stack t2, registro t3: ["lw $a0, " ++ t1 ++ "($sp)"] ++ ["lw $a1, " ++ t2 ++ "($sp)"] ++ ["jal pow_int"] ++ ["move " ++ t3 ++ ", $v0"]

registro t1, stack t2, registro t3: ["move $a0, " ++ t1] ++ ["lw $a1, " ++ t2 ++ "($sp)"] ++ ["jal pow_int"] ++ ["move " ++ t3 ++ ", $v0"]

stack t1, registro t2, registro t3: ["lw $a0, " ++ t1 ++ "($sp)"] ++ ["move $a1, " ++ t2] ++ ["jal pow_int"] ++ ["move " ++ t3 ++ ", $v0"]

registro t1, registro t2, registro t3: ["move $a0, " ++ t1] ++ ["move $a1, " ++ t2] ++ ["jal pow_int"] ++ ["move " ++ t3 ++ ", $v0"]

-}
