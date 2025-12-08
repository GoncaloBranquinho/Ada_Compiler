module CodeGen where

import IR
import SymbolTable
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Control.Monad.RWS (MonadState(get))
import GHC.Exts.Heap (GenClosure(value))
import MemoryAllocator

type Offset = Int
type Counter = (Int, Int, [String], Addresses, ScpInfo, [Int], Content)
type Addresses = Map.Map String [Location]
type Content = Map.Map String ValueInfo

data ValueInfo = Value | HeapP | DataP | Concat ValueInfo ValueInfo
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
                                   return ([".data"] ++ dataList ++ [".text", "main:"] ++ code2)


fillData :: [String] -> [Float] -> State Counter ()
fillData [] [] = return ()
fillData [] (flt:remainder) = do dataCounter <- newData
                                 addData ("flt" ++ show dataCounter ++ ": .float " ++ (show flt))
                                 fillData [] remainder
fillData (str:remainder) flt = do dataCounter <- newData
                                  addData ("str" ++ show dataCounter ++ ": .asciiz " ++ "\"" ++ str ++ "\"")
                                  fillData remainder flt

getAddress :: String -> String -> State Counter Location
getAddress str t = do (_, _, _,table,_,_,_) <- get
                      let (Just value) = Map.lookup str table
                      if (length value) == 1 then return (head value) else case t of
                                                                             "Integer" -> return (head value)
                                                                             "Float"   -> return (last value)

extractAddress :: Location -> State Counter String
extractAddress loc = case loc of
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
          let Just (x, y, z) = Map.lookup scp scpInfo
          let order' = tail order
          put (offset, dataCounter, dataList, table, scpInfo, order', content)
          return ["addiu $sp, $sp, " ++ show z]

transIR :: [Instr] -> State Counter [String]
transIR [] = return []
transIR ((COND opT t1 t2 l1 l2):remainder) = do t1'  <- getAddress t1 convertedT
                                                t2'  <- getAddress t2 convertedT
                                                t1'' <- extractAddress t1'
                                                t2'' <- extractAddress t2'
                                                let instr = 
                                                      if nextLabel remainder l2 
                                                      then case opT of
                                                          IR.EQ t -> case t of 
                                                            "Integer" -> ["beq " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l1]
                                                            "Float"  -> ["c.eq.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1t " ++ l1]
                                                          IR.NE t -> case t of 
                                                            "Integer" -> ["bne " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l1]
                                                            "Float"  -> ["c.eq.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1f " ++ l1]
                                                          IR.LT t -> case t of 
                                                            "Integer" -> ["blt " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l1]
                                                            "Float"  -> ["c.lt.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1t " ++ l1]
                                                          IR.LE t -> case t of 
                                                            "Integer" -> ["ble " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l1]
                                                            "Float"  -> ["c.le.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1t " ++ l1]
                                                      else if nextLabel remainder l1 
                                                      then case opT of
                                                          IR.EQ t -> case t of 
                                                            "Integer" -> ["bne " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l2]
                                                            "Float"  -> ["c.eq.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1f " ++ l2]
                                                          IR.NE t -> case t of 
                                                            "Integer" -> ["beq " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l2]
                                                            "Float"  -> ["c.eq.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1t " ++ l2]
                                                          IR.LT t -> case t of 
                                                            "Integer" -> ["bge " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l2]
                                                            "Float"  -> ["c.lt.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1f " ++ l2]
                                                          IR.LE t -> case t of 
                                                            "Integer" -> ["bgt " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l2]
                                                            "Float"  -> ["c.le.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1f " ++ l2]
                                                      else case opT of
                                                          IR.EQ t -> case t of 
                                                            "Integer" -> ["beq " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l1] ++ ["j " ++ l2]
                                                            "Float"  -> ["c.eq.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1t " ++ l1] ++ ["j " ++ l2]
                                                          IR.NE t -> case t of 
                                                            "Integer" -> ["bne " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l1] ++ ["j " ++ l2]
                                                            "Float"  -> ["c.eq.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1f " ++ l1] ++ ["j " ++ l2]
                                                          IR.LT t -> case t of 
                                                            "Integer" -> ["blt " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l1] ++ ["j " ++ l2]
                                                            "Float"  -> ["c.lt.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1t " ++ l1] ++ ["j " ++ l2]
                                                          IR.LE t -> case t of 
                                                            "Integer" -> ["ble " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ l1] ++ ["j " ++ l2]
                                                            "Float"  -> ["c.le.s, " ++ t1'' ++ ", " ++ t2''] ++ ["bc1t " ++ l1] ++ ["j " ++ l2]
                                                code1 <- transIR remainder
                                                return (instr ++ code1)
                            where convertedT = (\x -> val x) opT

transIR ((LABEL l):remainder) = do code1 <- transIR remainder
                                   return ([l ++ ":"] ++ code1)
transIR ((JUMP l):remainder) = do code1 <- transIR remainder 
                                  return (["j " ++ l] ++ code1)
transIR ((OP opT t1 t2 t3):remainder) = do t1' <- getAddress t1 convertedT
                                           t2' <- getAddress t2 convertedT
                                           t3' <- getAddress t3 convertedT
                                           t1'' <- extractAddress t1'
                                           t2'' <- extractAddress t2'
                                           t3'' <- extractAddress t3'
                                           instr <-
                                              case opT of
                                                ADD t    -> do changeContent t1'' Value
                                                               case t of 
                                                                  "Integer" -> return ["add " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                  "Float"   -> return ["add.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                SUB t    -> do changeContent t1'' Value
                                                               case t of 
                                                                  "Integer" -> return ["sub " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                  "Float"   -> return ["sub.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                MULT t   -> do changeContent t1'' Value
                                                               case t of 
                                                                  "Integer" -> return ["mul " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                  "Float"   -> return ["mul.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                DIV t    -> do changeContent t1'' Value
                                                               case t of 
                                                                  "Integer" -> return ["div " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                  "Float"   -> return ["add.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                POW t    -> do changeContent t1'' Value
                                                               case t of 
                                                                  "Integer" -> return []
                                                                  "Float"   -> return []
                                                CONCAT   -> do t2''' <- getContent t2''
                                                               t3''' <- getContent t3''
                                                               changeContent t1'' (Concat t2''' t3''')
                                                               return []
                                           code1 <- transIR remainder
                                           return (instr ++ code1)
      where convertedT = (\x -> val x) opT

transIR ((MOVE t t1 t2):remainder) = do t1'   <- getAddress t1 t
                                        t2'   <- getAddress t2 t
                                        t1''  <- extractAddress t1'
                                        t2''  <- extractAddress t2'
                                        t2''' <- getContent t2''
                                        changeContent t1'' t2'''
                                        let instr = case t of
                                                      "Integer" -> ["move " ++ t1'' ++ ", " ++ t2'']
                                                      "Float"   -> ["mov.s " ++ t1'' ++ ", " ++ t2'']
                                        code1 <- transIR remainder
                                        return (instr ++ code1)

transIR ((MOVEI t1 (litT t)):remainder) = do t1' <- getAddress t1 convertedT
                                             t2' <- getAddress (if convertedT == "String" then t else (show t)) convertedT
                                             t1'' <- extractAddress t1'
                                             t2'' <- extractAddress t2'
                                             let instr =
                                                case litT of 
                                                  TInt t    -> do changeContent t1'' Value
                                                                  ["li " ++ t1'' ++ ", " ++ (show t)]
                                                  TFloat t  -> do changeContent t1'' DataP
                                                                  ["lwc1 " ++ t1'' ++ ", " ++ t2'']
                                                  TString t -> do changeContent t1'' DataP
                                                                  ["la " ++ t1'' ++ ", " ++ t2'']
                                             code1 <- transIR remainder
                                             return (instr ++ code1)
    where convertedT = let litT = (lit t) in ((\x -> case x of TInt y -> "Integer"; TFloat y -> "Float"; TString y -> "String") litT)


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
-- falta fazer o decl
-- t1' t2' t3' nao sao ainda os registos, sao do tipo Location, e é preciso verificar se esta na stack, porque se for o caso precisamos de meter antes num registro (a ou v)
-- meter return onde falta
