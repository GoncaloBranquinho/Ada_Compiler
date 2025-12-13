module CodeGen where
import IR
import SymbolTable
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Control.Monad.RWS (MonadState(get))
import GHC.Exts.Heap (GenClosure(value))
import MemoryAllocator
import Debug.Trace


type Offset = Int
type Counter = (Int, [String], Addresses, ScpInfo, [Int], Content, Int, Int, Map.Map Int [WhileInfo])
type Addresses = Map.Map String [Location]
data ValueInfo = Lit String | Var String
  deriving (Show,Eq)
type Content = Map.Map String [ValueInfo]

newData :: State Counter Int
newData = do (dataCounter, dataList, table, scpInfo, order,content,curr,loopCounter,whileInfo) <- get
             put (dataCounter + 1, dataList, table, scpInfo, order,content,curr,loopCounter,whileInfo)
             return dataCounter

addData :: String -> State Counter ()
addData str = do (dataCounter, dataList, table, scpInfo, order,content,curr,loopCounter,whileInfo) <- get
                 put (dataCounter, dataList ++ [str], table, scpInfo, order,content,curr,loopCounter,whileInfo)

getLoopState :: State Counter Int
getLoopState = do (_,_,_,_,_,_,_,loopCounter,whileInfo) <- get
                  return loopCounter

getTable :: State Counter Addresses
getTable = do (_, _, table, _, _,_,_,_,_) <- get
              return table

nextLabel :: [Instr] -> String -> Bool
nextLabel ((LABEL l1):_) l2 = l1 == l2
nextLabel _ _ = False

transMips :: [Instr] -> [String] -> [Float] -> State Counter [String]
transMips instr strLit fltLit = do fillData strLit fltLit
                                   code2 <- transIR instr
                                   (_, dataList, _, _, _,_,_,_,_) <- get
                                   return ([".data"] ++ strBuf ++ strBufSize ++ stoiOvrFlwMsg ++ powOvrFlwMsg ++ powNegExpMsg ++ floatZero ++ floatOne ++ floatTen  ++ trueString ++ falseString ++ dataList ++ ["\n.text", "    main:"] ++ ["move $fp, $sp"] ++ code2 ++ ["j program_end\n"] ++ concatFun ++ dynamicStrCompareEq ++ staticStrCompareEq ++ readFun ++ putLineFun ++ intToStringFun ++ fltToStringFun ++ strToIntegerFun ++ boolToStringFun ++ powIntFun ++ powFloatFun ++ stoiOverflow ++ powOverflow ++ programEnd)
    where strBuf              = ["string_buffer: .space 1024"]
          strBufSize          = ["string_buffer_size: .half 1024"]
          stoiOvrFlwMsg       = ["stoi_overflow_str: .asciiz \"Erro: overflow na funcao stoi (conversao de string para int)!\\n\"\n"]
          powOvrFlwMsg        = ["pow_overflow_str: .asciiz \"Erro: overflow na funcao exponencial!\\n\""]
          powNegExpMsg        = ["pow_negative_exp_str: .asciiz \"Erro: expoente negativo na funcao exponencial!\\n\""]
          floatZero           = ["float_zero: .float 0.0"]
          floatOne            = ["float_one: .float 1.0"]
          floatTen            = ["float_ten: .float 10.0"]
          trueString          = ["true_string: .asciiz \"TRUE\""]
          falseString         = ["false_string: .asciiz \"FALSE\""]
          concatFun           = ["    concat:\naddi $sp, $sp, -16\nbne $a3, $0, concat_get_values\nli $a0, 64\nli $v0, 9\nsyscall\naddi $v0, $v0, 8\nmove $a1, $v0\nmove $a3, $v0\nj concat_already_initialized\n    concat_get_values:\nlw $a0, -8($a3)\nlw $a1, -4($a3)\n    concat_already_initialized:\nlb $v1, 0($a2)\nsb $v1, 0($a1)\naddi $a2, $a2, 1\naddi $a1, $a1, 1\nsub $v0, $a1, $a3\nbne $v0, $a0, concat_buffer_not_full\nsll $a0, $a0, 1\nsw $v1, 4($sp)\nsw $a2, 8($sp)\nsw $v0, 12($sp)\nli $v0, 9\nsyscall\nmove $a2, $a3\naddi $v0, $v0, 8\n    concat_transfer_buffer:\nlb $v1, 0($a2)\nsb $v1, 0($v0)\naddi $a2, $a2, 1\naddi $v0, $v0, 1\nbne $a2, $a1, concat_transfer_buffer\nlw $v1, 4($sp)\nlw $a2, 8($sp)\nlw $v0, 12($sp)\n    concat_buffer_not_full:\nbne $v1, $0, concat_already_initialized\nsubi $a1, $a1, 1\naddi $sp, $sp, 16\nsw $a0, -8($a3)\nsw $a1, -4($a3)\njr $ra\n"]
          dynamicStrCompareEq = ["    dynamic_str_compare_eq:\nlb $a2, 0($a0)\nlb $a3, 0($a1)\naddi $a0, $a0, 1\naddi $a1, $a1, 1\nbne $a2, $a3, dynamic_str_compare_eq_not_equal\nbne $a2, $0, dynamic_str_compare_eq\nli $v0, 1\nj dynamic_str_compare_eq_end\n    dynamic_str_compare_eq_not_equal:\nli $v0, 0\n    dynamic_str_compare_eq_end:\njr $ra\n"]
          staticStrCompareEq  = ["    static_str_compare_eq:\naddi $sp, $sp, -12\nsw $s0, 4($sp)\nsw $s1, 8($sp)\nlw $a2, 0($a0)\nlw $a3, 0($a1)\naddi $a0, $a0, 4\naddi $a1, $a1, 4\nj static_str_compare_eq_loop\n    static_str_compare_eq_next1:\nlw $a2, 0($a0)\naddi $a0, $a0, 4\naddi $s1, $s1, -1\nbne $a2, $0, static_str_compare_eq_loop\nli $v0, 0\nj static_str_compare_eq_end\n    static_str_compare_eq_next2:\nlw $a3, 0($a1)\naddi $a1, $a1, 4\naddi $s0, $s0, -1\nbne $a3, $0, static_str_compare_eq_loop\nli $v0, 0\nj static_str_compare_eq_end\n    static_str_compare_eq_next3:\nlw $a2, 0($a0)\nlw $a3, 0($a1)\naddi $a0, $a0, 4\naddi $a1, $a1, 4\nseq $v0, $a2, $0\nseq $v1, $a3, $0\nor $v1, $v0, $v1\nli $v0, 0\nbeq $v1, $0, static_str_compare_eq_loop\nbne $a2, $a3, static_str_compare_eq_end\nli $v0, 1\nj static_str_compare_eq_end\n    static_str_compare_eq_loop:\nlb $s0, 0($a2)\nlb $s1, 0($a3)\naddi $a2, $a2, 1\naddi $a3, $a3, 1\nseq $v0, $s0, $0\nseq $v1, $s1, $0\nor $v1, $v0, $v1\nbne $v1, $0, static_str_compare_eq_null\nbeq $s0, $s1, static_str_compare_eq_loop\nli $v0, 0\nj static_str_compare_eq_end\n    static_str_compare_eq_null:\nbeq $s0, $s1, static_str_compare_eq_next3\nbeq $s0, $0, static_str_compare_eq_next1\nj static_str_compare_eq_next2\n    static_str_compare_eq_end:\nlw $s0, 4($sp)\nlw $s1, 8($sp)\naddi $sp, $sp, 12\njr $ra\n"]
          readFun             = ["    read:\naddi $sp, $sp, -24\nsw $a2, 4($sp)\nsw $a3, 8($sp)\nsw $0, 12($sp)\nli $a2, 10\nli $a3, 0\n    read_start:\nla $a0, string_buffer\nlh $a1, string_buffer_size\nli $v0, 8\nsyscall\nmove $a1, $a0\n    read_check_size:\nlb $v1, 0($a0)\naddi $a0, $a0, 1\nbne $v1, $0, read_check_size\nsub $v1, $a0, $a1\nli $v0, 9\nmove $a0, $v1\nsyscall\nsub $v0, $v0, $a3\n    read_move_to_heap:\nlb $a0, 0($a1)\nsb $a0, 0($v0)\naddi $a1, $a1, 1\naddi $v0, $v0, 1\nbne $a0, $0, read_move_to_heap\nlw $a0, 12($sp)\nbne $a0, $0, read_not_first\nsub $a3, $v0, $v1\nsw $a3, 16($sp)\nli $a3, 0\n    read_not_first:\nadd $a0, $a0, $v1\nsubi $a0, $a0, 1\nsw $a0, 12($sp)\nlb $a0, -2($v0)\nsw $a3 20($sp)\naddi $a3, $v1, 3\nsra $a3, $a3, 2\nsll $a3, $a3, 2\naddi $a3, $a3, 1\nsub $a3, $a3, $v1\nlw $v1, 20($sp)\nadd $a3, $a3, $v1\nbne $a0, $a2, read_start\nsb $0, -2($v0)\nlw $a3, 16($sp)\nmove $v0, $a3\nlw $v1, 12($sp)\nlw $a2, 4($sp)\nlw $a3, 8($sp)\nsubi $v1, $v1, 1\nsw $v0, 0($a2)\nsw $v1, 0($a3)\naddi $sp, $sp, 24\njr $ra\n"]
          putLineFun          = ["    put_line:\nla $a0, string_buffer\nli $a1, 10\nsw $a1, 0($a0)\nli $v0, 4\nsyscall\njr $ra\n"]
          intToStringFun      = ["    itos:\nmove $a1, $a0\nmove $v1, $a0\nli $v0, 9\nli $a0, 12\nsyscall\nli $a2, 10\naddi $v0, $v0, 10\nsb $0, 1($v0)\n    itos_convert:\ndiv $a1, $a2\nmflo $a1\nmfhi $a3\naddi $a3, $a3, 48\nsb $a3, 0($v0)\naddi $v0, $v0, -1\nbne $a1, $0, itos_convert\nbge $v1, $0, itos_positive\nli $a0, 45\nsb $a0, 0($v0)\nj itos_negative\n    itos_positive:\naddi $v0, $v0, 1\n    itos_negative:\njr $ra\n"]
          fltToStringFun      = ["    ftos:\naddi $sp, $sp, -40\nsw $ra, 4($sp)\nli $a3, 0\nsw $a3, 8($sp)\nli $v1, 1\nl.s $f13 float_zero\nc.lt.s $f12, $f13\nbc1f ftos_count_integer_size\nli $v1, 0\nneg.s $f12, $f12\n    ftos_count_integer_size:\nl.s $f14, float_ten\nl.s $f1, float_one\naddi $a3, $a3, 1\ndiv.s $f12, $f12, $f14\nc.lt.s $f12, $f1\nbc1f ftos_count_integer_size\n    ftos_restart_integer_extraction:\nmul.s $f12, $f12, $f14\naddi $a3, $a3, -1\ncvt.w.s $f0, $f12\nmfc1 $a1, $f0\nli $a0, 4\nli $v0, 9\nsyscall\naddi $a1, $a1, 48\nsb $a1, 0($v0)\nsb $0, 1($v0)\nsw $a3, 12($sp)\nmove $a2, $v0\nlw $a1, 16($sp)\nlw $a3, 8($sp)\nbne $a3, $0, ftos_positive\nbne $v1, $0, ftos_positive\nli $v1, 45\nlb $a1, 0($a2)\nsb $v1, 0($a2)\nsb $a1, 1($a2)\nsb $0, 2($a2)\n    ftos_positive:\njal concat\nsw $a1, 16($sp)\nsw $a3, 8($sp)\nlw $a3, 12($sp)\ncvt.s.w $f0, $f0\nsub.s $f12, $f12, $f0\nbne $a3, $0, ftos_restart_integer_extraction\nsw $a0, 20($sp)\nmov.s $f13, $f12\nl.s $f0, float_ten\nli $a2, 0\nli $a3, 9\n    ftos_count_leading_zeros:\nmul.s $f13, $f13, $f0\nbeq $a3, $0, ftos_end_leading_zeros\naddi $a2, $a2, 1\naddi $a3, $a3, -1\nc.lt.s $f13, $f1\nbc1t ftos_count_leading_zeros\naddi $a2, $a2, -1\naddi $a3, $a3, 1\n    ftos_end_leading_zeros:\nli $v0, 9\nli $a0, 12\nli $v1, 48\nsyscall\nmove $a0, $a2\nmove $a3, $v0\nli $a1, 0\nbeq $a2, $0, ftos_no_zeros\n    ftos_add_zeros:\nsb $v1, 0($v0)\naddi $v0, $v0, 1\naddi $a2, $a2, -1\naddi $a1, $a1, 1\nbgt $a2, $0, ftos_add_zeros\n    ftos_no_zeros:\nsw $a1, 32($sp)\nsw $a3, 36($sp)\nsb $0, 0($v0)\nlw $a1, 16($sp)\nlw $a3, 8($sp)\nli $v0, 9\nli $a0, 4\nsyscall\nli $a0, 46\nsb $a0, 0($v0)\nsb $0, 1($v0)\nmove $a2, $v0\nlw $a0, 20($sp)\njal concat\nlw $a2, 36($sp)\njal concat\nsw $a0, 20($sp)\nsw $a1, 16($sp)\nsw $a3, 8($sp)\nlw $v1, 32($sp)\nsrl $v1, $v1, 3\nneg $v1, $v1\nadd $v1, $v1, 9\nl.s $f0, float_ten\nl.s $f14, float_one\nbeq $v1, $0, ftos_mult_number_equals_one\n    ftos_get_mult_number:\nmul.s $f14, $f14, $f0\naddi $v1, $v1, -1\nbne $v1, $0, ftos_get_mult_number\n    ftos_mult_number_equals_one:\nmul.s $f12, $f12, $f14\ncvt.w.s $f12, $f12\nmfc1 $a0, $f12\nbeq $a0, $0, ftos_end\njal itos\nmove $a2, $v0\nlw $a0, 20($sp)\nlw $a1, 16($sp)\nlw $a3, 8($sp)\njal concat\nsw $a3, 8($sp)\n    ftos_end:\nlw $a3, 8($sp)\nmove $v0, $a3\nlw $ra, 4($sp)\naddi $sp, $sp, 40\njr $ra\n"]
          strToIntegerFun     = ["    stoi:\naddi $sp, $sp, -4\nli $v0, 1\nsw $v0, 0($sp)\nli $v0, 0\nli $a2, 10\nli $a3, 32\nli $v1, 10\n    stoi_first_phase:\nlb $a1, 0($a0)\naddi $a0, $a0, 1\nbeq $a1, $a2, stoi_first_phase\nbeq $a1, $a3, stoi_first_phase\nli $a2, 45\nbne $a1, $a2, stoi_positive\nsw $0, 0($sp)\nlb $a1, 0($a0)\naddi $a0, $a0, 1\n    stoi_positive:\nli $a2, 48\nli $a3, 57\nbeq $a1, $0, program_end\n    stoi_second_phase:\nbeq $a1, $0, stoi_end\nblt $a1, $a2, stoi_end_second_phase\nbgt $a1, $a3, stoi_end_second_phase\nmult $v0, $v1\nmfhi $v0\nbne $v0, $0, stoi_overflow\nmflo $v0\nblt $v0, $0, stoi_overflow\nlw $a2, 0($sp)\nbne $a2, $0, stoi_positive_second_phase\nli $a2, 48    \naddi $a1, $a1, -48\nsub $v0, $v0, $a1\nlb $a1, 0($a0)\naddi $a0, $a0, 1\nj stoi_second_phase\n    stoi_positive_second_phase:\nli $a2, 48    \naddi $a1, $a1, -48\nadd $v0, $v0, $a1\nlb $a1, 0($a0)\naddi $a0, $a0, 1\nj stoi_second_phase\n    stoi_end_second_phase:\nli $a2, 10\nli $a3, 32\nbeq $v0, $0, program_end\n    stoi_third_phase:\nlb $a1, 0($a0)\naddi $a0, $a0, 1\nbeq $a1, $a2, stoi_third_phase\nbeq $a1, $a3, stoi_third_phase\nbeq $a1, $0, stoi_end\nj program_end\n    stoi_end:\nlw $a0, 0($sp)\naddi $sp, $sp, 4\njr $ra\n"]
          boolToStringFun     = ["    btos:\nbeq $a0, $0, btos_false\nla $v0, true_string\nj btos_true\n    btos_false:\nla $v0, false_string\n    btos_true:\njr $ra\n"]
          powIntFun           = ["    pow_int:\naddi $sp, $sp, -12\nsw $fp, 8($sp)\nmove $fp, $sp\nli $v0, -2\nli $v1, 31\nbne $v0, $a0, pow_int_negative_max\nbne $v1, $a1, pow_int_negative_max\nli $v0, 0x80000000\nj pow_int_end\n    pow_int_negative_max:\nli $v0, 1\nli $v1, 1\nbeq $a1, $0, pow_int_end\nbge $a1, $0, pow_int_non_negative_exp\nli $v0, 4\nla $a0, pow_negative_exp_str\nsyscall\nj program_end\n    pow_int_non_negative_exp:\nbeq $a0, $0, pow_int_end\nand $a3, $a1, $v0\nbge $a0, $0, pow_int_positive\nsub $a0, $0, $a0\naddiu $a3, $a3, 1\n    pow_int_positive:\nsrl $a3, $a3, 1\nbeq $a0, $v0, pow_int_end\n    pow_int_heatup:\nsw $a0, 4($sp)\nsubiu $sp, $sp, 4\nmult $a0, $a0\nsll $v1, $v1, 1\nmfhi $a2\nmflo $a0\nbne $a2, $0, pow_int_cooldown_further\nblt $a0, $0, pow_int_cooldown_further\nblt $v1, $a1, pow_int_heatup\nbne $v1, $a1, pow_int_cooldown_further\n    pow_int_cooldown:\nmult $v0, $a0\nmfhi $a2\nmflo $v0\nbne $a2, $0, pow_overflow\nblt $v0, $0, pow_overflow\nsub $a1, $a1, $v1\nbeq $a1, $0, pow_int_end\n    pow_int_cooldown_further:\nsrl $v1, $v1, 1\naddiu $sp, $sp, 4\nbeq $v1, $0, pow_overflow\nbgt $v1, $a1, pow_int_cooldown_further\nlw $a0, 4($sp)\nj pow_int_cooldown\n    pow_int_end:\nbeq $a3, $0, pow_int_end_positive\nsub $v0, $0, $v0\n    pow_int_end_positive:\nmove $sp, $fp\nlw $fp, 8($sp)\naddi $sp, $sp, 12\njr $ra\n"]
          powFloatFun         = ["    pow_float:\naddi $sp, $sp, -12\nsw $fp, 8($sp)\nmove $fp, $sp\nlwc1 $f0, float_one\nli $v1, 1\nbeq $a1, $0, pow_float_end_positive\nbge $a1, $0, pow_float_non_negative_exp\ndiv.s $f12, $f0, $f12\nsub $a1, $0, $a1\n    pow_float_non_negative_exp:\nlwc1 $f1, float_zero\nc.eq.s $f12, $f1\nmov.s $f0, $f12\nbc1t pow_float_end_positive\nlwc1 $f0, float_one\nli $v0, 1\nand $a3, $a1, $v0\nc.lt.s $f12, $f1\nbc1f pow_float_positive\nneg.s $f12, $f12\naddiu $a3, $a3, 1\n    pow_float_positive:\nsrl $a3, $a3, 1\nc.eq.s $f12, $f0\nbc1t pow_float_end\n    pow_float_heatup:\ns.s $f12, 4($sp)\nsubiu $sp, $sp, 4\nmul.s $f12, $f12, $f12\nsll $v1, $v1, 1\nmfc1 $a2, $f12\nli $v0, 0x7F800000\nbeq $a2, $v0, pow_float_cooldown_further\nli $v0, 0xFF800000\nbeq $a2, $v0, pow_float_cooldown_further\nli $v0, 0xFF000000\nand $a2, $a2, $v0\nbne $a2, $v0, pow_float_valid_1\nmfc1 $v0, $f12\nli $a2, 0x00FFFFFF\nand $v0, $v0, $a2\nbeq $v0, $0, pow_float_valid_1\nj pow_overflow\n    pow_float_valid_1:\nblt $v1, $a1, pow_float_heatup\nbne $v1, $a1, pow_float_cooldown_further\n    pow_float_cooldown:\nmul.s $f0, $f0, $f12\nmfc1 $a2, $f0\nli $v0, 0x7F800000\nbeq $a2, $v0, pow_overflow\nli $v0, 0xFF800000\nbeq $a2, $v0, pow_overflow\nli $v0, 0xFF000000\nand $a2, $a2, $v0\nbne $a2, $v0, pow_float_valid_2\nmfc1 $v0, $f0\nli $a2, 0x00FFFFFF\nand $v0, $v0, $a2\nbeq $v0, $0, pow_float_valid_2\nj pow_overflow\n    pow_float_valid_2:\nsub $a1, $a1, $v1\nbeq $a1, $0, pow_float_end\n    pow_float_cooldown_further:\nsrl $v1, $v1, 1\naddiu $sp, $sp, 4\nbeq $v1, $0, pow_overflow\nbgt $v1, $a1, pow_float_cooldown_further\nl.s $f12, 4($sp)\nj pow_float_cooldown\n    pow_float_end:\nbeq $a3, $0, pow_float_end_positive\nneg.s $f0, $f0\n    pow_float_end_positive:\nmove $sp, $fp\nlw $fp, 8($sp)\naddi $sp, $sp, 12\njr $ra\n"]
          stoiOverflow        = ["    stoi_overflow:\nli $v0, 4\nla $a0, stoi_overflow_str\nsyscall\nj program_end\n"]
          powOverflow         = ["    pow_overflow:\nli $v0, 4\nla $a0, pow_overflow_str\nsyscall\nj program_end\n"]
          programEnd          = ["    program_end:\nli $v0, 10\nsyscall"]


fillData :: [String] -> [Float] -> State Counter ()
fillData [] [] = return ()
fillData [] (flt:remainder) = do dataCounter <- newData
                                 addData ("flt" ++ show dataCounter ++ ": .float " ++ (show flt))
                                 fillData [] remainder
fillData (str:remainder) flt = do dataCounter <- newData
                                  addData ("str" ++ show dataCounter ++ ": .asciiz " ++ "\"" ++ str ++ "\"")
                                  fillData remainder flt

getLocation :: String -> String -> State Counter Location
getLocation str t = do (_, _,table,_,_,_,_,_,_) <- get
                       let Just value = Map.lookup str table
                       if ((length value) == 1 || t /= "Float") then return (head value) else return (last value)

getAddress :: Location -> State Counter String
getAddress loc = case loc of
                          RegI n   -> return n
                          RegF n   -> return n
                          Stack n  -> return (show n)
                          Global n -> return n

changeContent :: String -> [ValueInfo] -> State Counter ()
changeContent id val = do (dataCounter, dataList, table, scpInfo, order, content,curr,loopCounter,whileInfo) <- get
                          let newContent = Map.insert id val content
                          put (dataCounter,dataList,table,scpInfo,order,newContent,curr,loopCounter,whileInfo)

getContent :: String -> State Counter [ValueInfo]
getContent id = do (_,_,_,_,_,content,_,_,_) <- get
                   let val = Map.findWithDefault [] id content
                   return val

free :: State Counter [String]
free = do (dataCounter, dataList, table, scpInfo, order, content,curr,loopCounter,whileInfo) <- get
          let scp = head order
          let (x, y, z) = Map.findWithDefault (0,0,0) scp scpInfo
          let order' = tail order
          put (dataCounter, dataList, table, scpInfo, order', content,curr,loopCounter,whileInfo)
          return (if z == 0 then [] else ["addiu $sp, $sp, " ++ show z])


alloc :: State Counter [String]
alloc = do (dataCounter, dataList, table, scpInfo, order, content, currScp,loopCounter,whileInfo) <- get
           let (x,y,z) = Map.findWithDefault (0,0,0) currScp scpInfo
           put (dataCounter,dataList,table,scpInfo,order,content,currScp+1,loopCounter,whileInfo)
           return (if z == 0 then [] else ["addiu $sp, $sp, " ++ show (-z)])

transIR :: [Instr] -> State Counter [String]
transIR [] = return []
transIR ((COND opT t1 t2 l1 l2):remainder) = do t1'  <- getLocation t1 convertedT
                                                t2'  <- getLocation t2 convertedT
                                                t1'' <- getAddress t1'
                                                t2'' <- getAddress t2'
                                                let (instrAllocate, instrFixT1, instrFixT2) = case (convertedT, t1', t2') of
                                                                                                ("Float", Stack _, Stack _)   -> (["l.s $f12, " ++ t1'' ++ "($fp)"] ++ ["l.s $f13, " ++ t2'' ++ "($fp)"], "$f12", "$f13")
                                                                                                ("Float", Stack _, RegI _)    -> (["l.s $f12, " ++ t1'' ++ "($fp)"], "$f12", t2'')
                                                                                                ("Float", RegI _, Stack _)    -> (["l.s $f13, " ++ t2'' ++ "($fp)"], t1'', "$f13")
                                                                                                ("Float", RegI _, RegI _)     -> ([], t1'', t2'')
                                                                                                ("String", Stack _, Stack _) -> (["lw $a0, " ++ t1'' ++ "($fp)"] ++ ["lw $a1, " ++ t2'' ++ "($fp)"] ++ ["jal static_str_compare_eq"], "$v0", "$0")
                                                                                                ("String", Stack _, RegI _)  -> (["lw $a0, " ++ t1'' ++ "($fp)"] ++ ["move $a1, " ++ t2''] ++ ["jal static_str_compare_eq"], "$v0", "$0")
                                                                                                ("String", RegI _, Stack _)  -> (["move $a0, " ++ t1''] ++ ["lw $a1, " ++ t2'' ++ "($fp)"] ++ ["jal static_str_compare_eq"], "$v0", "$0")
                                                                                                ("String", RegI _, RegI _)   -> (["move $a0, " ++ t1''] ++ ["move $a1, " ++ t2''] ++ ["jal static_str_compare_eq"], "$v0", "$0")
                                                                                                (_, Stack _, Stack _) -> (["lw $a0, " ++ t1'' ++ "($fp)"] ++ ["lw $a1, " ++ t2'' ++ "($fp)"], "$a0", "$a1")
                                                                                                (_, Stack _, RegI _)  -> (["lw $a0, " ++ t1'' ++ "($fp)"], "$a0", t2'')
                                                                                                (_, RegI _, Stack _)  -> (["lw $a1, " ++ t2'' ++ "($fp)"], t1'', "$a1")
                                                                                                (_, RegI _, RegI _)   -> ([], t1'', t2'')
                                                let instrExecute = if nextLabel remainder l2
                                                                   then case (opT, convertedT) of
                                                                                               (IR.EQ _, "Float")   -> ["c.eq.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1t " ++ l1]
                                                                                               (IR.EQ _, "String")  -> ["bne " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1]
                                                                                               (IR.EQ _, _)         -> ["beq " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1]
                                                                                               (IR.NE _, "Float")   -> ["c.eq.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1f " ++ l1]
                                                                                               (IR.NE _, "String")  -> ["beq " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1]
                                                                                               (IR.NE _, _)         -> ["bne " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1]
                                                                                               (IR.LT _, "Integer") -> ["blt " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1]
                                                                                               (IR.LT _, "Float")   -> ["c.lt.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1t " ++ l1]
                                                                                               (IR.LE _, "Integer") -> ["ble " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1]
                                                                                               (IR.LE _, "Float")   -> ["c.le.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1t " ++ l1]
                                                                   else if nextLabel remainder l1
                                                                   then case (opT, convertedT) of
                                                                                               (IR.EQ _, "Float")   -> ["c.eq.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1f " ++ l2]
                                                                                               (IR.EQ _, "String")  -> ["beq " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l2]
                                                                                               (IR.EQ _, _)         -> ["bne " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l2]
                                                                                               (IR.NE _, "Float")   -> ["c.eq.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1t " ++ l2]
                                                                                               (IR.NE _, "String")  -> ["bne " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l2]
                                                                                               (IR.NE _, _)         -> ["beq " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l2]
                                                                                               (IR.LT _, "Integer") -> ["bge " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l2]
                                                                                               (IR.LT _, "Float")   -> ["c.lt.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1f " ++ l2]
                                                                                               (IR.LE _, "Integer") -> ["bgt " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l2]
                                                                                               (IR.LE _, "Float")   -> ["c.le.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1f " ++ l2]
                                                                   else case (opT, convertedT) of
                                                                                               (IR.EQ _, "Float")   -> ["c.eq.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1t " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.EQ _, "String")  -> ["bne " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.EQ _, _) -> ["beq " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.NE _, "Float")   -> ["c.eq.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1f " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.NE _, "String")  -> ["beq " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.NE _, _) -> ["bne " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.LT _, "Integer") -> ["blt " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.LT _, "Float")   -> ["c.lt.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1t " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.LE _, "Integer") -> ["ble " ++ instrFixT1 ++ ", " ++ instrFixT2 ++ ", " ++ l1] ++ ["j " ++ l2]
                                                                                               (IR.LE _, "Float")   -> ["c.le.s " ++ instrFixT1 ++ ", " ++ instrFixT2] ++ ["bc1t " ++ l1] ++ ["j " ++ l2]
                                                code1 <- transIR remainder
                                                return (instrAllocate ++ instrExecute ++ code1)
    where convertedT = (\x -> val x) opT

transIR ((LABEL l):remainder) = do code1 <- transIR remainder
                                   return ([l ++ ":"] ++ code1)
transIR ((JUMP l):remainder) = do code1 <- transIR remainder
                                  return (["j " ++ l] ++ code1)
transIR ((OP opT t1 t2 t3):remainder) = do t1' <- getLocation t1 convertedT
                                           t2' <- getLocation t2 convertedT
                                           t3' <- case opT of POW _ -> do getLocation t3 ((if (convertedT == "Float") then "Integer" else convertedT))
                                                              _     -> do getLocation t3 convertedT
                                           t1'' <- getAddress t1'
                                           t2'' <- getAddress t2'
                                           t3'' <- getAddress t3'
                                           t2''' <- getContent t2
                                           t3''' <- getContent t3
                                           loopCounter <- getLoopState
                                           case opT of CONCAT _ -> (if loopCounter == 0 then (changeContent t1 (t2''' ++ t3''')) else (changeContent t1 [Var t1])); _ -> return ()
                                           let instrExecute = case opT of
                                                                       ADD _  -> case (convertedT, t1', t2', t3') of
                                                                                                                  ("Float", Stack _, Stack _, Stack _)     -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["add.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, Stack _, RegI _)      -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["add.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, RegI _, Stack _)      -> ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["add.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, RegI _, RegI _)       -> ["add.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", RegF _, Stack _, Stack _)      -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["add.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ "$f13"]
                                                                                                                  ("Float", RegF _, Stack _, RegF _)       -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["add.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ t3'']
                                                                                                                  ("Float", RegF _, RegF _, Stack _)       -> ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["add.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$f13"]
                                                                                                                  ("Float", RegF _, RegF _, RegF _)        -> ["add.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                                                                  ("Integer", Stack _, Stack _, Stack _)   -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["add " ++ "$a2" ++ ", " ++ "$a0" ++ ", " ++ "$a1"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, Stack _, RegI _)    -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["add " ++ "$a2" ++ ", " ++ "$a0" ++ ", " ++ t3''] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, RegI _, Stack _)    -> ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["add " ++ "$a2" ++ ", " ++ t2'' ++ ", " ++ "$a1"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, RegI _, RegI _)     -> ["add " ++ "$a2" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", RegI _, Stack _, Stack _)    -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["add " ++ t1'' ++ ", " ++ "$a0" ++ ", " ++ "$a1"]
                                                                                                                  ("Integer", RegI _, Stack _, RegI _)     -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["add " ++ t1'' ++ ", " ++ "$a0" ++ ", " ++ t3'']
                                                                                                                  ("Integer", RegI _, RegI _, Stack _)     -> ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["add " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$a1"]
                                                                                                                  ("Integer", RegI _, RegI _, RegI _)      -> ["add " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                       SUB _  -> case (convertedT, t1', t2', t3') of
                                                                                                                  ("Float", Stack _, Stack _, Stack _)     -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["sub.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, Stack _, RegF _)      -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["sub.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, RegF _, Stack _)      -> ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["sub.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, RegF _, RegF _)       -> ["sub.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", RegF _, Stack _, Stack _)      -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["sub.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ "$f13"]
                                                                                                                  ("Float", RegF _, Stack _, RegF _)       -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["sub.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ t3'']
                                                                                                                  ("Float", RegF _, RegF _, Stack _)       -> ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["sub.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$f13"]
                                                                                                                  ("Float", RegF _, RegF _, RegF _)        -> ["sub.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                                                                  ("Integer", Stack _, Stack _, Stack _)   -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["sub " ++ "$a2" ++ ", " ++ "$a0" ++ ", " ++ "$a1"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, Stack _, RegI _)    -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["sub " ++ "$a2" ++ ", " ++ "$a0" ++ ", " ++ t3''] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, RegI _, Stack _)    -> ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["sub " ++ "$a2" ++ ", " ++ t2'' ++ ", " ++ "$a1"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, RegI _, RegI _)     -> ["sub " ++ "$a2" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", RegI _, Stack _, Stack _)    -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["sub " ++ t1'' ++ ", " ++ "$a0" ++ ", " ++ "$a1"]
                                                                                                                  ("Integer", RegI _, Stack _, RegI _)     -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["sub " ++ t1'' ++ ", " ++ "$a0" ++ ", " ++ t3'']
                                                                                                                  ("Integer", RegI _, RegI _, Stack _)     -> ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["sub " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$a1"]
                                                                                                                  ("Integer", RegI _, RegI _, RegI _)      -> ["sub " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                       MULT _ -> case (convertedT, t1', t2', t3') of
                                                                                                                  ("Float", Stack _, Stack _, Stack _)     -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["mul.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, Stack _, RegF _)      -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["mul.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, RegF _, Stack _)      -> ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["mul.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, RegF _, RegF _)       -> ["mul.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", RegF _, Stack _, Stack _)      -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["mul.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ "$f13"]
                                                                                                                  ("Float", RegF _, Stack _, RegF _)       -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["mul.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ t3'']
                                                                                                                  ("Float", RegF _, RegF _, Stack _)       -> ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["mul.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$f13"]
                                                                                                                  ("Float", RegF _, RegF _, RegF _)        -> ["mul.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                                                                  ("Integer", Stack _, Stack _, Stack _)   -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["mult $a0, $a1"] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, Stack _, RegI _)    -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["mult $a0" ++ ", " ++ t3''] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, RegI _, Stack _)    -> ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["mult "++ t2'' ++ ", " ++ "$a1"] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, RegI _, RegI _)     -> ["mult " ++ t2'' ++ ", " ++ t3''] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", RegI _, Stack _, Stack _)    -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["mult $a0, $a1"] ++ ["mflo " ++ t1'']
                                                                                                                  ("Integer", RegI _, Stack _, RegI _)     -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["mult $a0, " ++ t3''] ++ ["mflo " ++ t1'']
                                                                                                                  ("Integer", RegI _, RegI _, Stack _)     -> ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["mult " ++ t2'' ++ ", " ++ "$a1"] ++ ["mflo " ++ t1'']
                                                                                                                  ("Integer", RegI _, RegI _, RegI _)      -> ["mult " ++ t2'' ++ ", " ++ t3''] ++ ["mflo " ++ t1'']
                                                                       DIV _  -> case (convertedT, t1', t2', t3') of
                                                                                                                  ("Float", Stack _, Stack _, Stack _)     -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["div.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, Stack _, RegF _)      -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["div.s " ++ "$f14" ++ ", " ++ "$f12" ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, RegF _, Stack _)      -> ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["div.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ "$f13"] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, RegF _, RegF _)       -> ["div.s " ++ "$f14" ++ ", " ++ t2'' ++ ", " ++ t3''] ++ ["s.s $f14, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", RegF _, Stack _, Stack _)      -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["div.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ "$f13"]
                                                                                                                  ("Float", RegF _, Stack _, RegF _)       -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["div.s " ++ t1'' ++ ", " ++ "$f12" ++ ", " ++ t3'']
                                                                                                                  ("Float", RegF _, RegF _, Stack _)       -> ["l.s $f13, " ++ t3'' ++ "($fp)"] ++ ["div.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ "$f13"]
                                                                                                                  ("Float", RegF _, RegF _, RegF _)        -> ["div.s " ++ t1'' ++ ", " ++ t2'' ++ ", " ++ t3'']
                                                                                                                  ("Integer", Stack _, Stack _, Stack _)   -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["div $a0, $a1"] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, Stack _, RegI _)    -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["div $a0" ++ ", " ++ t3''] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, RegI _, Stack _)    -> ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["div "++ t2'' ++ ", " ++ "$a1"] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, RegI _, RegI _)     -> ["div " ++ t2'' ++ ", " ++ t3''] ++ ["mflo $a2"] ++ ["sw $a2, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", RegI _, Stack _, Stack _)    -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["div $a0, $a1"] ++ ["mflo " ++ t1'']
                                                                                                                  ("Integer", RegI _, Stack _, RegI _)     -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["div $a0, " ++ t3''] ++ ["mflo " ++ t1'']
                                                                                                                  ("Integer", RegI _, RegI _, Stack _)     -> ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["div " ++ t2'' ++ ", " ++ "$a1"] ++ ["mflo " ++ t1'']
                                                                                                                  ("Integer", RegI _, RegI _, RegI _)      -> ["div " ++ t2'' ++ ", " ++ t3''] ++ ["mflo " ++ t1'']
                                                                       POW t  -> case (convertedT, t2', t3', t1') of
                                                                                                                  ("Float", Stack _, Stack _, Stack _)     -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, Stack _, RegF _)      -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["jal pow_float"] ++ ["mov.s " ++ t1'' ++ ", $f0"]
                                                                                                                  ("Float", Stack _, RegI _, Stack _)      -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["move $a1, " ++ t3''] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", Stack _, RegI _, RegF _)       -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["move $a1, " ++ t3''] ++ ["jal pow_float"] ++ ["mov.s " ++ t1'' ++ ", $f0"]
                                                                                                                  ("Float", RegF _, Stack _, Stack _)      -> ["mov.s $f12, " ++ t2''] ++ ["ls $a1, " ++ t3'' ++ "($fp)"] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", RegF _, Stack _, RegF _)       -> ["mov.s $f12, " ++ t2''] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["jal pow_float"] ++ ["mov.s " ++ t1'' ++ ", $f0"]
                                                                                                                  ("Float", RegF _, RegI _, Stack _)       -> ["mov.s $f12, " ++ t2''] ++ ["move $a1, " ++ t3''] ++ ["jal pow_float"] ++ ["s.s $f0, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Float", RegF _, RegI _, RegF _)        -> ["mov.s $f12, " ++ t2''] ++ ["move $a1, " ++ t3''] ++ ["jal pow_float"] ++ ["mov.s " ++ t1'' ++ ", $f0"]
                                                                                                                  ("Integer", Stack _, Stack _, Stack _)   -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, Stack _, RegI _)    -> ["lw $a0, " ++ t1'' ++ "($sp)"] ++ ["lw $a1, " ++ t2'' ++ "($sp)"] ++ ["jal pow_int"] ++ ["move " ++ t3'' ++ ", $v0"]
                                                                                                                  ("Integer", Stack _, RegI _, Stack _)    -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["move $a1, " ++ t3''] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", Stack _, RegI _, RegI _)     -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["move $a1, " ++ t3''] ++ ["jal pow_int"] ++ ["move " ++ t1'' ++ ", $v0"]
                                                                                                                  ("Integer", RegI _, Stack _, Stack _)    -> ["move $a0, " ++ t2''] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", RegI _, Stack _, RegI _)     -> ["move $a0, " ++ t2''] ++ ["lw $a1, " ++ t3'' ++ "($fp)"] ++ ["jal pow_int"] ++ ["move " ++ t1'' ++ ", $v0"]
                                                                                                                  ("Integer", RegI _, RegI _, Stack _)     -> ["move $a0, " ++ t2''] ++ ["move $a1, " ++ t3''] ++ ["jal pow_int"] ++ ["sw $v0, " ++ t1'' ++ "($fp)"]
                                                                                                                  ("Integer", RegI _, RegI _, RegI _)      -> ["move $a0, " ++ t2''] ++ ["move $a1, " ++ t3''] ++ ["jal pow_int"] ++ ["move " ++ t1'' ++ ", $v0"]

                                                                       CONCAT _ -> if loopCounter == 0
                                                                                   then []
                                                                                   else case (t1', t2', t3') of
                                                                                                             (Stack _, Stack _, Stack _) -> ["lw $a2, " ++ t3'' ++ "($fp)"] ++ ["lw $a3, " ++ t2'' ++ "($fp)"] ++ ["lw $a0, -8($a3)"] ++ ["lw $a1, -4($a3)"] ++ ["jal concat"] ++ ["sw $a3, " ++ t1'' ++ "($fp)"]
                                                                                                             (Stack _, Stack _, RegI _)  -> ["move $a2, " ++ t3''] ++ ["lw $a3, " ++ t2'' ++ "($fp)"] ++ ["lw $a0, -8($a3)"] ++ ["lw $a1, -4($a3)"] ++ ["jal concat"] ++ ["sw $a3, " ++ t1'' ++ "($fp)"]
                                                                                                             (Stack _, RegI _, Stack _)  -> ["lw $a2, " ++ t3'' ++ "($fp)"] ++ ["move $a3, " ++ t2''] ++ ["lw $a0, -8($a3)"] ++ ["lw $a1, -4($a3)"] ++ ["jal concat"] ++ ["sw $a3, " ++ t1'' ++ "($fp)"]
                                                                                                             (Stack _, RegI _, RegI _)   -> ["move $a2, " ++ t3''] ++ ["move $a3, " ++ t2''] ++ ["lw $a0, -8($a3)"] ++ ["lw $a1, -4($a3)"] ++ ["jal concat"] ++ ["sw $a3, " ++ t1'' ++ "($fp)"]
                                                                                                             (RegI _, Stack _, Stack _)  -> ["lw $a2, " ++ t3'' ++ "($fp)"] ++ ["lw $a3, " ++ t2'' ++ "($fp)"] ++ ["lw $a0, -8($a3)"] ++ ["lw $a1, -4($a3)"] ++ ["jal concat"] ++ ["move " ++ t1'' ++ ", $a3"]
                                                                                                             (RegI _, Stack _, RegI _)   -> ["move $a2, " ++ t3''] ++ ["lw $a3, " ++ t2'' ++ "($sp)"] ++ ["lw $a0, -8($a3)"] ++ ["lw $a1, -4($a3)"] ++ ["jal concat"] ++ ["move " ++ t1'' ++ ", $a3"]
                                                                                                             (RegI _, RegI _, Stack _)   -> ["lw $a2, " ++ t3'' ++ "($sp)"] ++ ["move $a3, " ++ t2''] ++ ["lw $a0, -8($a3)"] ++ ["lw $a1, -4($a3)"] ++ ["jal concat"] ++ ["move " ++ t1'' ++ ", $a3"]
                                                                                                             (RegI _, RegI _, RegI _)    -> ["move $a2, " ++ t3''] ++ ["move $a3, " ++ t2''] ++ ["lw $a0, -8($a3)"] ++ ["lw $a1, -4($a3)"] ++ ["jal concat"] ++ ["move " ++ t1'' ++ ", $a3"]
                                                                                                                                                                                                                                                     -- evaluation (fazer isto tambem quando estamos a comparar strings)
                                           instrNext <- transIR remainder
                                           return (instrExecute ++ instrNext)
    where convertedT = (\x -> val x) opT

transIR ((MOVE t t1 t2):remainder) = do t1'   <- getLocation t1 t
                                        t2'   <- getLocation t2 t
                                        t1''  <- getAddress t1'
                                        t2''  <- getAddress t2'
                                        t2''' <- getContent t2
                                        if (t == "String")
                                          then do changeContent t1 t2'''
                                                  transIR remainder
                                          else do let instrExecute = case (t, t1', t2') of
                                                                                        ("Float", Stack _, Stack _) -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["s.s $f12, " ++ t1'' ++ "($fp)"]
                                                                                        ("Float", Stack _, RegF _)  -> ["s.s " ++ t2'' ++ ", " ++ t1'' ++ "($fp)"]
                                                                                        ("Float", RegF _, Stack _)  -> ["l.s " ++ t1'' ++ ", " ++ t2'' ++ "($fp)"]
                                                                                        ("Float", RegF _, RegF _)   -> ["mov.s " ++ t1'' ++ ", " ++ t2'']
                                                                                        (_, Stack _, Stack _)       -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["sw $a0, " ++ t1'' ++ "($fp)"]
                                                                                        (_, Stack _, RegI _)        -> ["sw " ++ t2'' ++ ", " ++ t1'' ++ "($fp)"]
                                                                                        (_, RegI _, Stack _)        -> ["lw " ++ t1'' ++ ", " ++ t2'' ++ "($fp)"]
                                                                                        (_, RegI _, RegI _)         -> ["move " ++ t1'' ++ ", " ++ t2'']
                                                  instrNext <- transIR remainder
                                                  return (instrExecute ++ instrNext)

transIR ((MOVEI t1 litT):remainder) = do t1' <- getLocation t1 convertedT
                                         let extractedT = case litT of TInt t -> (show t); TDouble t -> (show t); TString t -> t;
                                         t2' <- if (convertedT == "Integer") then (return (RegI "")) else (getLocation extractedT convertedT)
                                         t1'' <- getAddress t1'
                                         t2'' <- if (convertedT == "Integer") then (return extractedT) else (getAddress t2')
                                         if convertedT == "String" then (changeContent t1 ([Lit extractedT])) else return ()
                                         let instrExecute = case (litT, t1') of
                                                                             (TInt t, Stack _)    -> ["li $a0, " ++ t2''] ++ ["sw $a0, " ++ t1'' ++ "($fp)"]
                                                                             (TInt t, RegI _)     -> ["li " ++ t1'' ++ ", " ++ t2'']
                                                                             (TDouble t, Stack _) -> ["lwc1 $f12, " ++ t2''] ++ ["s.s $f12, " ++ t1'' ++ "($fp)"]
                                                                             (TDouble t, RegF _)  -> ["lwc1 " ++ t1'' ++ ", " ++ t2'']
                                                                             --(TString t, Stack _) -> ["la $a0, " ++ t2''] ++ ["sw $a0, " ++ t1'' ++ "($sp)"]
                                                                             --(TString t, RegI _)  -> ["la " ++ t1'' ++ ", " ++ t2'']
                                                                             (_,_)                -> []
                                         instrNext <- transIR remainder
                                         return (instrExecute ++ instrNext)
    where convertedT = (\x -> case x of TInt y -> "Integer"; TDouble y -> "Float"; TString y -> "String") litT


transIR (BEGIN:remainder) = do code1 <- alloc
                               code2 <- transIR remainder
                               return (code1 ++ code2)

transIR (END:remainder) = do code1 <- free
                             code2 <- transIR remainder
                             return (code1 ++ code2)


transIR (WHILE:remainder) = do (dataCounter, dataList, table, scpInfo, order, content, currScp,loopCounter,whileInfo) <- get
                               put (dataCounter, dataList, table, scpInfo, order, content, currScp,loopCounter+1,whileInfo)
                               let currWhile = Map.findWithDefault [] loopCounter whileInfo
                               code1 <- mapM (\k -> case k of 
                                                           EVAL x -> do t1 <- getContent x
                                                                        if (t1 /= []) 
                                                                        then do code2  <- concatMultiple t1
                                                                                t1'    <- getLocation x "String"
                                                                                t1''   <- getAddress t1'  
                                                                                changeContent x [Var x]
                                                                                let code3 = case t1' of 
                                                                                                     Stack t1''' -> ["sw $a3, " ++ t1'' ++ "($fp)"]
                                                                                                     RegI t1'''  -> ["move " ++ t1'' ++ ", $a3"]
                                                                                return (["li $a3, 0"] ++ code2 ++ code3)
                                                                        else return []
                                                           MV x y -> do y1 <- getContent y
                                                                        if (y1 /= []) 
                                                                        then do code2 <- concatMultiple y1
                                                                                x1'   <- getLocation x "String"
                                                                                x1''  <- getAddress x1'
                                                                                changeContent x [Var x]
                                                                                let code3 = case (x1') of 
                                                                                                       Stack x1''' -> ["sw $a3, " ++ x1'' ++ "($fp)"] ++ ["li $a3, 0"] ++ ["lw $a2, " ++ x1'' ++ "($fp)"] ++ ["jal concat"] ++ ["sw $a3, " ++ x1'' ++ "($fp)"]
                                                                                                       RegI x1'''  -> ["move " ++ x1'' ++ ", $a3"] ++ ["li $a3, 0"] ++ ["move $a2, " ++ x1''] ++ ["jal concat"] ++ ["move " ++ x1'' ++ ", $a2"]
                                                                                return (["li $a3, 0"] ++ code2 ++ code3)
                                                                        else return []
                                             ) currWhile
                                        >>= \x1 -> return (concat x1)
                                         
                               code4 <- transIR remainder
                               return (code1 ++ code4)
transIR (ENDWHILE:remainder) = do (dataCounter, dataList, table, scpInfo, order, content, currScp,loopCounter,whileInfo) <- get
                                  put (dataCounter, dataList, table, scpInfo, order, content, currScp,loopCounter-1,whileInfo)
                                  transIR remainder

transIR ((DECL t1 t):remainder) = do case t of
                                            "String" -> do t1'  <- getLocation t1 t
                                                           t1'' <- getAddress t1'
                                                           changeContent t1 ([Var t1])
                                            _        -> return ()
                                     transIR remainder


transIR ((READ t1 t2):remainder) = do t1' <- getLocation t1 "String"
                                      t2' <- getLocation t2 "Integer"
                                      t1'' <- getAddress t1'
                                      t2'' <- getAddress t2'
                                      changeContent t1 ([Var t1])
                                      let instrExecute = case (t1', t2') of
                                                                         (Stack _, Stack _) -> ["addi $a2, $fp, " ++ t1''] ++ ["addi $a3, $fp, " ++ t2''] ++ ["jal read"]
                                                                         (Stack _, RegI _)  -> ["subi $a2, $fp, 24"] ++ ["addi $a3, $fp, " ++ t2''] ++ ["jal read"] ++ ["lw " ++ t1'' ++ ", -24($fp)"]
                                                                         (RegI _, Stack _)  -> ["addi $a2, $fp, " ++ t1''] ++ ["subi $a3, $fp, 28"] ++ ["jal read"] ++ ["lw " ++ t2'' ++ ", -28($fp)"]
                                                                         (RegI _, RegI _)   -> ["subi $a2, $fp, 24"] ++ ["subi $a3, $fp, 28"] ++ ["jal read"] ++ ["lw " ++ t1'' ++ ", -24($fp)"] ++ ["lw " ++ t2'' ++ ", -28($fp)"]
                                      instrNext <- transIR remainder
                                      return (instrExecute ++ instrNext)

transIR ((PRINT t1):remainder) = do t1' <- getLocation t1 "String"
                                    t1'' <- getAddress t1'
                                    t1''' <- getContent t1
                                    instrExec <- printMultiple t1'''
                                    instrNext <- transIR remainder
                                    return (instrExec ++ ["jal put_line"] ++ instrNext)

transIR ((IR.TOSTR t t1 t2):remainder) = do t1' <- getLocation t1 "String"
                                            t2' <- getLocation t2 t
                                            t1'' <- getAddress t1'
                                            t2'' <- getAddress t2'
                                            changeContent t1 ([Var t1])
                                            let instrExec = case (t, t1', t2') of
                                                                               ("Float", Stack _, Stack _)   -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["jal ftos"] ++ ["s.s $v0, " ++ t1'' ++ "($fp)"]
                                                                               ("Float", Stack _, RegF _)    -> ["mov.s $f12, " ++ t2''] ++ ["jal ftos"] ++ ["s.s $v0, " ++ t1'' ++ "($fp)"]
                                                                               ("Float", RegI _, Stack _)    -> ["l.s $f12, " ++ t2'' ++ "($fp)"] ++ ["jal ftos"] ++ ["move " ++ t1'' ++ ", $v0"]
                                                                               ("Float", RegI _, RegF _)     -> ["mov.s $f12, " ++ t2''] ++ ["jal ftos"] ++ ["move " ++ t1'' ++ ", $v0"]
                                                                               ("Boolean", Stack _, Stack _)         -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["jal btos"] ++ ["sw $v0, " ++ t1'' ++ "($fp)"]
                                                                               ("Boolean", Stack _, RegI _)          -> ["move $a0, " ++ t2''] ++ ["jal btos"] ++ ["sw $v0, " ++ t1'' ++ "($fp)"]
                                                                               ("Boolean", RegI _, Stack _)          -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["jal btos"] ++ ["move " ++ t1'' ++ ", $v0"]
                                                                               ("Boolean", RegI _, RegI _)           -> ["move $a0, " ++ t2''] ++ ["jal btos"] ++ ["move " ++ t1'' ++ ", $v0"]
                                                                               (_, Stack _, Stack _)         -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["jal itos"] ++ ["sw $v0, " ++ t1'' ++ "($fp)"]
                                                                               (_, Stack _, RegI _)          -> ["move $a0, " ++ t2''] ++ ["jal itos"] ++ ["sw $v0, " ++ t1'' ++ "($fp)"]
                                                                               (_, RegI _, Stack _)          -> ["lw $a0, " ++ t2'' ++ "($fp)"] ++ ["jal itos"] ++ ["move " ++ t1'' ++ ", $v0"]
                                                                               (_, RegI _, RegI _)           -> ["move $a0, " ++ t2''] ++ ["jal itos"] ++ ["move " ++ t1'' ++ ", $v0"]
                                            instrNext <- transIR remainder
                                            return (instrExec ++ instrNext)

concatMultiple :: [ValueInfo] -> State Counter [String]
concatMultiple [] = return []
concatMultiple ((Lit t1):xs) = do t1' <- getLocation t1 "String"
                                  t1'' <- getAddress t1'
                                  instrNext <- concatMultiple xs -- guardar $a3 no t1
                                  return (["la $a2, " ++ t1''] ++ ["jal concat"] ++ instrNext)
concatMultiple ((Var t1):xs) = do t1' <- getLocation t1 "String"
                                  t1'' <- getAddress t1'
                                  instrNext <- concatMultiple xs
                                  let instrExec = case t1' of
                                                       Stack _ -> (["lw $a2, " ++ t1'' ++ "($fp)"] ++ ["jal concat"])
                                                       RegI _  -> (["move $a2, " ++ t1''] ++ ["jal concat"])
                                  return (instrExec ++ instrNext)

printMultiple :: [ValueInfo] -> State Counter [String]
printMultiple [] = return []
printMultiple (Lit t1:xs) = do t1'  <- getLocation t1 "String"
                               t1'' <- getAddress t1'
                               nextStrings <- printMultiple xs
                               return (["li $v0, 4"] ++ ["la $a0, " ++ t1''] ++ ["syscall"] ++ nextStrings)
printMultiple (Var t1:xs) = do x <- getLocation t1 "String"
                               let instr = case x of
                                                  RegI t1'  -> ["li $v0, 4"] ++ ["move $a0, " ++ t1'] ++ ["syscall"]
                                                  Stack t1' -> ["li $v0, 4"] ++ ["lw $a0, " ++ (show t1') ++ "($fp)"] ++ ["syscall"]
                               nextStrings <- printMultiple xs
                               return (instr ++ nextStrings)
