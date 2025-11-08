{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,303) ([0,0,4,0,0,0,1024,0,0,0,2048,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,4096,0,0,8,0,0,0,0,0,4096,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,40,0,0,0,0,0,0,960,0,0,0,0,0,0,16,0,0,0,0,0,0,0,4096,0,0,16432,2,4096,0,0,16,0,0,0,16432,2,4096,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,512,0,0,0,280,30914,0,0,6144,49665,120,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,280,30914,0,0,6144,49665,120,0,0,4,0,0,0,49152,0,0,0,0,32,0,0,0,0,0,0,0,0,65024,192,0,0,0,0,6,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6144,49665,120,0,0,280,30914,0,0,6144,512,120,0,0,24,30722,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,4096,0,0,6144,49665,120,0,0,0,0,0,0,0,4096,0,0,8,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,1024,0,0,0,0,32,0,12288,576,0,16,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,280,30914,0,0,6144,49665,120,0,0,280,30914,0,0,6144,49665,120,0,0,280,30914,0,0,6144,49665,120,0,0,280,30914,0,0,6144,49665,120,0,0,280,30914,0,0,6144,49665,120,0,0,280,30914,0,0,6144,49665,120,0,0,280,30914,0,0,6144,49665,120,0,0,280,30914,0,12288,576,0,16,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,8192,0,0,0,0,32,0,0,0,0,0,0,0,0,0,1536,0,0,0,0,6,0,0,0,1536,0,0,0,49280,0,0,0,32768,192,0,0,0,49280,0,0,0,32768,192,0,0,0,49280,0,0,0,32768,192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,280,30914,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,16432,2,4096,0,0,1024,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Prog","DeclCompStart","DeclComp","Decl","DeclNonInit","DeclInit","DeclVar","Type","ExecCompStart","ExecComp","Exec","IfThenElse","Assign","WhileLoop","IO","Exp","OrExp","AndExp","RelExp","AddExp","MultExp","PowExp","UnaryExp","Factor","main","put_line","get_line","integer","boolean","float","string","procedure","begin","end","is","if","then","else","while","loop","true","false","and","or","xor","not","\"=\"","\"/=\"","\"<\"","\"<=\"","\">\"","\">=\"","\"&\"","\":=\"","\"(\"","\")\"","\":\"","\";\"","\",\"","\"+\"","\"-\"","\"**\"","\"*\"","\"/\"","string_lit","id","integer_lit","float_lit","%eof"]
        bit_start = st Prelude.* 72
        bit_end = (st Prelude.+ 1) Prelude.* 72
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..71]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (35) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (35) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (28) = happyShift action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (72) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (38) = happyShift action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (69) = happyShift action_12
action_5 (5) = happyGoto action_6
action_5 (6) = happyGoto action_7
action_5 (7) = happyGoto action_8
action_5 (8) = happyGoto action_9
action_5 (9) = happyGoto action_10
action_5 (10) = happyGoto action_11
action_5 _ = happyReduce_3

action_6 (36) = happyShift action_17
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (69) = happyShift action_12
action_7 (7) = happyGoto action_16
action_7 (8) = happyGoto action_9
action_7 (9) = happyGoto action_10
action_7 (10) = happyGoto action_11
action_7 _ = happyReduce_2

action_8 (61) = happyShift action_15
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_6

action_10 _ = happyReduce_7

action_11 (60) = happyShift action_13
action_11 (62) = happyShift action_14
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_10

action_13 (31) = happyShift action_33
action_13 (32) = happyShift action_34
action_13 (33) = happyShift action_35
action_13 (34) = happyShift action_36
action_13 (11) = happyGoto action_32
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (69) = happyShift action_31
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_4

action_16 (61) = happyShift action_30
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (29) = happyShift action_25
action_17 (30) = happyShift action_26
action_17 (39) = happyShift action_27
action_17 (42) = happyShift action_28
action_17 (69) = happyShift action_29
action_17 (12) = happyGoto action_18
action_17 (13) = happyGoto action_19
action_17 (14) = happyGoto action_20
action_17 (15) = happyGoto action_21
action_17 (16) = happyGoto action_22
action_17 (17) = happyGoto action_23
action_17 (18) = happyGoto action_24
action_17 _ = happyReduce_17

action_18 (37) = happyShift action_63
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (29) = happyShift action_25
action_19 (30) = happyShift action_26
action_19 (39) = happyShift action_27
action_19 (42) = happyShift action_28
action_19 (69) = happyShift action_29
action_19 (14) = happyGoto action_62
action_19 (15) = happyGoto action_21
action_19 (16) = happyGoto action_22
action_19 (17) = happyGoto action_23
action_19 (18) = happyGoto action_24
action_19 _ = happyReduce_16

action_20 (61) = happyShift action_61
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_20

action_22 _ = happyReduce_21

action_23 _ = happyReduce_22

action_24 _ = happyReduce_23

action_25 (58) = happyShift action_60
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (58) = happyShift action_59
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (44) = happyShift action_48
action_27 (45) = happyShift action_49
action_27 (49) = happyShift action_50
action_27 (58) = happyShift action_51
action_27 (63) = happyShift action_52
action_27 (64) = happyShift action_53
action_27 (68) = happyShift action_54
action_27 (69) = happyShift action_55
action_27 (70) = happyShift action_56
action_27 (71) = happyShift action_57
action_27 (19) = happyGoto action_58
action_27 (20) = happyGoto action_40
action_27 (21) = happyGoto action_41
action_27 (22) = happyGoto action_42
action_27 (23) = happyGoto action_43
action_27 (24) = happyGoto action_44
action_27 (25) = happyGoto action_45
action_27 (26) = happyGoto action_46
action_27 (27) = happyGoto action_47
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (44) = happyShift action_48
action_28 (45) = happyShift action_49
action_28 (49) = happyShift action_50
action_28 (58) = happyShift action_51
action_28 (63) = happyShift action_52
action_28 (64) = happyShift action_53
action_28 (68) = happyShift action_54
action_28 (69) = happyShift action_55
action_28 (70) = happyShift action_56
action_28 (71) = happyShift action_57
action_28 (19) = happyGoto action_39
action_28 (20) = happyGoto action_40
action_28 (21) = happyGoto action_41
action_28 (22) = happyGoto action_42
action_28 (23) = happyGoto action_43
action_28 (24) = happyGoto action_44
action_28 (25) = happyGoto action_45
action_28 (26) = happyGoto action_46
action_28 (27) = happyGoto action_47
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (57) = happyShift action_38
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_5

action_31 _ = happyReduce_11

action_32 (57) = happyShift action_37
action_32 _ = happyReduce_8

action_33 _ = happyReduce_12

action_34 _ = happyReduce_13

action_35 _ = happyReduce_14

action_36 _ = happyReduce_15

action_37 (44) = happyShift action_48
action_37 (45) = happyShift action_49
action_37 (49) = happyShift action_50
action_37 (58) = happyShift action_51
action_37 (63) = happyShift action_52
action_37 (64) = happyShift action_53
action_37 (68) = happyShift action_54
action_37 (69) = happyShift action_55
action_37 (70) = happyShift action_56
action_37 (71) = happyShift action_57
action_37 (19) = happyGoto action_90
action_37 (20) = happyGoto action_40
action_37 (21) = happyGoto action_41
action_37 (22) = happyGoto action_42
action_37 (23) = happyGoto action_43
action_37 (24) = happyGoto action_44
action_37 (25) = happyGoto action_45
action_37 (26) = happyGoto action_46
action_37 (27) = happyGoto action_47
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (44) = happyShift action_48
action_38 (45) = happyShift action_49
action_38 (49) = happyShift action_50
action_38 (58) = happyShift action_51
action_38 (63) = happyShift action_52
action_38 (64) = happyShift action_53
action_38 (68) = happyShift action_54
action_38 (69) = happyShift action_55
action_38 (70) = happyShift action_56
action_38 (71) = happyShift action_57
action_38 (19) = happyGoto action_89
action_38 (20) = happyGoto action_40
action_38 (21) = happyGoto action_41
action_38 (22) = happyGoto action_42
action_38 (23) = happyGoto action_43
action_38 (24) = happyGoto action_44
action_38 (25) = happyGoto action_45
action_38 (26) = happyGoto action_46
action_38 (27) = happyGoto action_47
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (43) = happyShift action_88
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (47) = happyShift action_86
action_40 (48) = happyShift action_87
action_40 _ = happyReduce_29

action_41 (46) = happyShift action_85
action_41 _ = happyReduce_30

action_42 _ = happyReduce_33

action_43 (50) = happyShift action_76
action_43 (51) = happyShift action_77
action_43 (52) = happyShift action_78
action_43 (53) = happyShift action_79
action_43 (54) = happyShift action_80
action_43 (55) = happyShift action_81
action_43 (56) = happyShift action_82
action_43 (63) = happyShift action_83
action_43 (64) = happyShift action_84
action_43 _ = happyReduce_35

action_44 (66) = happyShift action_74
action_44 (67) = happyShift action_75
action_44 _ = happyReduce_42

action_45 _ = happyReduce_46

action_46 (65) = happyShift action_73
action_46 _ = happyReduce_49

action_47 _ = happyReduce_54

action_48 _ = happyReduce_59

action_49 _ = happyReduce_60

action_50 (44) = happyShift action_48
action_50 (45) = happyShift action_49
action_50 (49) = happyShift action_50
action_50 (58) = happyShift action_51
action_50 (63) = happyShift action_52
action_50 (64) = happyShift action_53
action_50 (68) = happyShift action_54
action_50 (69) = happyShift action_55
action_50 (70) = happyShift action_56
action_50 (71) = happyShift action_57
action_50 (26) = happyGoto action_72
action_50 (27) = happyGoto action_47
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (44) = happyShift action_48
action_51 (45) = happyShift action_49
action_51 (49) = happyShift action_50
action_51 (58) = happyShift action_51
action_51 (63) = happyShift action_52
action_51 (64) = happyShift action_53
action_51 (68) = happyShift action_54
action_51 (69) = happyShift action_55
action_51 (70) = happyShift action_56
action_51 (71) = happyShift action_57
action_51 (19) = happyGoto action_71
action_51 (20) = happyGoto action_40
action_51 (21) = happyGoto action_41
action_51 (22) = happyGoto action_42
action_51 (23) = happyGoto action_43
action_51 (24) = happyGoto action_44
action_51 (25) = happyGoto action_45
action_51 (26) = happyGoto action_46
action_51 (27) = happyGoto action_47
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (44) = happyShift action_48
action_52 (45) = happyShift action_49
action_52 (58) = happyShift action_51
action_52 (68) = happyShift action_54
action_52 (69) = happyShift action_55
action_52 (70) = happyShift action_56
action_52 (71) = happyShift action_57
action_52 (27) = happyGoto action_70
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (44) = happyShift action_48
action_53 (45) = happyShift action_49
action_53 (58) = happyShift action_51
action_53 (68) = happyShift action_54
action_53 (69) = happyShift action_55
action_53 (70) = happyShift action_56
action_53 (71) = happyShift action_57
action_53 (27) = happyGoto action_69
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_58

action_55 _ = happyReduce_57

action_56 _ = happyReduce_55

action_57 _ = happyReduce_56

action_58 (40) = happyShift action_68
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (69) = happyShift action_67
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (44) = happyShift action_48
action_60 (45) = happyShift action_49
action_60 (49) = happyShift action_50
action_60 (58) = happyShift action_51
action_60 (63) = happyShift action_52
action_60 (64) = happyShift action_53
action_60 (68) = happyShift action_54
action_60 (69) = happyShift action_55
action_60 (70) = happyShift action_56
action_60 (71) = happyShift action_57
action_60 (19) = happyGoto action_66
action_60 (20) = happyGoto action_40
action_60 (21) = happyGoto action_41
action_60 (22) = happyGoto action_42
action_60 (23) = happyGoto action_43
action_60 (24) = happyGoto action_44
action_60 (25) = happyGoto action_45
action_60 (26) = happyGoto action_46
action_60 (27) = happyGoto action_47
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_18

action_62 (61) = happyShift action_65
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (28) = happyShift action_64
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (61) = happyShift action_111
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_19

action_66 (59) = happyShift action_110
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (62) = happyShift action_109
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (29) = happyShift action_25
action_68 (30) = happyShift action_26
action_68 (39) = happyShift action_27
action_68 (42) = happyShift action_28
action_68 (69) = happyShift action_29
action_68 (12) = happyGoto action_108
action_68 (13) = happyGoto action_19
action_68 (14) = happyGoto action_20
action_68 (15) = happyGoto action_21
action_68 (16) = happyGoto action_22
action_68 (17) = happyGoto action_23
action_68 (18) = happyGoto action_24
action_68 _ = happyReduce_17

action_69 _ = happyReduce_53

action_70 _ = happyReduce_52

action_71 (59) = happyShift action_107
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_51

action_73 (44) = happyShift action_48
action_73 (45) = happyShift action_49
action_73 (49) = happyShift action_50
action_73 (58) = happyShift action_51
action_73 (63) = happyShift action_52
action_73 (64) = happyShift action_53
action_73 (68) = happyShift action_54
action_73 (69) = happyShift action_55
action_73 (70) = happyShift action_56
action_73 (71) = happyShift action_57
action_73 (25) = happyGoto action_106
action_73 (26) = happyGoto action_46
action_73 (27) = happyGoto action_47
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (44) = happyShift action_48
action_74 (45) = happyShift action_49
action_74 (49) = happyShift action_50
action_74 (58) = happyShift action_51
action_74 (63) = happyShift action_52
action_74 (64) = happyShift action_53
action_74 (68) = happyShift action_54
action_74 (69) = happyShift action_55
action_74 (70) = happyShift action_56
action_74 (71) = happyShift action_57
action_74 (25) = happyGoto action_105
action_74 (26) = happyGoto action_46
action_74 (27) = happyGoto action_47
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (44) = happyShift action_48
action_75 (45) = happyShift action_49
action_75 (49) = happyShift action_50
action_75 (58) = happyShift action_51
action_75 (63) = happyShift action_52
action_75 (64) = happyShift action_53
action_75 (68) = happyShift action_54
action_75 (69) = happyShift action_55
action_75 (70) = happyShift action_56
action_75 (71) = happyShift action_57
action_75 (25) = happyGoto action_104
action_75 (26) = happyGoto action_46
action_75 (27) = happyGoto action_47
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (44) = happyShift action_48
action_76 (45) = happyShift action_49
action_76 (49) = happyShift action_50
action_76 (58) = happyShift action_51
action_76 (63) = happyShift action_52
action_76 (64) = happyShift action_53
action_76 (68) = happyShift action_54
action_76 (69) = happyShift action_55
action_76 (70) = happyShift action_56
action_76 (71) = happyShift action_57
action_76 (23) = happyGoto action_103
action_76 (24) = happyGoto action_44
action_76 (25) = happyGoto action_45
action_76 (26) = happyGoto action_46
action_76 (27) = happyGoto action_47
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (44) = happyShift action_48
action_77 (45) = happyShift action_49
action_77 (49) = happyShift action_50
action_77 (58) = happyShift action_51
action_77 (63) = happyShift action_52
action_77 (64) = happyShift action_53
action_77 (68) = happyShift action_54
action_77 (69) = happyShift action_55
action_77 (70) = happyShift action_56
action_77 (71) = happyShift action_57
action_77 (23) = happyGoto action_102
action_77 (24) = happyGoto action_44
action_77 (25) = happyGoto action_45
action_77 (26) = happyGoto action_46
action_77 (27) = happyGoto action_47
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (44) = happyShift action_48
action_78 (45) = happyShift action_49
action_78 (49) = happyShift action_50
action_78 (58) = happyShift action_51
action_78 (63) = happyShift action_52
action_78 (64) = happyShift action_53
action_78 (68) = happyShift action_54
action_78 (69) = happyShift action_55
action_78 (70) = happyShift action_56
action_78 (71) = happyShift action_57
action_78 (23) = happyGoto action_101
action_78 (24) = happyGoto action_44
action_78 (25) = happyGoto action_45
action_78 (26) = happyGoto action_46
action_78 (27) = happyGoto action_47
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (44) = happyShift action_48
action_79 (45) = happyShift action_49
action_79 (49) = happyShift action_50
action_79 (58) = happyShift action_51
action_79 (63) = happyShift action_52
action_79 (64) = happyShift action_53
action_79 (68) = happyShift action_54
action_79 (69) = happyShift action_55
action_79 (70) = happyShift action_56
action_79 (71) = happyShift action_57
action_79 (23) = happyGoto action_100
action_79 (24) = happyGoto action_44
action_79 (25) = happyGoto action_45
action_79 (26) = happyGoto action_46
action_79 (27) = happyGoto action_47
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (44) = happyShift action_48
action_80 (45) = happyShift action_49
action_80 (49) = happyShift action_50
action_80 (58) = happyShift action_51
action_80 (63) = happyShift action_52
action_80 (64) = happyShift action_53
action_80 (68) = happyShift action_54
action_80 (69) = happyShift action_55
action_80 (70) = happyShift action_56
action_80 (71) = happyShift action_57
action_80 (23) = happyGoto action_99
action_80 (24) = happyGoto action_44
action_80 (25) = happyGoto action_45
action_80 (26) = happyGoto action_46
action_80 (27) = happyGoto action_47
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (44) = happyShift action_48
action_81 (45) = happyShift action_49
action_81 (49) = happyShift action_50
action_81 (58) = happyShift action_51
action_81 (63) = happyShift action_52
action_81 (64) = happyShift action_53
action_81 (68) = happyShift action_54
action_81 (69) = happyShift action_55
action_81 (70) = happyShift action_56
action_81 (71) = happyShift action_57
action_81 (23) = happyGoto action_98
action_81 (24) = happyGoto action_44
action_81 (25) = happyGoto action_45
action_81 (26) = happyGoto action_46
action_81 (27) = happyGoto action_47
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (44) = happyShift action_48
action_82 (45) = happyShift action_49
action_82 (49) = happyShift action_50
action_82 (58) = happyShift action_51
action_82 (63) = happyShift action_52
action_82 (64) = happyShift action_53
action_82 (68) = happyShift action_54
action_82 (69) = happyShift action_55
action_82 (70) = happyShift action_56
action_82 (71) = happyShift action_57
action_82 (24) = happyGoto action_97
action_82 (25) = happyGoto action_45
action_82 (26) = happyGoto action_46
action_82 (27) = happyGoto action_47
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (44) = happyShift action_48
action_83 (45) = happyShift action_49
action_83 (49) = happyShift action_50
action_83 (58) = happyShift action_51
action_83 (63) = happyShift action_52
action_83 (64) = happyShift action_53
action_83 (68) = happyShift action_54
action_83 (69) = happyShift action_55
action_83 (70) = happyShift action_56
action_83 (71) = happyShift action_57
action_83 (24) = happyGoto action_96
action_83 (25) = happyGoto action_45
action_83 (26) = happyGoto action_46
action_83 (27) = happyGoto action_47
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (44) = happyShift action_48
action_84 (45) = happyShift action_49
action_84 (49) = happyShift action_50
action_84 (58) = happyShift action_51
action_84 (63) = happyShift action_52
action_84 (64) = happyShift action_53
action_84 (68) = happyShift action_54
action_84 (69) = happyShift action_55
action_84 (70) = happyShift action_56
action_84 (71) = happyShift action_57
action_84 (24) = happyGoto action_95
action_84 (25) = happyGoto action_45
action_84 (26) = happyGoto action_46
action_84 (27) = happyGoto action_47
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (44) = happyShift action_48
action_85 (45) = happyShift action_49
action_85 (49) = happyShift action_50
action_85 (58) = happyShift action_51
action_85 (63) = happyShift action_52
action_85 (64) = happyShift action_53
action_85 (68) = happyShift action_54
action_85 (69) = happyShift action_55
action_85 (70) = happyShift action_56
action_85 (71) = happyShift action_57
action_85 (22) = happyGoto action_94
action_85 (23) = happyGoto action_43
action_85 (24) = happyGoto action_44
action_85 (25) = happyGoto action_45
action_85 (26) = happyGoto action_46
action_85 (27) = happyGoto action_47
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (44) = happyShift action_48
action_86 (45) = happyShift action_49
action_86 (49) = happyShift action_50
action_86 (58) = happyShift action_51
action_86 (63) = happyShift action_52
action_86 (64) = happyShift action_53
action_86 (68) = happyShift action_54
action_86 (69) = happyShift action_55
action_86 (70) = happyShift action_56
action_86 (71) = happyShift action_57
action_86 (21) = happyGoto action_93
action_86 (22) = happyGoto action_42
action_86 (23) = happyGoto action_43
action_86 (24) = happyGoto action_44
action_86 (25) = happyGoto action_45
action_86 (26) = happyGoto action_46
action_86 (27) = happyGoto action_47
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (44) = happyShift action_48
action_87 (45) = happyShift action_49
action_87 (49) = happyShift action_50
action_87 (58) = happyShift action_51
action_87 (63) = happyShift action_52
action_87 (64) = happyShift action_53
action_87 (68) = happyShift action_54
action_87 (69) = happyShift action_55
action_87 (70) = happyShift action_56
action_87 (71) = happyShift action_57
action_87 (21) = happyGoto action_92
action_87 (22) = happyGoto action_42
action_87 (23) = happyGoto action_43
action_87 (24) = happyGoto action_44
action_87 (25) = happyGoto action_45
action_87 (26) = happyGoto action_46
action_87 (27) = happyGoto action_47
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (29) = happyShift action_25
action_88 (30) = happyShift action_26
action_88 (39) = happyShift action_27
action_88 (42) = happyShift action_28
action_88 (69) = happyShift action_29
action_88 (12) = happyGoto action_91
action_88 (13) = happyGoto action_19
action_88 (14) = happyGoto action_20
action_88 (15) = happyGoto action_21
action_88 (16) = happyGoto action_22
action_88 (17) = happyGoto action_23
action_88 (18) = happyGoto action_24
action_88 _ = happyReduce_17

action_89 _ = happyReduce_25

action_90 _ = happyReduce_9

action_91 (37) = happyShift action_114
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (46) = happyShift action_85
action_92 _ = happyReduce_32

action_93 (46) = happyShift action_85
action_93 _ = happyReduce_31

action_94 _ = happyReduce_34

action_95 (66) = happyShift action_74
action_95 (67) = happyShift action_75
action_95 _ = happyReduce_45

action_96 (66) = happyShift action_74
action_96 (67) = happyShift action_75
action_96 _ = happyReduce_44

action_97 (66) = happyShift action_74
action_97 (67) = happyShift action_75
action_97 _ = happyReduce_43

action_98 (56) = happyShift action_82
action_98 (63) = happyShift action_83
action_98 (64) = happyShift action_84
action_98 _ = happyReduce_39

action_99 (56) = happyShift action_82
action_99 (63) = happyShift action_83
action_99 (64) = happyShift action_84
action_99 _ = happyReduce_37

action_100 (56) = happyShift action_82
action_100 (63) = happyShift action_83
action_100 (64) = happyShift action_84
action_100 _ = happyReduce_40

action_101 (56) = happyShift action_82
action_101 (63) = happyShift action_83
action_101 (64) = happyShift action_84
action_101 _ = happyReduce_38

action_102 (56) = happyShift action_82
action_102 (63) = happyShift action_83
action_102 (64) = happyShift action_84
action_102 _ = happyReduce_41

action_103 (56) = happyShift action_82
action_103 (63) = happyShift action_83
action_103 (64) = happyShift action_84
action_103 _ = happyReduce_36

action_104 _ = happyReduce_48

action_105 _ = happyReduce_47

action_106 _ = happyReduce_50

action_107 _ = happyReduce_61

action_108 (41) = happyShift action_113
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (44) = happyShift action_48
action_109 (45) = happyShift action_49
action_109 (49) = happyShift action_50
action_109 (58) = happyShift action_51
action_109 (63) = happyShift action_52
action_109 (64) = happyShift action_53
action_109 (68) = happyShift action_54
action_109 (69) = happyShift action_55
action_109 (70) = happyShift action_56
action_109 (71) = happyShift action_57
action_109 (19) = happyGoto action_112
action_109 (20) = happyGoto action_40
action_109 (21) = happyGoto action_41
action_109 (22) = happyGoto action_42
action_109 (23) = happyGoto action_43
action_109 (24) = happyGoto action_44
action_109 (25) = happyGoto action_45
action_109 (26) = happyGoto action_46
action_109 (27) = happyGoto action_47
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_27

action_111 _ = happyReduce_1

action_112 (59) = happyShift action_117
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (29) = happyShift action_25
action_113 (30) = happyShift action_26
action_113 (39) = happyShift action_27
action_113 (42) = happyShift action_28
action_113 (69) = happyShift action_29
action_113 (12) = happyGoto action_116
action_113 (13) = happyGoto action_19
action_113 (14) = happyGoto action_20
action_113 (15) = happyGoto action_21
action_113 (16) = happyGoto action_22
action_113 (17) = happyGoto action_23
action_113 (18) = happyGoto action_24
action_113 _ = happyReduce_17

action_114 (43) = happyShift action_115
action_114 _ = happyFail (happyExpListPerState 114)

action_115 _ = happyReduce_26

action_116 (37) = happyShift action_118
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_28

action_118 (39) = happyShift action_119
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_24

happyReduce_1 = happyReduce 9 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Prog happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 (EmptyDecl
	)

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (DeclComp happy_var_1 happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (DeclNonInit happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 5 9 happyReduction_9
happyReduction_9 ((HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (DeclInit happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn10
		 (DeclVarLast happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyTerminal (ID happy_var_3))
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (DeclVarNonLast happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn11
		 (TypeInteger
	)

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn11
		 (TypeBoolean
	)

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn11
		 (TypeFloat
	)

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn11
		 (TypeString
	)

happyReduce_16 = happySpecReduce_1  12 happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  12 happyReduction_17
happyReduction_17  =  HappyAbsSyn12
		 (EmptyExec
	)

happyReduce_18 = happySpecReduce_2  13 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (ExecComp happy_var_1 happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  14 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happyReduce 8 15 happyReduction_24
happyReduction_24 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (IfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 (HappyAbsSyn19  happy_var_3)
	_
	(HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn16
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 6 17 happyReduction_26
happyReduction_26 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 18 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (PutLine happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 6 18 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (GetLine happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  19 happyReduction_29
happyReduction_29 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  20 happyReduction_30
happyReduction_30 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  20 happyReduction_31
happyReduction_31 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Or happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  20 happyReduction_32
happyReduction_32 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (XOr happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  21 happyReduction_33
happyReduction_33 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  21 happyReduction_34
happyReduction_34 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (And happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  22 happyReduction_35
happyReduction_35 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  22 happyReduction_36
happyReduction_36 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  22 happyReduction_37
happyReduction_37 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (Lt happy_var_3 happy_var_1
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  22 happyReduction_38
happyReduction_38 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  22 happyReduction_39
happyReduction_39 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (Le happy_var_3 happy_var_1
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  22 happyReduction_40
happyReduction_40 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (Le happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  22 happyReduction_41
happyReduction_41 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (Ne happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  23 happyReduction_42
happyReduction_42 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  23 happyReduction_43
happyReduction_43 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (Concat happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  23 happyReduction_44
happyReduction_44 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (Add happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  23 happyReduction_45
happyReduction_45 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  24 happyReduction_46
happyReduction_46 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  24 happyReduction_47
happyReduction_47 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (Mult happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  24 happyReduction_48
happyReduction_48 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (Div happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  25 happyReduction_49
happyReduction_49 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  25 happyReduction_50
happyReduction_50 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (Pow happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  26 happyReduction_51
happyReduction_51 (HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Not happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  26 happyReduction_52
happyReduction_52 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (happy_var_2
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  26 happyReduction_53
happyReduction_53 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Sub (IntLit 0) happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  26 happyReduction_54
happyReduction_54 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  27 happyReduction_55
happyReduction_55 (HappyTerminal (INTEGER_LITERAL happy_var_1))
	 =  HappyAbsSyn27
		 (IntLit happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  27 happyReduction_56
happyReduction_56 (HappyTerminal (FLOAT_LITERAL happy_var_1))
	 =  HappyAbsSyn27
		 (FloatLit happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  27 happyReduction_57
happyReduction_57 (HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn27
		 (Var happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  27 happyReduction_58
happyReduction_58 (HappyTerminal (STRING_LITERAL happy_var_1))
	 =  HappyAbsSyn27
		 (StringLit happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  27 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn27
		 (TrueLit
	)

happyReduce_60 = happySpecReduce_1  27 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn27
		 (FalseLit
	)

happyReduce_61 = happySpecReduce_3  27 happyReduction_61
happyReduction_61 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (happy_var_2
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 72 72 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	MAIN -> cont 28;
	PUTL -> cont 29;
	GETL -> cont 30;
	INT -> cont 31;
	BOOL -> cont 32;
	FLOAT -> cont 33;
	STRING -> cont 34;
	PROC -> cont 35;
	BEGIN -> cont 36;
	END -> cont 37;
	IS -> cont 38;
	IF -> cont 39;
	THEN -> cont 40;
	ELSE -> cont 41;
	WHILE -> cont 42;
	LOOP -> cont 43;
	TRUE -> cont 44;
	FALSE -> cont 45;
	AND -> cont 46;
	OR -> cont 47;
	XOR -> cont 48;
	NOT -> cont 49;
	EQUAL -> cont 50;
	NOT_EQUAL -> cont 51;
	LESS -> cont 52;
	LESS_EQ -> cont 53;
	GREAT -> cont 54;
	GREAT_EQ -> cont 55;
	CONCAT -> cont 56;
	ASSIGN -> cont 57;
	LPAREN -> cont 58;
	RPAREN -> cont 59;
	COLON -> cont 60;
	SEMI -> cont 61;
	COMMA -> cont 62;
	PLUS -> cont 63;
	SUB -> cont 64;
	POW -> cont 65;
	MULT -> cont 66;
	DIV -> cont 67;
	STRING_LITERAL happy_dollar_dollar -> cont 68;
	ID happy_dollar_dollar -> cont 69;
	INTEGER_LITERAL happy_dollar_dollar -> cont 70;
	FLOAT_LITERAL happy_dollar_dollar -> cont 71;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 72 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Prog = Prog Decl Exec
    deriving Show

data Decl = DeclInit DeclVar Type Exp
          | DeclNonInit DeclVar Type
          | DeclComp Decl Decl
          | EmptyDecl
    deriving Show

data DeclVar = DeclVarNonLast DeclVar String
             | DeclVarLast String
    deriving Show

data Type = TypeInteger
          | TypeBoolean
          | TypeFloat 
          | TypeString
    deriving Show

data Exec = Assign String Exp
          | IfThenElse Exp Exec Exec
          | While Exp Exec
          | PutLine Exp
          | GetLine String Exp
          | ExecComp Exec Exec
          | EmptyExec
    deriving (Show, Eq)

data Exp = TrueLit
         | FalseLit
         | IntLit Int
         | FloatLit Double
         | Var String
         | StringLit String
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Pow Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | XOr Exp Exp
         | Eq Exp Exp
         | Ne Exp Exp
         | Lt Exp Exp
         | Le Exp Exp
         | Not Exp
         | Concat Exp Exp
    deriving (Show, Eq)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
