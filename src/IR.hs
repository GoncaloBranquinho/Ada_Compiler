module IR where

import Lexer
import Parser
import Data.List
import Control.Monad.State
import SymbolTable

data Instr = MOVE Temp Temp
           | MOVEI Temp Literal
           | OP BinOp Temp Temp Temp
           | LABEL Label
           | JUMP Label
           | COND BinOp Temp Temp Label Label
           | PRINT Temp
           | READ Temp
           | LENGTH Temp Temp
           | DECL Temp String
    deriving (Show, Eq)

data BinOp = ADD String | SUB String | MULT String | DIV String | POW String | AND
            | OR | XOR | EQ String | NE String | LT String | LE String | CONCAT
    deriving (Show, Eq)

data Literal = TInt Int | TDouble Double | TString String
    deriving (Eq)

instance Show Literal where
    show (TInt n)    = show n
    show (TDouble n) = show n
    show (TString n) = show n

type Temp = String
type Label = String
type Count = (Int,Int,Int,String)

typeToString :: Type -> String
typeToString TypeInteger = "Integer"
typeToString TypeBoolean = "Boolean"
typeToString TypeFloat   = "Float"
typeToString TypeString  = "String"


newTemp :: State Count Temp
newTemp = do (temps, labels, scope, typ) <- get
             put (temps+1, labels, scope, typ)
             return ("t" ++ show temps)

popTemp :: Int -> State Count ()
popTemp n = do (temps, labels, scope, typ) <- get
               put (temps-n, labels, scope, typ)


newLabel :: State Count Label
newLabel = do (temps,labels, scope, typ) <- get
              put (temps, labels+1, scope, typ)
              return ("label" ++ show labels)

newScope :: State Count ()
newScope = do (temps, labels, scope, typ) <- get
              put (temps, labels, scope+1, typ)

newTyp :: String -> State Count ()
newTyp str = do (temps, labels, scope, _) <- get
                put (temps, labels, scope, str)

-- so me falta no main, tambem passar o primeiro symtab, para ter o escopo 0, e depois preciso tambem de no getVarScope, escrever o codigo para o caso de estarmos no ambito 0
getVarTyp :: State Count String
getVarTyp = do (_,_,_, typ) <- get
               return typ


getScope :: State Count Int
getScope = do (_, _, scope, _) <- get
              return scope

typToString :: TypeST -> String
typToString TypeIntegerST = "Integer"
typToString TypeFloatST = "Float"
typToString TypeBooleanST = "Boolean"
typToString TypeStringST = "String"


getVarScope :: String -> String -> (SymTab, ScopeMem) -> State Count (Temp,String)
getVarScope id scope (symtab,(table, _)) = do if scope == "0"
                                              then do 
                                                  (newScope,typ) <- searchID id symtab
                                                  return (newScope,typ)
                                              else do 
                                                  (newScope,typ) <- searchScope id scope table
                                                  return (newScope,typ)


searchScope :: String -> String -> [(ScopeID, SymTab)] -> State Count (Temp,String)
searchScope id scope1 ((scope2, table):rest) = if scope1 == (show scope2) then searchID id table else searchScope id scope1 rest


searchID :: String -> SymTab -> State Count (Temp,String)
searchID id1 ((temp,(typ,_)):rest) = do let id2 = takeWhile (\x -> x /= '@') temp
                                        if id1 == id2 
                                          then do
                                              let strTyp = typToString typ
                                              return (temp,strTyp) 
                                          else 
                                              searchID id1 rest

transAST :: Prog -> (SymTab, ScopeMem) -> State Count [Instr]
transAST (Prog decl exec) table = do code1 <- transDecl decl table
                                     code2 <- transExec exec table
                                     return (code1 ++ code2)

transDecl :: Decl -> (SymTab, ScopeMem) -> State Count [Instr]
transDecl EmptyDecl table = return []
transDecl (DeclComp decl1 decl2) table = do code1 <- transDecl decl1 table
                                            code2 <- transDecl decl2 table
                                            return (code1 ++ code2)
transDecl (DeclInit ids typ exp) table = do idsList <- transDeclVar ids table
                                            t1 <- newTemp
                                            code1 <- transExp exp table t1
                                            let typString = typeToString typ
                                            let decls = map (\v -> DECL v typString) idsList
                                            let moves = map (\v -> MOVE v t1) idsList
                                            popTemp 1
                                            return (decls ++ code1 ++ moves)
transDecl (DeclNonInit ids typ) table = do idsList <- transDeclVar ids table
                                           let typString = typeToString typ
                                           return (concatMap (\v -> [DECL v typString]) idsList)


transDeclVar :: DeclVar -> (SymTab, ScopeMem) -> State Count [Temp]
transDeclVar (DeclVarLast id) table = do scope <- getScope
                                         let dest = id ++ "@" ++ show scope
                                         return [dest]
transDeclVar (DeclVarNonLast ids id) table = do scope <- getScope
                                                let dest = id ++ "@" ++ show scope
                                                rest <- transDeclVar ids table
                                                return ([dest] ++ rest)


transCond :: Exp -> (SymTab, ScopeMem) -> Label -> Label -> State Count [Instr]
transCond TrueLit table labelt labelf = return [JUMP labelt]
transCond FalseLit table labelt labelf = return [JUMP labelf]
transCond (Not cond) table labelt labelf = do code1 <- transCond cond table labelf labelt
                                              return code1
transCond (Eq exp1 exp2) table labelt labelf = do t1 <- newTemp
                                                  t2 <- newTemp
                                                  code1 <- transExp exp1 table t1
                                                  code2 <- transExp exp2 table t2
                                                  typ <- getVarTyp
                                                  popTemp 2
                                                  return (code1 ++ code2 ++ [COND (IR.EQ typ) t1 t2 labelt labelf])
transCond (Ne exp1 exp2) table labelt labelf = do t1 <- newTemp
                                                  t2 <- newTemp
                                                  code1 <- transExp exp1 table t1
                                                  code2 <- transExp exp2 table t2
                                                  typ <- getVarTyp
                                                  popTemp 2
                                                  return (code1 ++ code2 ++ [COND (IR.NE typ) t1 t2 labelt labelf])
transCond (Lt exp1 exp2) table labelt labelf = do t1 <- newTemp
                                                  t2 <- newTemp
                                                  code1 <- transExp exp1 table t1
                                                  code2 <- transExp exp2 table t2
                                                  typ <- getVarTyp
                                                  popTemp 2
                                                  return (code1 ++ code2 ++ [COND (IR.LT typ) t1 t2 labelt labelf])
transCond (Le exp1 exp2) table labelt labelf = do t1 <- newTemp
                                                  t2 <- newTemp
                                                  code1 <- transExp exp1 table t1
                                                  code2 <- transExp exp2 table t2
                                                  typ <- getVarTyp
                                                  popTemp 2
                                                  return (code1 ++ code2 ++ [COND (IR.LE typ) t1 t2 labelt labelf])
transCond (And cond1 cond2) table labelt labelf = do label1 <- newLabel
                                                     code1 <- transCond cond1 table label1 labelf
                                                     code2 <- transCond cond2 table labelt labelf
                                                     return (code1 ++ [LABEL label1] ++ code2)
transCond (Or cond1 cond2) table labelt labelf = do label1 <- newLabel
                                                    code1 <- transCond cond1 table labelt label1
                                                    code2 <- transCond cond2 table labelt labelf
                                                    return (code1 ++ [LABEL label1] ++ code2)
transCond (XOr cond1 cond2) table labelt labelf = do code1 <- transCond (Or (And cond1 (Not cond2)) (And (Not cond1) cond2)) table labelt labelf
                                                     return code1
transCond (Var id) table labelt labelf = do t1 <- newTemp
                                            code1 <- transExp (Var id) table t1
                                            popTemp 1
                                            return (code1 ++ [COND (NE "Integer") t1 "0" labelt labelf])

transExec :: Exec -> (SymTab, ScopeMem) -> State Count [Instr]
transExec EmptyExec _ = return []
transExec (DeclBlock decl exec) table = do newScope
                                           code1 <- transDecl decl table
                                           code2 <- transExec exec table
                                           return (code1 ++ code2)
transExec (Assign id exp) table = do scope <- getScope
                                     (newDest, _) <- getVarScope id (show scope) table
                                     code1 <- transExp exp table newDest
                                     return code1
transExec (ExecComp exec1 exec2) table = do code1 <- transExec exec1 table
                                            code2 <- transExec exec2 table
                                            return (code1 ++ code2)
transExec (IfThenElse cond exec1 exec2) table = do label1 <- newLabel
                                                   label2 <- newLabel
                                                   label3 <- newLabel
                                                   code1 <- transCond cond table label1 label2
                                                   code2 <- transExec exec1 table
                                                   code3 <- transExec exec2 table
                                                   return (code1 ++ [LABEL label1] ++ code2 ++ [JUMP label3] ++ [LABEL label2] ++ code3 ++ [LABEL label3])
transExec (WhileLoop cond exec) table = do label1 <- newLabel
                                           label2 <- newLabel
                                           label3 <- newLabel
                                           code1 <- transCond cond table label2 label3
                                           code2 <- transExec exec table
                                           return ([LABEL label1] ++ code1 ++ [LABEL label2] ++ code2 ++ [JUMP label1, LABEL label3])
transExec (PutLine exp) table = do t1 <- newTemp
                                   code1 <- transExp exp table t1
                                   popTemp 1
                                   return (code1 ++ [PRINT t1])
transExec (GetLine id1 id2) table = do scope <- getScope
                                       (newDest1,_) <- getVarScope id1 (show scope) table
                                       (newDest2,_) <- getVarScope id2 (show scope) table
                                       t1 <- newTemp
                                       t2 <- newTemp
                                       popTemp 2
                                       return ([READ t1, MOVE newDest1 t1, LENGTH t2 t1] ++ [MOVE newDest2 t2])



transExp :: Exp -> (SymTab, ScopeMem) -> Temp -> State Count [Instr]
transExp TrueLit table dest = do newTyp "Boolean" 
                                 return [MOVEI dest (TInt 1)]
transExp FalseLit table dest = do newTyp "Boolean"
                                  return [MOVEI dest (TInt 0)]
transExp (IntLit num) table dest = do newTyp "Integer"
                                      return [MOVEI dest (TInt num)]
transExp (FloatLit num) table dest = do newTyp "Float"
                                        return [MOVEI dest (TDouble num)]
transExp (StringLit num) table dest = do newTyp "String"
                                         return [MOVEI dest (TString num)]
transExp (Var id) table dest = do scope <- getScope
                                  (newTemp,typ) <- getVarScope id (show scope) table
                                  newTyp typ
                                  return ([MOVE dest newTemp])
transExp (Add exp1 exp2) table dest = do t1 <- newTemp
                                         t2 <- newTemp
                                         code1 <- transExp exp1 table t1
                                         code2 <- transExp exp2 table t2
                                         typ <- getVarTyp
                                         popTemp 2
                                         return (code1 ++ code2 ++ [OP (ADD typ) dest t1 t2])
transExp (Mult exp1 exp2) table dest = do t1 <- newTemp
                                          t2 <- newTemp
                                          code1 <- transExp exp1 table t1
                                          code2 <- transExp exp2 table t2
                                          typ <- getVarTyp
                                          popTemp 2
                                          return (code1 ++ code2 ++ [OP (IR.MULT typ) dest t1 t2])
transExp (Sub exp1 exp2) table dest = do t1 <- newTemp
                                         t2 <- newTemp
                                         code1 <- transExp exp1 table t1
                                         code2 <- transExp exp2 table t2
                                         typ <- getVarTyp
                                         popTemp 2
                                         return (code1 ++ code2 ++ [OP (IR.SUB typ) dest t1 t2])
transExp (Div exp1 exp2) table dest = do t1 <- newTemp
                                         t2 <- newTemp
                                         code1 <- transExp exp1 table t1
                                         code2 <- transExp exp2 table t2
                                         typ <- getVarTyp
                                         popTemp 2
                                         return (code1 ++ code2 ++ [OP (IR.DIV typ) dest t1 t2])
transExp (Pow exp1 exp2) table dest = do t1 <- newTemp
                                         t2 <- newTemp
                                         code1 <- transExp exp1 table t1
                                         code2 <- transExp exp2 table t2
                                         typ <- getVarTyp
                                         popTemp 2
                                         return (code1 ++ code2 ++ [OP (IR.POW typ) dest t1 t2])
transExp (Concat exp1 exp2) table dest = do t1 <- newTemp
                                            t2 <- newTemp
                                            code1 <- transExp exp1 table t1
                                            code2 <- transExp exp2 table t2
                                            popTemp 2
                                            return (code1 ++ code2 ++ [OP IR.CONCAT dest t1 t2])
transExp (And exp1 exp2) table dest = do label1 <- newLabel
                                         label2 <- newLabel
                                         label3 <- newLabel
                                         code1 <- transCond (And exp1 exp2) table label1 label2
                                         return (code1 ++ [LABEL label1, MOVEI dest (TInt 1), JUMP label3, LABEL label2, MOVEI dest (TInt 0), LABEL label3])
transExp (Or exp1 exp2) table dest = do label1 <- newLabel
                                        label2 <- newLabel
                                        label3 <- newLabel
                                        code1 <- transCond (Or exp1 exp2) table label1 label2
                                        return (code1 ++ [LABEL label1, MOVEI dest (TInt 1), JUMP label3, LABEL label2, MOVEI dest (TInt 0), LABEL label3])
transExp (XOr exp1 exp2) table dest = do label1 <- newLabel
                                         label2 <- newLabel
                                         label3 <- newLabel
                                         code1 <- transCond (XOr exp1 exp2) table label1 label2
                                         return (code1 ++ [LABEL label1, MOVEI dest (TInt 1), JUMP label3, LABEL label2, MOVEI dest (TInt 0), LABEL label3])
transExp (Eq exp1 exp2) table dest = do label1 <- newLabel
                                        label2 <- newLabel
                                        label3 <- newLabel 
                                        code1 <- transCond (Eq exp1 exp2) table label1 label2
                                        return (code1 ++ [LABEL label1, MOVEI dest (TInt 1), JUMP label3, LABEL label2, MOVEI dest (TInt 0), LABEL label3])
transExp (Lt exp1 exp2) table dest = do label1 <- newLabel
                                        label2 <- newLabel
                                        label3 <- newLabel
                                        code1 <- transCond (Lt exp1 exp2) table label1 label2
                                        return (code1 ++ [LABEL label1, MOVEI dest (TInt 1), JUMP label3, LABEL label2, MOVEI dest (TInt 0), LABEL label3])
transExp (Le exp1 exp2) table dest = do label1 <- newLabel
                                        label2 <- newLabel
                                        label3 <- newLabel
                                        code1 <- transCond (Le exp1 exp2) table label1 label2
                                        return (code1 ++ [LABEL label1, MOVEI dest (TInt 1), JUMP label3, LABEL label2, MOVEI dest (TInt 0), LABEL label3])
transExp (Ne exp1 exp2) table dest = do label1 <- newLabel
                                        label2 <- newLabel
                                        label3 <- newLabel
                                        code1 <- transCond (Ne exp1 exp2) table label1 label2
                                        return (code1 ++ [LABEL label1, MOVEI dest (TInt 1), JUMP label3, LABEL label2, MOVEI dest (TInt 0), LABEL label3])
transExp (Not cond1) table dest = do label1 <- newLabel
                                     label2 <- newLabel
                                     label3 <- newLabel
                                     code1 <- transCond (Not cond1) table label1 label2
                                     return (code1 ++ [LABEL label1, MOVEI dest (TInt 1), JUMP label3, LABEL label2, MOVEI dest (TInt 0), LABEL label3])
