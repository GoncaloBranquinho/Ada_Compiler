module IR where

import Lexer
import Parser
import Data.List
import Control.Monad.State
import Control.Monad (when)
import SymbolTable
import qualified Data.Map.Strict as Map
import Control.Monad.RWS (MonadState(get))
import Lexer (Token(WHILE))

data Instr = MOVE String Temp Temp
           | MOVEI Temp Literal
           | OP BinOp Temp Temp Temp
           | LABEL Label
           | JUMP Label
           | COND BinOp Temp Temp Label Label
           | PRINT Temp
           | READ Temp Temp
           | DECL Temp String
           | TOSTR String Temp Temp
           | BEGIN
           | END
           | WHILE
           | ENDWHILE
    deriving (Show, Eq)

data BinOp = ADD { val :: String } | SUB { val :: String } | MULT { val :: String } | DIV { val :: String } | POW { val :: String } | AND
            | OR | XOR | EQ { val :: String } | NE { val :: String } | LT { val :: String } | LE { val :: String } | CONCAT { val :: String}
    deriving (Show,Eq)


data Literal = TInt Int | TDouble Float | TString String
    deriving (Show, Eq)


type Temp = String
data WhileInfo = MV Temp Temp | EVAL Temp
   deriving (Eq,Show)
type Label = String
type Table = Map.Map Int [(String,Int,Bool)]
type Count = (Int,Int,Int,String,([String],[Float]), Table, Int, Int, [Int], Int, Int, Map.Map Int [WhileInfo])

emptyIR :: Count
emptyIR = (0,0,0,"",([],[]),Map.empty,0,1,[],-1,0, Map.empty)

typeToString :: Type -> String
typeToString TypeInteger = "Integer"
typeToString TypeBoolean = "Boolean"
typeToString TypeFloat   = "Float"
typeToString TypeString  = "String"

typeToOffset :: Type -> Int
typeToOffset TypeInteger = 4
typeToOffset TypeBoolean = 4
typeToOffset TypeFloat   = 4
typeToOffset TypeString  = 4


newTemp :: State Count Temp
newTemp = do (temps, labels, scope, typ, setString, table, currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
             put (temps+1, labels, scope, typ, setString, table, currScopeMips, nextScopeMips,finishOrder,currWhile,nextWhile,whileMap)
             return ("_t" ++ show temps ++ "@" ++ show currScopeMips)

popTemp :: Int -> State Count ()
popTemp n = do (temps, labels, scope, typ, setString, table, currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
               put (temps-n, labels, scope, typ, setString, table,currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap)


newLabel :: State Count Label
newLabel = do (temps,labels, scope, typ, setString, table,currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
              put (temps, labels+1, scope, typ, setString, table,currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap)
              return ("label" ++ show labels)

newScope :: State Count ()
newScope = do (temps, labels, scope, typ, setString, table,currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
              put (temps, labels, scope+1, typ, setString, table,currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap)

newScopeMips :: State Count ()
newScopeMips = do (temps, labels, scope, typ, setString, table, currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
                  put (temps, labels, scope, typ, setString, table, currScopeMips+1, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap)


newTyp :: String -> State Count ()
newTyp str = do (temps, labels, scope, _, setString, table,currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
                put (temps, labels, scope, str, setString, table,currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap)


getVarTyp :: State Count String
getVarTyp = do (_,_,_, typ,_,_,_,_,_,_,_,_) <- get
               return typ


getCurrScopeMips :: State Count Int
getCurrScopeMips = do (_,_,_,_,_,_,currScopeMips,_,_,_,_,_) <- get
                      return currScopeMips


enterScope :: State Count Int
enterScope = do (temps,labels,scope,typ,setString, table,currScopeMips,nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
                put (temps,labels,scope,typ,setString, table,nextScopeMips,nextScopeMips+1, finishOrder,currWhile,nextWhile,whileMap)
                return nextScopeMips

exitScope :: Int -> State Count ()
exitScope parentScope = do (temps,labels,scope,typ,setString, table,_,nextScopeMips,finishOrder,currWhile,nextWhile,whileMap) <- get
                           put (temps,labels,scope,typ,setString, table,parentScope,nextScopeMips, finishOrder,currWhile,nextWhile,whileMap)




enterWhile :: State Count Int
enterWhile = do (temps,labels,scope,typ,setString, table,currScopeMips,nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
                put (temps,labels,scope,typ,setString, table,currScopeMips,nextScopeMips, finishOrder,nextWhile,nextWhile+1,whileMap)
                return nextWhile

exitWhile :: Int -> State Count ()
exitWhile parentWhile = do (temps,labels,scope,typ,setString, table,currScopeMips,nextScopeMips,finishOrder,_,nextWhile,whileMap) <- get
                           put (temps,labels,scope,typ,setString, table,currScopeMips,nextScopeMips, finishOrder,parentWhile,nextWhile,whileMap)


getCurrWhile :: State Count Int
getCurrWhile = do (_,_,_,_,_,_,_,_,_,currWhile,_,_) <- get
                  return currWhile

addWhile :: WhileInfo -> State Count ()
addWhile id = do (temps,labels,scope,typ,(setString,setFloat), table,currScopeMips,nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
                 let whileInfo = Map.findWithDefault [] currWhile whileMap
                 let whileInfo' = whileInfo ++ (if (elem id whileInfo) then [] else [id])
                 let newMap = Map.insert currWhile whileInfo' whileMap
                 put (temps,labels,scope,typ, (setString,setFloat), table, currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,newMap)

addSet :: String -> State Count ()
addSet str = do (temps,labels,scope,typ,(setString,setFloat), table,currScopeMips,nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
                if (elem str setString)
                  then return ()
                  else put (temps,labels,scope,typ, (setString ++ [str],setFloat), table, currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap)
addSetFloat :: Float -> State Count ()
addSetFloat num = do (temps,labels,scope,typ,(setString,setFloat), table,currScopeMips,nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
                     if (elem num setFloat)
                        then return ()
                        else put (temps,labels,scope,typ, (setString,setFloat++[num]), table, currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap)







addTable :: String -> Int -> Bool -> State Count ()
addTable str offset isFloat = do (temps,labels,scope,typ,setString,table,currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap) <- get
                                 let scopeList = Map.findWithDefault [] currScopeMips table
                                 let newScopeList = if (any (\(s,_,_) -> s == str) scopeList)
                                                      then scopeList
                                                      else (scopeList ++ [(str,offset,isFloat)])
                                 put (temps,labels,scope,typ,setString,Map.insert currScopeMips newScopeList table,currScopeMips, nextScopeMips, finishOrder,currWhile,nextWhile,whileMap)


addFinishOrder :: Int -> State Count ()
addFinishOrder n = do (temps,labels,scope,typ,setString,table,currScopeMips,nextScopeMips,finishOrder,currWhile,nextWhile,whileMap) <- get
                      put (temps,labels,scope,typ,setString,table,currScopeMips,nextScopeMips,finishOrder ++ [n],currWhile,nextWhile,whileMap)

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

transAST :: Prog -> (SymTab, ScopeMem) -> State Count ([Instr],Table,[Int],[String],[Float],Map.Map Int [WhileInfo])
transAST (Prog decl exec) table = do code1 <- transDecl decl table
                                     code2 <- transExec exec table
                                     addFinishOrder 0
                                     (_,_,_,_,(setStr,setFlt),table,_,_, finishOrder,_,_,whileInfo) <- get
                                     return ([IR.BEGIN] ++ code1 ++ code2 ++ [IR.END],table, finishOrder,setStr,setFlt,whileInfo)

transDecl :: Decl -> (SymTab, ScopeMem) -> State Count [Instr]
transDecl EmptyDecl table = return []
transDecl (DeclComp decl1 decl2) table = do code1 <- transDecl decl1 table
                                            code2 <- transDecl decl2 table
                                            return (code1 ++ code2)
transDecl (DeclInit ids typ exp) table = do idsList <- transDeclVar ids table
                                            let offset = typeToOffset typ
                                            let isFloat = typ == TypeFloat
                                            mapM_ (\id -> addTable id offset isFloat) idsList
                                            t1 <- newTemp
                                            addTable t1 offset False
                                            code1 <- transExp exp table t1
                                            let typString = typeToString typ
                                            let newTyp = typeToString typ
                                            (_,_,_,_,_,_,_,_,_,currWhile,_,_) <- get
                                            let decls = map (\v -> DECL v typString) idsList
                                            let moves = map (\v -> MOVE newTyp v t1) idsList
                                            popTemp 1
                                            return (decls ++ code1 ++ moves)
transDecl (DeclNonInit ids typ) table = do idsList <- transDeclVar ids table
                                           let offset = typeToOffset typ
                                           let isFloat = typ == TypeFloat
                                           mapM_ (\id -> addTable id offset isFloat) idsList
                                           let typString = typeToString typ
                                           return (concatMap (\v -> [DECL v typString]) idsList)


transDeclVar :: DeclVar -> (SymTab, ScopeMem) -> State Count [Temp]
transDeclVar (DeclVarLast id) table = do scope <- getCurrScopeMips
                                         let dest = id ++ "@" ++ show scope
                                         return [dest]
transDeclVar (DeclVarNonLast ids id) table = do scope <- getCurrScopeMips
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
                                                  addTable t1 4 False
                                                  addTable t2 4 False
                                                  code1 <- transExp exp1 table t1
                                                  code2 <- transExp exp2 table t2
                                                  typ <- getVarTyp
                                                  popTemp 2
                                                  return (code1 ++ code2 ++ [COND (IR.EQ typ) t1 t2 labelt labelf])
transCond (Ne exp1 exp2) table labelt labelf = do t1 <- newTemp
                                                  t2 <- newTemp
                                                  addTable t1 4 False
                                                  addTable t2 4 False
                                                  code1 <- transExp exp1 table t1
                                                  code2 <- transExp exp2 table t2
                                                  typ <- getVarTyp
                                                  popTemp 2
                                                  return (code1 ++ code2 ++ [COND (IR.NE typ) t1 t2 labelt labelf])
transCond (Lt exp1 exp2) table labelt labelf = do t1 <- newTemp
                                                  t2 <- newTemp
                                                  addTable t1 4 False
                                                  addTable t2 4 False
                                                  code1 <- transExp exp1 table t1
                                                  code2 <- transExp exp2 table t2
                                                  typ <- getVarTyp
                                                  popTemp 2
                                                  return (code1 ++ code2 ++ [COND (IR.LT typ) t1 t2 labelt labelf])
transCond (Le exp1 exp2) table labelt labelf = do t1 <- newTemp
                                                  t2 <- newTemp
                                                  addTable t1 4 False
                                                  addTable t2 4 False
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
                                            t2 <- newTemp
                                            addTable t1 4 False
                                            addTable t2 4 False
                                            code1 <- transExp (Var id) table t1
                                            popTemp 2
                                            return (code1 ++ [MOVEI t2 (TInt 0)] ++ [COND (NE "Integer") t1 t2 labelt labelf])

transExec :: Exec -> (SymTab, ScopeMem) -> State Count [Instr]
transExec EmptyExec _ = return []
transExec (DeclBlock decl exec) table = do newScope
                                           parentScope <- getCurrScopeMips
                                           current <- enterScope
                                           code1 <- transDecl decl table
                                           code2 <- transExec exec table
                                           addFinishOrder current
                                           exitScope parentScope
                                           return ([IR.BEGIN] ++ code1 ++ code2 ++ [IR.END])
transExec (Assign id exp) table = do scope <- getCurrScopeMips
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
                                           parentWhile <- getCurrWhile
                                           current <- enterWhile
                                           code1 <- transCond cond table label2 label3
                                           code2 <- transExec exec table
                                           exitWhile parentWhile
                                           return ([IR.WHILE] ++ [LABEL label1] ++ code1 ++ [LABEL label2] ++ code2 ++ [JUMP label1, LABEL label3, ENDWHILE])
transExec (PutLine exp) table = do t1 <- newTemp
                                   addTable t1 4 False
                                   code1 <- transExp exp table t1
                                   popTemp 1
                                   return (code1 ++ [PRINT t1])
transExec (GetLine id1 id2) table = do scope <- getCurrScopeMips
                                       (newDest1,_) <- getVarScope id1 (show scope) table
                                       (newDest2,_) <- getVarScope id2 (show scope) table
                                       (_,_,_,_,_,_,_,_,_,currWhile,_,_) <- get
                                       when (currWhile >= 0) $ do
                                         addWhile (EVAL newDest1)
                                       return [READ newDest1 newDest2]



transExp :: Exp -> (SymTab, ScopeMem) -> Temp -> State Count [Instr]
transExp TrueLit table dest = do newTyp "Boolean"
                                 addTable dest 4 False
                                 return [MOVEI dest (TInt 1)]
transExp FalseLit table dest = do newTyp "Boolean"
                                  return [MOVEI dest (TInt 0)]
transExp (IntLit num) table dest = do newTyp "Integer"
                                      return [MOVEI dest (TInt num)]
transExp (FloatLit num) table dest = do newTyp "Float"
                                        addSetFloat num
                                        return [MOVEI dest (TDouble num)]
transExp (StringLit num) table dest = do newTyp "String"
                                         addSet num
                                         (_,_,_,_,_,_,_,_,_,currWhile,_,_) <- get
                                         when (currWhile >= 0) $ do
                                           addWhile (MV dest num)
                                         return [MOVEI dest (TString num)]
transExp (Var id) table dest = do scope <- getCurrScopeMips
                                  (newTemp,typ) <- getVarScope id (show scope) table
                                  newTyp typ
                                  addTable dest 4 False
                                  (_,_,_,_,_,_,_,_,_,currWhile,_,_) <- get
                                  when (currWhile >= 0 && typ == "String") $ do
                                    addWhile (MV dest newTemp)
                                  return ([MOVE typ dest newTemp])
transExp (Add exp1 exp2) table dest = do t1 <- newTemp
                                         t2 <- newTemp
                                         addTable t1 4 False
                                         addTable t2 4 False
                                         code1 <- transExp exp1 table t1
                                         code2 <- transExp exp2 table t2
                                         typ <- getVarTyp
                                         popTemp 2
                                         return (code1 ++ code2 ++ [OP (ADD typ) dest t1 t2])
transExp (Mult exp1 exp2) table dest = do t1 <- newTemp
                                          t2 <- newTemp
                                          addTable t1 4 False
                                          addTable t2 4 False
                                          code1 <- transExp exp1 table t1
                                          code2 <- transExp exp2 table t2
                                          typ <- getVarTyp
                                          popTemp 2
                                          return (code1 ++ code2 ++ [OP (IR.MULT typ) dest t1 t2])
transExp (Sub exp1 exp2) table dest = do t1 <- newTemp
                                         t2 <- newTemp
                                         addTable t1 4 False
                                         addTable t2 4 False
                                         code1 <- transExp exp1 table t1
                                         code2 <- transExp exp2 table t2
                                         typ <- getVarTyp
                                         popTemp 2
                                         return (code1 ++ code2 ++ [OP (IR.SUB typ) dest t1 t2])
transExp (Div exp1 exp2) table dest = do t1 <- newTemp
                                         t2 <- newTemp
                                         addTable t1 4 False
                                         addTable t2 4 False
                                         code1 <- transExp exp1 table t1
                                         code2 <- transExp exp2 table t2
                                         typ <- getVarTyp
                                         popTemp 2
                                         return (code1 ++ code2 ++ [OP (IR.DIV typ) dest t1 t2])
transExp (Pow exp1 exp2) table dest = do t1 <- newTemp
                                         t2 <- newTemp
                                         addTable t1 4 False
                                         addTable t2 4 False
                                         code1 <- transExp exp1 table t1
                                         typ1 <- getVarTyp
                                         code2 <- transExp exp2 table t2
                                         typ2 <- getVarTyp
                                         let typ3 = if (typ1 == "Float" || typ2 == "Float") then "Float" else "Integer"
                                         newTyp typ3
                                         popTemp 2
                                         return (code1 ++ code2 ++ [OP (IR.POW typ3) dest t1 t2])
transExp (Concat exp1 exp2) table dest = do t1 <- newTemp
                                            t2 <- newTemp
                                            addTable t1 4 False
                                            addTable t2 4 False
                                            (_,_,_,_,_,_,_,_,_,currWhile,_,_) <- get
                                            when (currWhile >= 0) $ do
                                              addWhile (EVAL t1)
                                              addWhile (EVAL t2)
                                            code1 <- transExp exp1 table t1
                                            code2 <- transExp exp2 table t2
                                            popTemp 2
                                            return (code1 ++ code2 ++ [OP (IR.CONCAT "String") dest t1 t2])
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

transExp (ToStr exp) table dest = do t1 <- newTemp
                                     addTable t1 4 False
                                     code1 <- transExp (exp) table t1
                                     typ <- getVarTyp
                                     return (code1 ++ [IR.TOSTR typ dest t1])
