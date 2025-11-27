module SymbolTable where

import Lexer
import Parser
import Data.List
import Control.Monad.State
import System.Environment (getArgs)

{--
   Forgive the crudeness of the code. You see, it's the first time I tried using State Monads
   (or even Monads, for what it's worth - although at least the Maybe Monad would count, so,
   more precisely, it's the first time I use a Monad explicitly)
--}

-- TBD (the comment below)

{--
   There's at least one change that is yet to be done: we need add implicit conversion for
   when a Float and an Integer operate with one another, for example during the assignment or
   arithmetic operations. This can be done via the inclusion of a new argument for the typeCheck
   function, with the purpose of it being the expected type of the operation, which would force
   all Integers to implicitly convert into Floats.

   We should also try to create type error texts, by adding a String type to the TypeErrorST.
--}

data TypeST = TypeIntegerST
            | TypeBooleanST
            | TypeFloatST
            | TypeStringST
            | TypeErrorST
    deriving (Show, Eq)

convertType :: Type -> TypeST
convertType t | t == TypeInteger = TypeIntegerST
              | t == TypeBoolean = TypeBooleanST
              | t == TypeFloat   = TypeFloatST
              | t == TypeString  = TypeStringST
              | otherwise        = TypeErrorST

type Name = String
type SymTab = [(Name, TypeST]
type ScopeMem = ([SymTab], [SymTab])
type ErrorList = [(Exp, TypeST, TypeST)]
type SymTabState = (SymTab, (ErrorList, ScopeMem))


emptyST :: SymTabState
emptyST = ([], ([], ([], [])))

bindST :: Name -> TypeST -> State SymTabState SymTabState
bindST n t = state (\s -> let (x, xs, ys) = ((n ++ "@" ++ (show ((length $ snd $ snd $ snd s) + (length $ fst $ snd $ snd s))), t), fst s, snd s) in ((x:xs, ys), (x:xs, ys)))

enterScopeST :: State SymTabState SymTabState
enterScopeST = state (\s -> ((fst s, (fst $ snd s, (fst $ snd $ snd s, fst s:(snd $ snd $ snd s)))), (fst s, (fst $ snd s, (fst $ snd $ snd s, fst s:(snd $ snd $ snd s))))))

exitScopeST :: State SymTabState SymTabState
exitScopeST = state (\s -> ((head $ snd $ snd $ snd s, (fst $ snd s, (fst s:(fst $ snd $ snd s), tail $ snd $ snd $ snd s))), ((head $ snd $ snd $ snd s, (fst $ snd s, (fst s:(fst $ snd $ snd s), tail $ snd $ snd $ snd s))))))

lookUpST :: Name -> State SymTabState TypeST
lookUpST n = state (\s -> (removeJustType $ lookup n (fst s), s))

updateErrorTableST :: Exp -> TypeST -> TypeST -> State SymTabState SymTabState
updateErrorTableST exp t1 t2 | t1 /= t2 = state (\s -> ((fst s, ((exp, t1, t2):(fst $ snd s), snd $ snd s)), (fst s, ((exp, t1, t2):(fst $ snd s), snd $ snd s))))
                             | otherwise        = state (\s -> (s, s) )

buildSTProg :: Prog -> State SymTabState SymTabState
buildSTProg (Prog d e) = buildSTDecl d >>= \t0 -> buildSTExec e >>= \t1 -> return t1

buildSTDecl :: Decl -> State SymTabState SymTabState
buildSTDecl (DeclInit dv t exp) = buildSTDeclVar dv t' >>= \t0 -> typeCheck exp >>= \t1 -> updateErrorTableST exp t1 t' >>= \t2 -> return t2
    where t' = convertType t
buildSTDecl (DeclNonInit dv t) = buildSTDeclVar dv t' >>= \t0 -> return t0
    where t' = convertType t
buildSTDecl (DeclComp d1 d2) = buildSTDecl d1 >>= \t0 -> buildSTDecl d2 >>= \t1 -> return t1
buildSTDecl (EmptyDecl) = get >>= \t0 -> return t0

buildSTDeclVar :: DeclVar -> TypeST -> State SymTabState SymTabState
buildSTDeclVar (DeclVarNonLast dv s) t = bindST s t >>= \t0 -> buildSTDeclVar dv t >>= \t1 -> return t1
buildSTDeclVar (DeclVarLast s) t = bindST s t >>= \t0 -> return t0

buildSTExec :: Exec -> State SymTabState SymTabState
buildSTExec (Assign n exp) = lookUpST n >>= \t0 -> typeCheck exp >>= \t1 -> updateErrorTableST exp t0 t1 >>= \t2 -> return t2
buildSTExec (IfThenElse exp e1 e2) = typeCheck exp >>= \t0 -> updateErrorTableST exp t0 TypeBooleanST >>= \t1 -> buildSTExec e1 >>= \t2 -> buildSTExec e2 >>= \t3 -> return t3
buildSTExec (WhileLoop exp e) = typeCheck exp >>= \t0 -> updateErrorTableST exp t0 TypeBooleanST >>= \t1 -> buildSTExec e >>= \t2 -> return t2
buildSTExec (PutLine exp) = typeCheck exp >>= \t0 -> updateErrorTableST exp t0 TypeStringST >>= \t1 -> return t1
buildSTExec (GetLine n exp) = typeCheck exp >>= \t0 -> updateErrorTableST exp t0 TypeIntegerST >>= \t1 -> typeCheck n >>= \t2 -> updateErrorTableST n t2 TypeStringST >>= \t3 -> return t3
buildSTExec (ExecComp e1 e2) = buildSTExec e1 >>= \t0 -> buildSTExec e2 >>= \t1 -> return t1
buildSTExec (DeclBlock d e) = enterScopeST >>= \t0 -> buildSTDecl d >>= \t1 -> buildSTExec e >>= \t2 -> exitScopeST >>= \t3 -> return t3
buildSTExec (EmptyExec) = get >>= \t0 -> return t0

type Code = String

example :: Code
example = "procedure Main is x : Integer := 1; begin x := 2; end Main;"

getSymTab :: Code -> SymTab
getSymTab c = fst $ evalState (buildSTProg $ parse $ alexScanTokensInsensitive c) emptyST

lookUpSymTab :: Name -> SymTab -> Maybe TypeST
lookUpSymTab n [] = Just TypeErrorST
lookUpSymTab n (x:xs) | n /= fst x = lookUpSymTab n xs
                      | otherwise  = Just (snd x)

removeJustType :: Maybe TypeST -> TypeST
removeJustType (Just t) = t
removeJustType Nothing = TypeErrorST

nameToTypeST :: Name -> Code -> TypeST
nameToTypeST n c = removeJustType $ (lookUpSymTab n) $ getSymTab c


typeCheck :: Exp -> State SymTabState TypeST
typeCheck TrueLit = return TypeBooleanST
typeCheck FalseLit = return TypeBooleanST
typeCheck (IntLit _) = return TypeIntegerST
typeCheck (FloatLit _) = return TypeFloatST
typeCheck (StringLit _) = return TypeStringST
typeCheck (Var n) = lookUpST n >>= \t0 -> return t0
typeCheck (Add e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (elem (t0) [TypeIntegerST, TypeFloatST] && t0 == t1) then t1 else TypeErrorST)
typeCheck (Sub e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (elem (t0) [TypeIntegerST, TypeFloatST] && t0 == t1) then t1 else TypeErrorST)
typeCheck (Mult e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (elem (t0) [TypeIntegerST, TypeFloatST] && t0 == t1) then t1 else TypeErrorST)
typeCheck (Div e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (elem (t0) [TypeIntegerST, TypeFloatST] && t0 == t1) then t1 else TypeErrorST)
typeCheck (And e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (t0 == TypeBooleanST && t0 == t1) then TypeBooleanST else TypeErrorST)
typeCheck (Or e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (t0 == TypeBooleanST && t0 == t1) then TypeBooleanST else TypeErrorST)
typeCheck (XOr e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (t0 == TypeBooleanST && t0 == t1) then TypeBooleanST else TypeErrorST)
typeCheck (Eq e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (t0 == TypeBooleanST && t0 == t1) then TypeBooleanST else TypeErrorST)
typeCheck (Ne e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (t0 == TypeBooleanST && t0 == t1) then TypeBooleanST else TypeErrorST)
typeCheck (Lt e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (t0 == TypeBooleanST && t0 == t1) then TypeBooleanST else TypeErrorST)
typeCheck (Le e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (t0 == TypeBooleanST && t0 == t1) then TypeBooleanST else TypeErrorST)
typeCheck (Pow e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (elem t0 [TypeIntegerST, TypeFloatST] && t1 == TypeIntegerST) then t0 else TypeErrorST)
typeCheck (Concat e1 e2) = typeCheck e1 >>= \t0 -> typeCheck e2 >>= \t1 -> return (if (t0 == TypeStringST && t0 == t1) then TypeStringST else TypeErrorST)
typeCheck (Not e1) = typeCheck e1 >>= \t0 -> return (if (t0 == TypeBooleanST) then TypeBooleanST else TypeErrorST)


-- This main function can be used for debugging - or experimenting with - this program on GHCi (it's the acronym for Haskell's interpreter, for those unaware).

{--
main :: IO ()d
main = do
    code <- getLine
    print $ evalState (buildSTProg $ parse $ alexScanTokensInsensitive code) emptyST
--}

