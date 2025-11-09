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

data TypeST = TypeIntegerST
            | TypeBooleanST
            | TypeFloatST
            | TypeStringST
            | TypeErrorST -- só serve para mostrar que não existe nas funções de lookUp (exceto a lookUpST, que atua durante a criação da tabela - atualmente não tem utilidade)
    deriving (Show, Eq)

convertType :: Type -> TypeST
convertType t | t == TypeInteger = TypeIntegerST
              | t == TypeBoolean = TypeBooleanST
              | t == TypeFloat   = TypeFloatST
              | t == TypeString  = TypeStringST
              | otherwise        = TypeErrorST

type Name = String
type SymTab = [(Name, TypeST)]
type ScopeMem = ([SymTab], [SymTab])
type SymTabState = (SymTab, ScopeMem)

emptyST :: SymTabState
emptyST = ([], ([], []))

bindST :: Name -> TypeST -> State SymTabState SymTabState
bindST n t = state (\s -> let (x, xs, ys) = ((n, t), fst s, snd s) in ((x:xs, ys), (x:xs, ys)))

enterScopeST :: State SymTabState SymTabState
enterScopeST = state (\s -> ((fst s, (fst s:(fst $ snd s), snd $ snd s)), (fst s, (fst s:(fst $ snd s), snd $ snd s))))

exitScopeST :: State SymTabState SymTabState
exitScopeST = state (\s -> ((head $ fst $ snd s, (tail $ fst $ snd s, fst s:(snd $ snd s))), (head $ fst $ snd s, (tail $ fst $ snd s, fst s:(snd $ snd s)))))

lookUpST :: Name -> State SymTabState (Maybe TypeST)
lookUpST n = state (\s -> (lookup n (fst s), s))

buildSTProg :: Prog -> State SymTabState SymTabState
buildSTProg (Prog d e) = buildSTDecl d >>= \t0 -> buildSTExec e >>= \t1 -> return t1

buildSTDecl :: Decl -> State SymTabState SymTabState
buildSTDecl (DeclInit dv t exp) = buildSTDeclVar dv t' >>= \t0 -> return t0
    where t' = convertType t
buildSTDecl (DeclNonInit dv t) = buildSTDeclVar dv t' >>= \t0 -> return t0
    where t' = convertType t
buildSTDecl (DeclComp d1 d2) = buildSTDecl d1 >>= \t0 -> buildSTDecl d2 >>= \t1 -> return t1
buildSTDecl (EmptyDecl) = get >>= \t0 -> return t0

buildSTDeclVar :: DeclVar -> TypeST -> State SymTabState SymTabState
buildSTDeclVar (DeclVarNonLast dv s) t = get >>= \t0 -> bindST s t >>= \t1 -> buildSTDeclVar dv t >>= \t2 -> return t2
buildSTDeclVar (DeclVarLast s) t = get >>= \t0 -> bindST s t >>= \t1 -> return t1

buildSTExec :: Exec -> State SymTabState SymTabState
buildSTExec (Assign s exp) = get >>= \t0 -> return t0
buildSTExec (IfThenElse exp e1 e2) = buildSTExec e1 >>= \t0 -> buildSTExec e2 >>= \t1 -> return t1
buildSTExec (WhileLoop exp e) = buildSTExec e >>= \t0 -> return t0
buildSTExec (PutLine exp) = get >>= \t0 -> return t0
buildSTExec (GetLine s exp) = get >>= \t0 -> return t0
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

nameToTypeST :: Name -> Code -> TypeST
nameToTypeST n c = removeJustType $ (lookUpSymTab n) $ getSymTab c

-- esta função main serve para testar no GHCi

{--
main :: IO ()
main = do
    code <- getLine
    print $ evalState (buildSTProg $ parse $ alexScanTokensInsensitive code) emptyST
--}

