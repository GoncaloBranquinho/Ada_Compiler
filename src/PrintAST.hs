module PrintAST where

import Parser 

indent :: Int -> String
indent n = replicate (n * 4) ' '

printAST :: Prog -> IO ()
printAST prog = putStrLn (prettyProg 0 prog)

prettyProg :: Int -> Prog -> String
prettyProg n (Prog decl exec) =
  indent n ++ "Prog\n" ++
  prettyDecl (n+1) decl ++
  prettyExec (n+1) exec

prettyDecl :: Int -> Decl -> String
prettyDecl n decl = case decl of
  DeclComp d1 d2 -> indent n ++ "DeclComp\n" ++ prettyDecl (n+1) d1 ++ prettyDecl (n+1) d2
  DeclInit (DeclVarLast v) t e -> indent n ++ "DeclInit\n" ++ indent (n+1) ++ "DeclVarLast \"" ++ v ++ "\"\n"
                                ++ indent (n+1) ++ show t ++ "\n" ++ prettyExpr (n+1) e
  DeclNonInit (DeclVarLast v) t -> indent n ++ "DeclNonInit\n" ++ indent (n+1) ++ "DeclVarLast \"" ++ v ++ "\"\n"
                                  ++ indent (n+1) ++ show t ++ "\n"
  EmptyDecl -> indent n ++ "EmptyDecl\n"
  _ -> indent n ++ "Decl (complex pattern)\n" 

prettyExec :: Int -> Exec -> String
prettyExec n exec = case exec of
  ExecComp e1 e2 -> indent n ++ "ExecComp\n" ++ prettyExec (n+1) e1 ++ prettyExec (n+1) e2
  Assign v e -> indent n ++ "Assign\n" ++ indent (n+1) ++ show v ++ "\n" ++ prettyExpr (n+1) e
  IfThenElse c t e -> indent n ++ "IfThenElse\n" ++ prettyExpr (n+1) c ++ prettyExec (n+1) t ++ prettyExec (n+1) e
  WhileLoop c b -> indent n ++ "While\n" ++ prettyExpr (n+1) c ++ prettyExec (n+1) b
  PutLine e -> indent n ++ "PutLine\n" ++ prettyExpr (n+1) e
  GetLine v1 v2 -> indent n ++ "GetLine\n" ++ indent (n+1) ++ show v1 ++ "\n" ++ show v2
  DeclBlock decl exec -> indent n ++ "DeclBlock\n" ++ prettyDecl (n+1) decl ++ prettyExec (n+1) exec
  EmptyExec -> indent n ++ "EmptyExec\n"

prettyExpr :: Int -> Exp -> String
prettyExpr n expr = case expr of
  IntLit i -> indent n ++ "IntLit " ++ show i ++ "\n"
  FloatLit f -> indent n ++ "FloatLit " ++ show f ++ "\n"
  Var v -> indent n ++ "Var \"" ++ v ++ "\"\n"
  StringLit s -> indent n ++ "StringLit \"" ++ s ++ "\"\n"
  TrueLit -> indent n ++ "TrueLit\n"
  FalseLit -> indent n ++ "FalseLit\n"
  Add e1 e2 -> indent n ++ "Add\n" ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  Sub e1 e2 -> indent n ++ "Sub\n" ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  Mult e1 e2 -> indent n ++ "Mult\n" ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  Div e1 e2  -> indent n ++ "Div\n"  ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  Pow e1 e2  -> indent n ++ "Pow\n"  ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  And e1 e2  -> indent n ++ "And\n"  ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  Or  e1 e2  -> indent n ++ "Or\n"   ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  XOr e1 e2  -> indent n ++ "XOr\n"  ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  Eq  e1 e2  -> indent n ++ "Eq\n"   ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  Ne  e1 e2  -> indent n ++ "Ne\n"   ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  Lt  e1 e2  -> indent n ++ "Lt\n"   ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  Le  e1 e2  -> indent n ++ "Le\n"   ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2
  Not e      -> indent n ++ "Not\n"  ++ prettyExpr (n+1) e
  Concat e1 e2 -> indent n ++ "Concat\n" ++ prettyExpr (n+1) e1 ++ prettyExpr (n+1) e2

