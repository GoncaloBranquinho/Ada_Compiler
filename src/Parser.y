{
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      main        { MAIN }
      put_line    { PUTL }
      get_line    { GETL }
      integer     { INT }
      boolean     { BOOL }
      float       { FLOAT }
      string      { STRING }
      procedure   { PROC }
      begin       { BEGIN }
      end         { END }
      is          { IS }
      if          { IF }
      then        { THEN }
      else        { ELSE }
      while       { WHILE }
      loop        { LOOP }
      declare     { DECLARE }
      true        { TRUE }
      false       { FALSE }
      and         { AND }
      or          { OR }
      xor         { XOR }
      not         { NOT }
      "="         { EQUAL }
      "/="        { NOT_EQUAL }
      "<"         { LESS }
      "<="        { LESS_EQ }
      ">"         { GREAT }
      ">="        { GREAT_EQ }
      "&"         { CONCAT }
      ":="        { ASSIGN }
      "("         { LPAREN }
      ")"         { RPAREN }
      ":"         { COLON }
      ";"         { SEMI }
      ","         { COMMA }
      "+"         { PLUS }
      "-"         { SUB }
      "**"        { POW }
      "*"         { MULT }
      "/"         { DIV }
      string_lit  { STRING_LITERAL $$ }
      id          { ID $$ }
      integer_lit { INTEGER_LITERAL $$ }
      float_lit   { FLOAT_LITERAL $$ }

%%

Prog  : procedure main is DeclCompStart begin ExecCompStart end main ";" { Prog $4 $6 }

DeclCompStart : DeclComp { $1 }
              |          { EmptyDecl }

DeclComp : Decl ";"          { $1 }
         | DeclComp Decl ";" { DeclComp $1 $2 }

Decl     : DeclNonInit { $1 }
         | DeclInit    { $1 }

DeclNonInit : DeclVar ":" Type { DeclNonInit $1 $3 }

DeclInit : DeclVar ":" Type ":=" Exp { DeclInit $1 $3 $5 }

DeclVar  : id             { DeclVarLast $1 }
         | DeclVar "," id { DeclVarNonLast $1 $3 } 

Type     : integer { TypeInteger }
         | boolean { TypeBoolean }
         | float   { TypeFloat }
         | string  { TypeString }

ExecCompStart : ExecComp { $1 }
              |          { EmptyExec }

ExecComp : Exec ";"          { $1 }
         | ExecComp Exec ";" { ExecComp $1 $2 }

Exec : IfThenElse { $1 }
     | Assign     { $1 }
     | WhileLoop  { $1 }
     | IO         { $1 }
     | DeclBlock  { $1 }

IfThenElse : if Exp then ExecCompStart else ExecCompStart end if { IfThenElse $2 $4 $6 }

Assign : id ":=" Exp { Assign $1 $3 }

WhileLoop : while Exp loop ExecCompStart end loop { WhileLoop $2 $4 }

IO : put_line "(" Exp ")"        { PutLine $3 }
   | get_line "(" id "," id ")"  { GetLine $3 $5 }

DeclBlock : declare DeclCompStart begin ExecCompStart end { DeclBlock $2 $4 }

Exp        : OrExp { $1 }

OrExp      : AndExp           { $1 }
           | OrExp or AndExp  { Or $1 $3 }
           | OrExp xor AndExp { XOr $1 $3 }

AndExp     : RelExp            { $1 }
           | AndExp and RelExp { And $1 $3 }

RelExp     : AddExp             { $1 }
           | AddExp "=" AddExp  { Eq $1 $3 }
           | AddExp ">" AddExp  { Lt $3 $1 }
           | AddExp "<" AddExp  { Lt $1 $3 }
           | AddExp ">=" AddExp { Le $3 $1 }
           | AddExp "<=" AddExp { Le $1 $3 }
           | AddExp "/=" AddExp { Ne $1 $3 }

AddExp     : MultExp            { $1 }
           | AddExp "&" MultExp { Concat $1 $3 }
           | AddExp "+" MultExp { Add $1 $3 }
           | AddExp "-" MultExp { Sub $1 $3 }

MultExp    : PowExp             { $1 }
           | MultExp "*" PowExp { Mult $1 $3 }
           | MultExp "/" PowExp { Div $1 $3 }

PowExp     : UnaryExp             { $1 }
           | UnaryExp "**" PowExp { Pow $1 $3 }

UnaryExp   : not UnaryExp { Not $2 }
           | "+" Factor   { $2 }
           | "-" Factor   { Sub (IntLit 0) $2 }
           | Factor       { $1 }

Factor     : integer_lit               { IntLit $1 }
           | float_lit                 { FloatLit $1 }
           | id                        { Var $1 }
           | string_lit                { StringLit $1 }
           | true                      { TrueLit }
           | false                     { FalseLit }
           | "(" Exp ")"               { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Prog = Prog Decl Exec
    deriving Show

data Decl = DeclInit DeclVar Type Exp
          | DeclNonInit DeclVar Type
          | DeclComp Decl Decl
          | EmptyDecl
    deriving (Show, Eq)

data DeclVar = DeclVarNonLast DeclVar String
             | DeclVarLast String
    deriving (Show, Eq)

data Type = TypeInteger
          | TypeBoolean
          | TypeFloat
          | TypeString
    deriving (Show, Eq)

data Exec = Assign String Exp
          | IfThenElse Exp Exec Exec
          | WhileLoop Exp Exec
          | PutLine Exp
          | GetLine String String
          | ExecComp Exec Exec
          | DeclBlock Decl Exec
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

}
