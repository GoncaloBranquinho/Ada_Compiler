{
module Main where
}

%name ada
%tokentype { Token }
%error { parseError }

%token
      Main        { MAIN }
      Put_Line    { PUTL }
      Get_Line    { GETL }
      Integer     { INT }
      Boolean     { BOOL }
      Float       { FLOAT }
      String      { STRING }
      procedure   { PROC }
      begin       { BEGIN }
      end         { END }
      is          { IS }
      if          { IF }
      then        { THEN }
      else        { ELSE }
      while       { WHILE }
      loop        { LOOP }
      True        { TRUE }
      False       { FALSE }
      null        { NULL }
      and         { AND }
      or          { OR }
      xor         { XOR }
      not         { NOT }
      in          { IN }
      '='         { EQUAL }
      '/='        { NOT_EQUAL }
      '<'         { LESS }
      '<='        { LESS_EQ }
      '>'         { GREAT }
      '>='        { GREAT_EQ }
      '&'         { CONCAT }
      ':='        { ASSIGN }
      '('         { LPAREN }
      ')'         { RPAREN }
      ':'         { COLON }
      ';'         { SEMI }
      ','         { COMMA }
      '+'         { PLUS }
      '-'         { SUB }
      '**'        { POW }
      '*'         { MULT }
      '/'         { DIV }
      abs         { ABS }
      mod         { MOD }
      rem         { REM }
      str         { STRING_LITERAL $$ }
      id          { ID $$ }
      int         { INTEGER_LITERAL $$ }
      float       { FLOAT_LITERAL $$ }

%%

Exp    : Exp1                    { Exp1 $1 }

Exp1   : Exp1 '+' Term           { Plus $1 $3 }
       | Exp1 '-' Term           { Minus $1 $3 }
       | Term                    { Term $1 }

Term   : Term '*' Factor         { Times $1 $3 }
       | Term '/' Factor         { Div $1 $3 }
       | Factor                  { Factor $1 }

Factor : int                     { Int $1 }
       | var                     { Var $1 }
       | '(' Exp ')'             { Brack $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
      = Let String Exp Exp
      | Exp1 Exp1
      deriving Show

data Exp1
      = Plus Exp1 Term
      | Minus Exp1 Term
      | Term Term
      deriving Show

data Term
      = Times Term Factor
      | Div Term Factor
      | Factor Factor
      deriving Show

data Factor
      = Int Int
      | Var String
      | Brack Exp
      deriving Show

}
