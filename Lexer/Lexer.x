{
module Lexer where
import Data.Char
}

%wrapper "basic"

$white = [\ \t\n\r]
$digit = [0-9]
$alpha = [a-zA-Z]

tokens :- 

-- Ignorar espaços em branco 
$white+                           ;

-- Keywords
if                                { \s -> IF }
then                              { \s -> THEN }
else                              { \s -> ELSE }
while                             { \s -> WHILE }

-- Operadores booleanos
and                               { \s -> AND } 
or                                { \s -> OR }
xor                               { \s -> XOR }
not                               { \s -> NOT }
in                                { \s -> IN }

-- Operadores relacionais
"/="                              { \s -> NOT_EQUAL }
"="                               { \s -> EQUAL }
"<"                               { \s -> LESS }
"<="                              { \s -> LESS_EQUAL}
">"                               { \s -> GREATER }
">="                              { \s -> GREATER_EQUAL }

-- Concatenação 
"&"                               { \s -> CONCAT }

-- Atribuição
":="                              { \s -> ASSIGN }

-- Parênteses
"("                               { \s -> LPAREN }
")"                               { \s -> RPAREN }

-- Operadores aritmétricos
"+"                               { \s -> PLUS }
"-"                               { \s -> SUB }
"**"                              { \s -> POW }
"*"                               { \s -> MULT }
"/"                               { \s -> DIV }
abs                               { \s -> ABS }
mod                               { \s -> MOD }
rem                               { \s -> REM }

-- Identificadores
$alpha($alpha|$digit|"_")*        { \s -> ID (map toLower s) }
$digit+                           { \s -> NUM (read s) }

-- Números
$digit+"."$digit+                 { \s -> REAL (read s) }


{
data Token = IF
           | THEN
           | ELSE
           | WHILE
           | AND
           | OR
           | XOR
           | NOT
           | IN
           | EQUAL
           | NOT_EQUAL
           | LESS
           | LESS_EQUAL
           | GREATER
           | GREATER_EQUAL
           | CONCAT
           | ASSIGN
           | LPAREN
           | RPAREN
           | PLUS
           | SUB
           | POW
           | MULT
           | DIV
           | ABS
           | MOD 
           | REM
           | ID String
           | NUM Int
           | REAL Double
    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTokens s)

}


