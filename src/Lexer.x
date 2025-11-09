{
module Lexer where
import Data.Char
}

%wrapper "basic"

$white = [\ \t\n\r]
$digit = [0-9]
$alpha = [a-zA-Z]

tokens :- 

$white+                                    ;
main                                       { \s -> MAIN }
put_line                                   { \s -> PUTL }
get_line                                   { \s -> GETL }
integer                                    { \s -> INT }
boolean                                    { \s -> BOOL }
float                                      { \s -> FLOAT }
string                                     { \s -> STRING }
procedure                                  { \s -> PROC }
begin                                      { \s -> BEGIN }
end                                        { \s -> END }
is                                         { \s -> IS }
if                                         { \s -> IF }
then                                       { \s -> THEN }
else                                       { \s -> ELSE }
while                                      { \s -> WHILE }
loop                                       { \s -> LOOP }
declare                                    { \s -> DECLARE }
true                                       { \s -> TRUE }
false                                      { \s -> FALSE }
and                                        { \s -> AND } 
or                                         { \s -> OR }
xor                                        { \s -> XOR }
not                                        { \s -> NOT }
"="                                        { \s -> EQUAL }
"/="                                       { \s -> NOT_EQUAL }
"<"                                        { \s -> LESS }
"<="                                       { \s -> LESS_EQ }
">"                                        { \s -> GREAT }
">="                                       { \s -> GREAT_EQ }
"&"                                        { \s -> CONCAT }
":="                                       { \s -> ASSIGN }
"("                                        { \s -> LPAREN }
")"                                        { \s -> RPAREN }
":"                                        { \s -> COLON }
";"                                        { \s -> SEMI }
","                                        { \s -> COMMA }
"+"                                        { \s -> PLUS }
"-"                                        { \s -> SUB }
"**"                                       { \s -> POW }
"*"                                        { \s -> MULT }
"/"                                        { \s -> DIV }
\"[^\"]*\"                                 { \s -> STRING_LITERAL (removerAspas s) }
$alpha($alpha|$digit|"_"($alpha|$digit))*  { \s -> ID s }
$digit+"."$digit+                          { \s -> FLOAT_LITERAL (read s) }
$digit+                                    { \s -> INTEGER_LITERAL (read s) }

{

removerAspas :: String -> String
removerAspas ('"':[]) = []
removerAspas ('"':xs) = removerAspas xs
removerAspas (x:xs) = x:removerAspas xs

data Token = MAIN
           | PUTL
           | GETL
           | INT
           | BOOL
           | FLOAT
           | STRING
           | PROC
           | BEGIN
           | END
           | IS
           | IF
           | THEN
           | ELSE
           | WHILE
           | LOOP
           | DECLARE
           | TRUE
           | FALSE
           | AND
           | OR
           | XOR
           | NOT
           | EQUAL
           | NOT_EQUAL
           | LESS
           | LESS_EQ
           | GREAT
           | GREAT_EQ
           | CONCAT
           | ASSIGN
           | LPAREN
           | RPAREN
           | COLON
           | SEMI
           | COMMA
           | PLUS
           | SUB
           | POW
           | MULT
           | DIV
           | STRING_LITERAL String
           | ID String
           | INTEGER_LITERAL Int
           | FLOAT_LITERAL Double
    deriving (Eq, Show)

alexScanTokensInsensitive :: String -> [Token]
alexScanTokensInsensitive = alexScanTokens . map toLower

}
