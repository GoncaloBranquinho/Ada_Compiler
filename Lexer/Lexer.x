{
module Lexer where
import Data.Char
}

%wrapper "basic"

$white = [\ \t\n\r]
$digit = [0-9]
$alpha = [a-zA-Z]

tokens :- 

$white+                     ;
Main                        { \s -> MAIN }
Put_Line                    { \s -> PUTL }
Get_Line                    { \s -> GETL }
Integer                     { \s -> INT }
Boolean                     { \s -> BOOL }
Float                       { \s -> FLOAT }
String                      { \s -> STRING }
procedure                   { \s -> PROC }
begin                       { \s -> BEGIN }
end                         { \s -> END }
is                          { \s -> IS }
if                          { \s -> IF }
then                        { \s -> THEN }
else                        { \s -> ELSE }
while                       { \s -> WHILE }
loop                        { \s -> LOOP }
True                        { \s -> TRUE }
False                       { \s -> FALSE }
null                        { \s -> NULL }
and                         { \s -> AND } 
or                          { \s -> OR }
xor                         { \s -> XOR }
not                         { \s -> NOT }
in                          { \s -> IN }
"="                         { \s -> EQUAL }
"/="                        { \s -> NOT_EQUAL }
"<"                         { \s -> LESS }
"<="                        { \s -> LESS_EQ }
">"                         { \s -> GREAT }
">="                        { \s -> GREAT_EQ }
"&"                         { \s -> CONCAT }
":="                        { \s -> ASSIGN }
"("                         { \s -> LPAREN }
")"                         { \s -> RPAREN }
":"                         { \s -> COLON }
";"                         { \s -> SEMI }
","                         { \s -> COMMA }
"+"                         { \s -> PLUS }
"-"                         { \s -> SUB }
"**"                        { \s -> POW }
"*"                         { \s -> MULT }
"/"                         { \s -> DIV }
abs                         { \s -> ABS }
mod                         { \s -> MOD }
rem                         { \s -> REM }
\"[^\"]*\"                  { \s -> STRING_LITERAL s }
$alpha($alpha|$digit|"_")*  { \s -> ID (map toLower s) }
$digit+"."$digit+           { \s -> FLOAT_LITERAL (read s) }
$digit+                     { \s -> INTEGER_LITERAL (read s) }

{
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
           | TRUE
           | FALSE
           | NULL
           | AND
           | OR
           | XOR
           | NOT
           | IN
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
           | ABS
           | MOD
           | REM
           | STRING_LITERAL String
           | ID String
           | INTEGER_LITERAL Int
           | FLOAT_LITERAL Double
    deriving (Eq, Show)

main = do
    s <- getContents
    print (alexScanTokens s)

}
