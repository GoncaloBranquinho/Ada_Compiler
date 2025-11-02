module Main where 
import Lexer
-- importar o tipos e a função
-- alexScanTokens :: String -> [Token]

main = do 
  txt <- getContents
  print (alexScanTokens txt)
