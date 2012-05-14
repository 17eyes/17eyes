-- Dummy program that parses standard input and dumps the AST.

import Lang.Php.Ast

main :: IO ()
main = interact $ show . (runParser parse () "<stdin>" :: String -> Either ParseError Ast)
