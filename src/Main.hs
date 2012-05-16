-- Dummy program that parses standard input and dumps the AST.

import System.IO
import Lang.Php.Ast

main :: IO ()
main = do
    hSetEncoding stdin latin1
    input <- getContents
    case runParser parse () "<stdin>" input :: Either ParseError Ast of
        (Left err) -> error (show err ++ "\n")
        (Right ast) -> putStrLn $ show ast
