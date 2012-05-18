import System.Environment(getArgs)
import System.IO
import System.Console.GetOpt

import Lang.Php.Ast
import Codebase

data Options = Options {
    optCodebaseDir :: String,
    optInputFile   :: String,
    optAction      :: Options -> IO ()
 }

defaultOptions = Options {
    optCodebaseDir = "www",
    optInputFile   = "www/index.php",
    optAction      = printIncludes
}

options :: [OptDescr (Options -> Options)]
options = [
    Option [] ["dump-ast"] (NoArg $ \x -> x { optAction = dumpAst })
           "just parse from standard input and dump the AST",
    Option [] ["unparse"]  (NoArg $ \x -> x { optAction = parseUnparse })
           "parse from the standard input and reconstruct the source code",
    Option "d" ["codebase"] (ReqArg (\d x -> x { optCodebaseDir = d })
                                    "DIR")
           ("set the codebase directory (instead of the default " ++
            (optCodebaseDir defaultOptions) ++ ")")
 ]

parseString :: String -> String -> IO Ast
parseString name input = case runParser (parse <* eof) () name input of
    (Left err)  -> error (show err ++ "\n")
    (Right ast) -> return ast

(dumpAst, parseUnparse) = (work show, work unparse)
 where work f _ = getContents >>= parseString "<stdin>" >>= putStrLn . f

printIncludes :: Options -> IO ()
printIncludes opts = do
    codebase <- scanCodebase (optCodebaseDir opts)
    putStrLn $ (show $ length $ codebasePaths codebase) ++ " source files"
    -- TODO: implement

main :: IO ()
main = do
    hSetEncoding stdin latin1
    args <- getArgs
    (opts, files) <- case getOpt RequireOrder options args of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    case files of
        [] -> optAction opts opts
        xs -> mapM_ (\f -> optAction opts (opts { optInputFile = f })) xs
  where
    header = "Usage: phpsa [OPTION...] FILES..."
