--
-- Copyright (c) 2012 by Tomasz Dudziak, Mateusz Kocielski
-- www.17eyes.com, hello@17eyes.com
--

import System.Environment(getArgs)
import System.IO
import System.Console.GetOpt
import Compiler.Hoopl(showGraph)

import Lang.Php.Ast
import Lang.Php.Ast.Traversal
import Lang.Php.Ast.Analysis

import Lang.Php.Cfg.Generation
import Lang.Php.Cfg.Dot

import Codebase
import Pipeline
import qualified Issue

data Options = Options {
    optCodebaseDir :: String,
    optInputFile   :: String,
    optAction      :: Options -> IO ()
 }

defaultOptions = Options {
    optCodebaseDir = "www",
    optInputFile   = "www/index.php",
    optAction      = astAnalyses
}

options :: [OptDescr (Options -> Options)]
options = [
    Option [] ["dot"] (NoArg $ \x -> x { optAction = dumpCfg })
           ("parse from the standard input and output CFG representation "
           ++ "consumable by Graphviz's `dot'."),
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

dumpCfg _ = do
    ast <- parseString "<stdin>" =<< getContents
    let (cfg, _) = runGMonad (toCfg ast)
    putStr (cfgToDot cfg)

astAnalyses :: Options -> IO ()
astAnalyses opts = do
    let fn = optInputFile opts
    issues <- analyzeFile fn <$> readFile fn
    forM_ issues $ \is -> do
        let loc = (maybe "?" id $ Issue.issueFileName is) ++ ":" ++
                  (maybe "?" show $ Issue.issueLineNumber is) ++ " "
        putStrLn (loc ++ Issue.issueTitle is)
        putStrLn (take 78 $ repeat '-')
        putStrLn (Issue.issueMessage is)
        putStrLn ""

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
    header = "Usage: 17eyes [OPTION...] FILES..."
