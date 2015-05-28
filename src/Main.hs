{-
Copyright (c) 2015 17eyes.com

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
-}

import System.Environment(getArgs)
import System.IO
import System.Console.GetOpt
import Data.String.Utils(replace)
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
    Option [] ["resolve"] (ReqArg (\d x -> x { optAction = codebaseResolve d }) "NAME")
           "try to find a function, class, method or constant by name",
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

codebaseResolve :: String -> Options -> IO ()
codebaseResolve name opts = do
    let dir = optCodebaseDir opts
    let cb_name = replace "/" "" dir -- FIXME
    cb <- createCodebase dir cb_name
    updateCodebase cb
    msgs <- concat <$> sequence [findFunctions cb, findMethods cb, findFiles cb] -- TODO: resolve classes etc.
    putStr (intercalate ((take 78 $ repeat '=') ++ "\n") msgs)
 where
   findFunctions cb =
     map (\(lbl, graph) -> "Found function (entry point " ++ show lbl ++ "):\n" ++ cfgToDot graph ++ "\n")
         <$> resolveFunction cb name

   findMethods cb =
     map (\graph -> "Found method:\n" ++ cfgToDot graph ++ "\n")
         <$> resolveMethod cb name

   findFiles cb = do
     map (\(name, graph) -> "Found file " ++ name ++ ":\n" ++ cfgToDot graph ++ "\n")
         <$> resolveFile cb name

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
