-- | This module specifies an overall information flow of the analyzer. All
-- analyses have to be invoked somewhere here, if they are to be executed.
module Pipeline(analyzeFile) where

import Text.Parsec.Error

import Lang.Php.Ast
import Lang.Php.Ast.Traversal
import Lang.Php.Ast.Analysis
import qualified CharAnalysis
import Common

import qualified Kinds
import qualified Issue
import Issue(Issue(..))

-- | Perform all file-level analyses.
analyzeFile :: FilePath -> String -> [Issue]
analyzeFile file_name source =
    (CharAnalysis.runAnalyses file_name source) ++
    case parseOnly file_name source of
        (Left xs) -> xs
        (Right ast) -> astAnalyses ast

parseOnly :: FilePath -> String -> Either [Issue] Ast
parseOnly file_name source =
    case runParser (parse :: Parser Ast) () file_name source of
        (Right ast) -> Right ast
        (Left err) -> Left $ [Issue Kinds.parseError
                                    (Just file_name)
                                    (Just (sourceLine $ errorPos err))
                                    (map messageString (errorMessages err))
                                    (show err)]

astAnalyses :: Ast -> [Issue]
astAnalyses ast = concatMap (runAstAnalysis ast) allAstAnalyses
