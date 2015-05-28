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
