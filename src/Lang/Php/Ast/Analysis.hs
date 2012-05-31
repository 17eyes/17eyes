module Lang.Php.Ast.Analysis(allAstAnalyses) where

import qualified Lang.Php.Ast.Analysis.Style as Style
import qualified Lang.Php.Ast.Analysis.Goto as Goto
import qualified Lang.Php.Ast.Analysis.Reachability as Reachability

allAstAnalyses = Style.allAnalyses ++ Goto.allAnalyses ++ Reachability.allAnalyses
