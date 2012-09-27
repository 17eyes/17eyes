--
-- Copyright (c) 2012 by Tomasz Dudziak, Mateusz Kocielski
-- www.17eyes.com, hello@17eyes.com
--

module Lang.Php.Ast.Analysis(allAstAnalyses) where

import qualified Lang.Php.Ast.Analysis.Func as Func
import qualified Lang.Php.Ast.Analysis.Goto as Goto
import qualified Lang.Php.Ast.Analysis.Style as Style
import qualified Lang.Php.Ast.Analysis.Reachability as Reachability

allAstAnalyses = Style.allAnalyses ++ Goto.allAnalyses ++
  Reachability.allAnalyses ++ Func.allAnalyses
