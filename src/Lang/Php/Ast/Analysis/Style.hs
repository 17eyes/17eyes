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

module Lang.Php.Ast.Analysis.Style(allAnalyses) where

import Data.Char(isSpace)
import Data.Generics.Aliases(mkM)
import Data.Either(partitionEithers)

import qualified Data.Intercal as IC
import Lang.Php.Ast
import Lang.Php.Ast.Traversal
import Common
import qualified Kinds

allAnalyses = [
      finishPhp
    , styleIncludeRequire
    , stringLiterals
    , functionCalls
    , classDeclaration
    , functionDeclaration
    , shortTags
    , controlStructs
 ]

finishPhp :: AstAnalysis
finishPhp = AstAnalysis () $ \ast@(Ast _ _ sl) -> do
    case reverse (IC.toList2 sl) of
        (StoredPos pos (StmtNothing (StmtEndClose (TopLevel text Nothing)))):_ ->
             if all isSpace text
               then withSourcePos pos $ emitIssue Kinds.styleFinishPhp [] ()
               else return ()
        _ -> return ()
    return ast

styleIncludeRequire :: AstAnalysis
styleIncludeRequire = AstAnalysis () $ \ast@(Ast _ _ sl) -> do
    mapM_ top_lvl_stmt (IC.toList2 sl)
    return ast
 where
    top_lvl_stmt (StoredPos pos (StmtExpr (inc@(ExprInclude _ _ _ _)) _ _)) = top_lvl_inc pos inc
    top_lvl_stmt x = traverse (mkM deep_expr) x >> return ()

    deep_expr :: Expr -> TraverseState () Expr
    deep_expr (inc@(ExprInclude _ _ _ _)) = deep_inc inc >> return inc
    deep_expr x = return x

    top_lvl_inc pos (ExprInclude Req Once _ _) = return ()
    top_lvl_inc pos x =
        withSourcePos pos $ emitIssue Kinds.styleIncludeTopLevel [unparse x] ()

    deep_inc (ExprInclude Inc Once _ _) = return ()
    deep_inc x = emitIssue Kinds.styleIncludeDeep [unparse x] ()

stringLiterals :: AstAnalysis
stringLiterals = AstAnalysis () $ \lit -> do
    case strLitAsSimple lit of
        Just ('"':xs) -> if trivial xs then emit ('"':xs) else return ()
        _             -> return ()
    return lit
 where
    emit x = emitIssue Kinds.styleStringLiterals [x] ()

    trivial [] = True
    trivial ('\\':'$':xs) = trivial xs
    trivial ('\\':'\\':xs) = trivial xs
    trivial ('\\':'"':xs) = trivial xs
    trivial ('\\':_:_) = False
    trivial ('$':_) = False
    trivial (_:xs) = trivial xs

functionCalls :: AstAnalysis
functionCalls = AstAnalysis () analysis
 where
    analysis x@(ROnlyValFunc _ [] _) = return x
    analysis x@(ROnlyValFunc _ _ _) =
        (emitIssue Kinds.styleFunctionCalls [unparse x] ()) >> return x
    analysis x = return x

classDeclaration :: AstAnalysis
functionDeclaration :: AstAnalysis
(classDeclaration, functionDeclaration) = (AstAnalysis () aClass, AstAnalysis () aFunc)
 where
    aClass stmt@(StmtClass cls) =
        (check (wsCapPost $ className cls) "class"
                                           [wsCapMain $ className cls])
        >> return stmt
    aClass x = return x

    aFunc :: Stmt -> TraverseState () Stmt
    aFunc stmt@(StmtFuncDef func) =
        (check (wsCapPost $ funcArgs func) "function"
                                           [show $ funcName func])
        >> return stmt
    aFunc x = return x

    check ws f_or_c ctx = do
        let has_nl = any (elem '\n') [xs | (WS xs) <- ws]
        if has_nl then return () else emitIssue Kinds.styleClassFunctionDeclaration ctx ()

shortTags :: AstAnalysis
shortTags = AstAnalysis () analysis
 where
    analysis x@(TopLevel _ (Just (Right opentag))) = do
      if opentag == "php"
        then return x
        else (emitIssue Kinds.styleShortTags [unparse x] ()) >> return x

    analysis x = return x

-------------------------------------------------------------------------------
-- XXXSHM: this analysis should be somehow ,,adaptive'' instead of forcing THE
--         GREAT CODING STYLE!
-------------------------------------------------------------------------------

controlStructs :: AstAnalysis
controlStructs = AstAnalysis () analysis
 where
    analysis :: Stmt -> TraverseState () Stmt

    -- while - blocks vs. stmts, ws
    analysis x@(StmtWhile (While (WSCap _ _ ws) block _)) =
      (checkBlock x $ wsCapMain block) >>
      checkWS x ws

    -- for - blocks vs. stmts, ws
    analysis x@(StmtFor(For (WSCap _ _ ws) block _)) =
      (checkBlock x $ wsCapMain block) >>
      checkWS x ws

    -- foreach - blocks vs. stmts, ws
    analysis x@(StmtForeach(Foreach (WSCap _ _ ws) block _)) =
      (checkBlock x $ wsCapMain block) >>
      checkWS x ws

    -- do-while - blocks vs. stmts, ws
    analysis x@(StmtDoWhile(DoWhile (WSCap _ block ws) _ _)) =
      checkBlock x block >>
      checkWS x ws

    -- if - blocks vs. stmts, ws
    analysis x@(StmtIf (If _ blocks elseBlock)) =
      -- block
      mapM (\(IfBlock _ _ (block, _)) -> checkBlock x $ wsCapMain block)
        blocks >>
      maybe (return x) (\(block, _) -> checkBlock x $ wsCapMain block)
        elseBlock >>
      -- ws
      mapM (\(IfBlock _ ws _) -> checkWS x ws) blocks >>
      maybe (return x) (\(WSCap ws _ _, _) -> checkWS x ws) elseBlock

    -- switch - check for the default case, ws
    analysis x@(StmtSwitch (Switch _ (WSCap _ _ ws) _ _ cases)) =
      checkWS x ws >>
      if null (fst $ partitionEithers (map (\(StoredPos _ (Case c _)) -> c)
        cases))
        then (emitIssue Kinds.styleControlStructsSwitchDefault [unparse x] ())
          >> return x
        else return x

    analysis x = return x

    -- checks if block is a real block or just a statement (which should be
    -- considered harmful (tm) in the control structs
    checkBlock x block =
      case block of
        Left _ -> (emitIssue Kinds.styleControlStructs [unparse block] ()) >>
          return x
        Right _ -> return x

    -- checks if there's at least one character between parens and block.
    checkWS x ws =
        if ws /= [] then return x else (emitIssue Kinds.styleControlStructsWS
          [unparse x] ()) >> return x
