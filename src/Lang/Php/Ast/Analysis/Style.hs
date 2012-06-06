module Lang.Php.Ast.Analysis.Style(allAnalyses) where

import Data.Char(isSpace)
import Data.Generics.Aliases(mkM)

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
