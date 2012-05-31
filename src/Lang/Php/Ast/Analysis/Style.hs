module Lang.Php.Ast.Analysis.Style(allAnalyses) where

import Data.Char(isSpace)
import Data.Generics.Aliases(mkM)

import qualified Data.Intercal as IC
import Lang.Php.Ast
import Lang.Php.Ast.Traversal
import Common

allAnalyses = [
      finishPhp
    , styleIncludeRequire
    , stringLiterals
    , functionCalls
    , classDeclaration
    , functionDeclaration
    , shortTags
 ]

mkKind = IssueKind "Lang.Php.Analysis.Style"

finishPhp :: AstAnalysis
finishPhp = AstAnalysis () $ \ast@(Ast _ _ sl) -> do
    case reverse (IC.toList2 sl) of
        (StoredPos pos (StmtNothing (StmtEndClose (TopLevel text Nothing)))):_ ->
             if all isSpace text then emit pos else return ()
        _ -> return ()
    return ast
 where
    -- TODO: different warning if it's not followed by anything?
    msg = "This file ends with a closing PHP tag (like \"?>\"). " ++
          "If such tag is followed by white space at the end of a file " ++
          "this can caused HTTP headers to be sent sooner than expected."

    emit pos = emitIssue $ Issue {
        issueTitle = "PHP closing tag at the end of a file",
        issueMessage = msg,
        issueFileName = Just (sourceName pos),
        issueFunctionName = Nothing,
        issueLineNumber = Just (sourceLine pos),
        issueKind = mkKind "finishPhp",
        issueSeverity = ISStyle,
        issueConfidence = ICSure,
        issueContext = [] -- this issue is file-unique
    }

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
    top_lvl_inc pos x = emitIssue $ Issue {
        issueTitle = "top-level includes should be done with require_once",
        issueMessage = msg_top_lvl,
        issueFileName = Just (sourceName pos),
        issueFunctionName = Nothing,
        issueLineNumber = Just (sourceLine pos),
        issueKind = mkKind "styleIncludeRequire/top_lvl",
        issueSeverity = ISStyle,
        issueConfidence = ICSure,
        issueContext = [unparse x]
    }

    deep_inc (ExprInclude Inc Once _ _) = return ()
    deep_inc x = emitIssue $ Issue {
        issueTitle = "non-top-level includes should be done with include_once",
        issueMessage = msg_deep,
        issueFileName = Nothing, -- filled by emitIssue
        issueFunctionName = Nothing,
        issueLineNumber = Nothing, -- filled by emitIssue
        issueKind = mkKind "styleIncludeRequire/deep",
        issueSeverity = ISStyle,
        issueConfidence = ICSure,
        issueContext = [unparse x]
    }

    msg_top_lvl =
        "Including files with require_once prevents the script from further " ++
        "execution if the inclusion fails. It also disallows including the " ++
        "same file multiple times. This is usually the desired behavior for " ++
        "top-level include statements."

    msg_deep =
        "Including files with include_once disallows including the same " ++
        "file many times but still allows the script to continue if the " ++
        "inclusion fails. This is usually the desired behavior of include " ++
        "statements inside functions and control structures (e.g. those " ++
        "used to load plugins)."

stringLiterals :: AstAnalysis
stringLiterals = AstAnalysis () $ \lit@(StrLit x) -> do
    case x of
        ('"':xs) -> if trivial xs then emit x else return ()
        _        -> return ()
    return lit
 where
    emit x = emitIssue $ Issue {
        issueTitle = "simple string literals should be single-quoted",
        issueMessage = msg,
        issueFileName = Nothing, -- filled by emitIssue
        issueFunctionName = Nothing,
        issueLineNumber = Nothing, -- filled by emitIssue
        issueKind = mkKind "stringLiterals",
        issueSeverity = ISStyle,
        issueConfidence = ICSure,
        issueContext = [x]
    }

    trivial [] = True
    trivial ('\\':'$':xs) = trivial xs
    trivial ('\\':'\\':xs) = trivial xs
    trivial ('\\':'"':xs) = trivial xs
    trivial ('\\':_:_) = False
    trivial ('$':_) = False
    trivial (_:xs) = trivial xs

    msg = "If you don't use variables or special escapes inside string "
        ++ "literals, it might be better to put them in single quotes ('...'). "
        ++ "This helps avoiding unintended expansion of variables inside "
        ++ "string literals."

functionCalls :: AstAnalysis
functionCalls = AstAnalysis () analysis
 where
    analysis x@(ROnlyValFunc _ [] _) = return x
    analysis x@(ROnlyValFunc _ _ _) = (emitIssue $ Issue {
        issueTitle = "there should be no space before parenthesis in function calls",
        issueMessage = msg,
        issueFileName = Nothing, -- filled by emitIssue
        issueFunctionName = Nothing,
        issueLineNumber = Nothing, -- filled by emitIssue
        issueKind = mkKind "functionCalls",
        issueSeverity = ISStyle,
        issueConfidence = ICSure,
        issueContext = [unparse x]
    }) >> return x
    analysis x = return x

    msg = "The usual convention is to put no whitespace between the function "
       ++ "name and the opening parenthesis in a function call. This helps "
       ++ "visually distinguish function calls from statements like \"if\" "
       ++ "or \"while\"."

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
        if has_nl then return () else emitIssue $ Issue {
            issueTitle = f_or_c ++ " declarations should have their opening brace on a new line",
            issueMessage = msg,
            issueFileName = Nothing, -- filled by emitIssue
            issueFunctionName = Nothing,
            issueLineNumber = Nothing, -- filled by emitIssue
            issueKind = mkKind $ f_or_c ++ "Declaration",
            issueSeverity = ISStyle,
            issueConfidence = ICSure,
            issueContext = ctx
        }

    msg = "By convention the opening brace after class and function declarations "
       ++ "is usually in the next line. This serves as a visual aid to "
       ++ "distinguish these declarations from statements like \"if\" or "
       ++ "\"switch\"."

shortTags :: AstAnalysis
shortTags = AstAnalysis () analysis
 where
    analysis x@(TopLevel _ (Just (Right opentag))) = do
      if opentag == "php" then return x else (emitIssue $ Issue {
          issueTitle = "short tags might be not supported by the interpreter",
          issueMessage = msg,
          issueFileName = Nothing, -- filled by emitIssue
          issueFunctionName = Nothing,
          issueLineNumber = Nothing, -- filled by emitIssue
          issueKind = mkKind "shortTags",
          issueSeverity = ISStyle,
          issueConfidence = ICSure,
          issueContext = [unparse x]
      }) >> return x

    analysis x = return x

    -- FIXME: something better?
    msg = "Short tags <? and <?= might be not supported by the PHP "
       ++ "interpreter, thus using short tags on some servers may lead to "
       ++ "information leak. It's recommended to use long tags, since they're "
       ++ "supported by all PHP versions."
