module Lang.Php.Ast.Analysis.Style where

import Lang.Php.Ast
import Lang.Php.Ast.Traversal
import Common

mkKind = IssueKind "Lang.Php.Analysis.Style"

dummyAnalysis :: Expr -> TraverseState () Expr
dummyAnalysis x = do
    emitIssue $ Issue {
        issueTitle = "<title>",
        issueMessage = "<message>",
        issueFileName = Nothing,
        issueFunctionName = Nothing,
        issueLineNumber = Nothing,
        issueKind = mkKind "dummy",
        issueSeverity = ISStyle,
        issueConfidence = ICSure,
        issueContext = [] -- FIXME
    }
    return x
