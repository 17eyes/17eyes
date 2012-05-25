module Lang.Php.Ast.Analysis.Style(allAnalyses) where

import Data.Char(isSpace)

import qualified Data.Intercal as IC
import Lang.Php.Ast
import Lang.Php.Ast.Traversal
import Common

allAnalyses = [finishPhp]

mkKind = IssueKind "Lang.Php.Analysis.Style"

finishPhp :: AstAnalysis
finishPhp = AstAnalysis () $ \ast@(Ast _ _ sl) -> do
    let stmts = map snd $ fst $ IC.breakEnd sl
    case reverse stmts of
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
