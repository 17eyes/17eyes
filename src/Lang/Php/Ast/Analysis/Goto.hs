{-# LANGUAGE FlexibleInstances #-}

module Lang.Php.Ast.Analysis.Goto(allAnalyses) where

import qualified Data.Intercal as IC
import Control.Monad.State(get,put,modify)

import Lang.Php.Ast hiding (get,put)
import Lang.Php.Ast.Traversal

allAnalyses = [
      backwardGotoAnalysisFunc
    , backwardGotoAnalysisTL
    , labelAnalysisFunc
    , labelAnalysisTL
 ]

data Entry = ELabel SourcePos String
           | EGoto SourcePos String
           | ELoop [Entry]
    deriving Show

mkKind = IssueKind "Lang.Php.Analysis.Goto"

-- detecting backward goto (loop-like)

backwardGotoAnalysisTL :: AstAnalysis
backwardGotoAnalysisTL = AstAnalysis [] $ \ast@(Ast _ _ st) -> do
    detectBackwardGoto (extract st)
    return ast

backwardGotoAnalysisFunc :: AstAnalysis
backwardGotoAnalysisFunc = AstAnalysis [] $ \func@(Func _ _ _ _ block) -> do
    detectBackwardGoto (extract block)
    return func

detectBackwardGoto :: [Entry] -> TraverseState [String] ()
detectBackwardGoto = mapM_ $ \entry -> case entry of
    (ELabel _ lab) -> modify (lab:)
    (ELoop xs) -> detectBackwardGoto xs
    (EGoto pos lab) -> do
        past <- get
        if lab `elem` past
          then emitIssue $ Issue {
            issueTitle = "goto instead of structural loop",
            issueMessage = msg,
            issueFileName = Just (sourceName pos),
            issueFunctionName = Nothing, -- FIXME
            issueLineNumber = Just (sourceLine pos),
            issueKind = mkKind "detectBackwardGoto",
            issueSeverity = ISStyle,
            issueConfidence = ICLikely,
            issueContext = [lab]
          }
          else return ()
 where
    msg = "This goto statement jumps backwards. Although this is correct, "
       ++ "such control flow may sometimes be not very obvious and would "
       ++ "be better expressed as a loop (while or do-while)."

-- check for missing or redundant goto labels

labelAnalysisTL :: AstAnalysis
labelAnalysisTL = AstAnalysis () $ \ast@(Ast _ _ st) -> do
    detectBadLabels (extract st)
    return ast

labelAnalysisFunc :: AstAnalysis
labelAnalysisFunc = AstAnalysis () $ \func@(Func _ _ _ _ block) -> do
    detectBadLabels (extract block)
    return func

detectBadLabels :: [Entry] -> TraverseState () ()
detectBadLabels es = do
    let gotos = allGotos es
    let labs = allLabels es
    let missing_lab = filter (\(x,_) -> maybe True (const False) $ lookup x labs) gotos
    let redundant_lab = filter (\(x,_) -> maybe True (const False) $ lookup x gotos) labs
    forM_ missing_lab $ \(lab,pos) -> emitIssue $ Issue {
        issueTitle = "goto to a missing label " ++ show lab,
        issueMessage = msg_missing,
        issueFileName = Just (sourceName pos),
        issueFunctionName = Nothing, -- FIXME
        issueLineNumber = Just (sourceLine pos),
        issueKind = mkKind "detectBadLabels/missing",
        issueSeverity = ISCritical,
        issueConfidence = ICSure,
        issueContext = [lab]
    }
    forM_ redundant_lab $ \(lab,pos) -> emitIssue $ Issue {
        issueTitle = "unused label " ++ show lab,
        issueMessage = msg_redundant,
        issueFileName = Just (sourceName pos),
        issueFunctionName = Nothing, -- FIXME
        issueLineNumber = Just (sourceLine pos),
        issueKind = mkKind "detectBadLabels/redundant",
        issueSeverity = ISStyle,
        issueConfidence = ICSure,
        issueContext = [lab]
    }
  where
    msg_redundant = "This label was declared but there is not goto statement "
                 ++ "referencing it. This may indicate a possible problem "
                 ++ "(a typo maybe?)."
    msg_missing = "This goto statement references a nonexistent label. This "
                 ++ "will trigger a runtime error when executed."

allGotos :: [Entry] -> [(String, SourcePos)]
allGotos = concatMap $ \x -> case x of
    (EGoto pos lab) -> [(lab,pos)]
    (ELoop xs) -> allGotos xs
    _ -> []

allLabels :: [Entry] -> [(String, SourcePos)]
allLabels = concatMap $ \x -> case x of
    (ELabel pos lab) -> [(lab,pos)]
    (ELoop xs) -> allGotos xs
    _ -> []

-- generating [Entry] from the AST of a single function (or function-like block)

class EntryExtractable a where
    extract :: a -> [Entry]

instance EntryExtractable a => EntryExtractable (IC.Intercal WS a) where
    extract = concatMap extract . map snd . fst . IC.breakEnd

instance EntryExtractable a => EntryExtractable (IC.Intercal a WS) where
    extract = concatMap extract . map fst . fst . IC.breakEnd

instance EntryExtractable a => EntryExtractable (WSCap a) where
    extract = extract . wsCapMain

instance EntryExtractable (Block Stmt) where
    extract (Block x) = extract x

instance EntryExtractable (StoredPos Stmt) where
    extract (StoredPos p (StmtLabel wsc)) = [ELabel p (getLabel wsc)]
    extract (StoredPos p (StmtGoto wsc _)) = [EGoto p (getLabel wsc)]
    extract (StoredPos _ (StmtBlock (Block x))) = extract x
    extract (StoredPos _ (StmtIf x)) = extract x
    extract (StoredPos _ (StmtSwitch s)) = extract s
    extract (StoredPos _ (StmtTry wbl catches)) = extract wbl ++ extract catches
    extract (StoredPos _ (StmtNamespace n)) = extract n
    extract (StoredPos _ (StmtDoWhile x)) = extract x
    extract (StoredPos _ (StmtFor x)) = extract x
    extract (StoredPos _ (StmtForeach x)) = extract x
    extract (StoredPos _ (StmtWhile x)) = extract x
    extract _ = []

getLabel :: WSCap Label -> String
getLabel wsc = case wsCapMain wsc of
    Label x -> x

instance (EntryExtractable a, EntryExtractable b) => EntryExtractable (Either a b) where
    extract (Left x) = extract x
    extract (Right x) = extract x

instance EntryExtractable a => EntryExtractable [a] where
    extract = concatMap extract

instance EntryExtractable a => EntryExtractable (Maybe a) where
    extract Nothing = []
    extract (Just x) = extract x

-- non-looping control flow statments: if, switch, try..catch

instance EntryExtractable IfBlock where
    extract (IfBlock _ b) = extract b

instance EntryExtractable If where
    extract (If _ bs el) = extract bs ++ extract el

instance EntryExtractable Switch where
    extract (Switch _ _ _ _ cases) = do
        (Case _ sl) <- cases
        extract sl

instance EntryExtractable Catch where
    extract (Catch _ _ block) = extract block

instance EntryExtractable Namespace where
    extract (Namespace _ b) = extract b

-- loops: dowhile for, foreach, and while

instance EntryExtractable DoWhile where
    extract (DoWhile block _ _) = [ELoop (extract block)]

instance EntryExtractable For where
    extract (For _ block _) = [ELoop (extract block)]

instance EntryExtractable Foreach where
    extract (Foreach _ block _) = [ELoop (extract block)]

instance EntryExtractable While where
    extract (While _ block _) = [ELoop (extract block)]
