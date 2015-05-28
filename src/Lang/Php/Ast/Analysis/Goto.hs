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

{-# LANGUAGE FlexibleInstances #-}

module Lang.Php.Ast.Analysis.Goto(allAnalyses) where

import qualified Data.Intercal as IC
import Control.Monad.State(get,put,modify)

import Lang.Php.Ast hiding (get,put)
import Lang.Php.Ast.Traversal

import qualified Kinds
import qualified Issue

allAnalyses = mkAnalyses detectBackwardGoto []
           ++ mkAnalyses detectBadLabels ()
           ++ mkAnalyses detectLoopJump ()

type GotoAnalysis st = [Entry] -> TraverseState st ()

data Entry = ELabel SourcePos String
           | EGoto SourcePos String
           | ELoop [Entry]
    deriving Show

mkAnalyses :: GotoAnalysis st -> st -> [AstAnalysis]
mkAnalyses f init = [top_level, func]
 where
    top_level = AstAnalysis init $ \ast@(Ast _ _ st) -> do
        f (extract st)
        return ast

    func = AstAnalysis init $ \func@(Func _ _ _ _ _ block) -> do
        f (extract block)
        return func

-- detecting backward goto (loop-like)

detectBackwardGoto :: [Entry] -> TraverseState [String] ()
detectBackwardGoto = mapM_ $ \entry -> case entry of
    (ELabel _ lab) -> modify (lab:)
    (ELoop xs) -> detectBackwardGoto xs
    (EGoto pos lab) -> do
        past <- get
        if lab `elem` past
          then withSourcePos pos $ emitIssue Kinds.backwardGoto [lab] ()
          else return ()

-- check for missing or redundant goto labels

detectBadLabels :: [Entry] -> TraverseState () ()
detectBadLabels es = do
    let gotos = allGotos es
    let labs = allLabels es
    let missing_lab = filter (\(x,_) -> maybe True (const False) $ lookup x labs) gotos
    let redundant_lab = filter (\(x,_) -> maybe True (const False) $ lookup x gotos) labs
    forM_ missing_lab $ \(lab,pos) ->
        withSourcePos pos $ emitIssue Kinds.badLabelsMissing [lab] lab
    forM_ redundant_lab $ \(lab,pos) ->
        withSourcePos pos $ emitIssue Kinds.badLabelsRedundant [lab] lab

allGotos :: [Entry] -> [(String, SourcePos)]
allGotos = concatMap $ \x -> case x of
    (EGoto pos lab) -> [(lab,pos)]
    (ELoop xs) -> allGotos xs
    _ -> []

allLabels :: [Entry] -> [(String, SourcePos)]
allLabels = concatMap $ \x -> case x of
    (ELabel pos lab) -> [(lab,pos)]
    (ELoop xs) -> allLabels xs
    _ -> []

-- warn about gotos to an inside of a loop

detectLoopJump :: [Entry] -> TraverseState () ()
detectLoopJump = mapM_ (check []) . window
 where
    check :: [String] -> ([Entry], Entry, [Entry]) -> TraverseState () ()
    check ctx (pre, EGoto pos lab, post) = do
        if lab `elem` (ctx ++ inLoops pre ++ inLoops post)
         then withSourcePos pos $ emitIssue Kinds.loopGoto [lab] ()
         else return ()
    check ctx (pre, ELabel _ _, post) = return ()
    check ctx (pre, ELoop xs, post) = mapM_ (check (ctx ++ inLoops pre ++ inLoops post)) $ window xs

inLoops :: [Entry] -> [String]
inLoops = concatMap $ \x -> case x of
    (ELoop x) -> allLabels x
    _ -> []
 where
    allLabels ((ELabel _ x):xs) = x:(allLabels xs)
    allLabels ((EGoto _ _):xs) = allLabels xs
    allLabels ((ELoop xs):xs') = allLabels xs ++ allLabels xs'
    allLabels [] = []

window :: [a] -> [([a],a,[a])]
window [] = []
window (x:xs) = ([],x,xs):[((x:pre),y,post) | (pre,y,post) <- window xs]

-- generating [Entry] from the AST of a single function (or function-like block)

class EntryExtractable a where
    extract :: a -> [Entry]

instance EntryExtractable a => EntryExtractable (IC.Intercal WS a) where
    extract = concatMap extract . IC.toList2

instance EntryExtractable a => EntryExtractable (IC.Intercal a WS) where
    extract = concatMap extract . IC.toList1

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
    extract (IfBlock _ _ (b, _)) = extract b

instance EntryExtractable If where
    extract (If _ bs Nothing) = extract bs
    extract (If _ bs (Just (el, _))) = extract bs ++ extract el

instance EntryExtractable Switch where
    extract (Switch _ _ _ _ cases) = do
        (StoredPos _ (Case _ sl)) <- cases
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
