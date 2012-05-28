{-# LANGUAGE FlexibleInstances #-}

module Lang.Php.Ast.Analysis.Goto(allAnalyses) where

import qualified Data.Intercal as IC

import Text.Parsec.Pos(SourcePos)
import Lang.Php.Ast
import Lang.Php.Ast.Traversal

allAnalyses = [gotoAnalysis]

data Entry = ELabel SourcePos String | EGoto SourcePos String deriving Show

class EntryExtractable a where
    extract :: a -> [Entry]

gotoAnalysis :: AstAnalysis
gotoAnalysis = undefined

instance EntryExtractable a => EntryExtractable (IC.Intercal WS a) where
    extract = concatMap extract . map snd . fst . IC.breakEnd

instance EntryExtractable (Block Stmt) where
    extract (Block x) = extract x

instance EntryExtractable (StoredPos Stmt) where
    extract (StoredPos p (StmtLabel wsc)) = [ELabel p (getLabel wsc)]
    extract (StoredPos p (StmtGoto wsc _)) = [EGoto p (getLabel wsc)]
    extract (StoredPos _ (StmtBlock (Block x))) = extract x
    extract (StoredPos _ (StmtIf x)) = extract x
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

instance EntryExtractable IfBlock where
    extract (IfBlock _ b) = extract b

instance EntryExtractable If where
    extract (If _ bs el) = extract bs ++ extract el
