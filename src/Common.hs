{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Common (
  module Text.ParserCombinators.Parsec,
  Oper, Parse(..), Unparse(..)) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Text.ParserCombinators.Parsec hiding (State, parse, choice)
import Text.ParserCombinators.Parsec.Expr

-- parsec 2 vs 3 stuff

type Oper a = Operator Char () a

class Parse a where
  parse :: Parser a

class Unparse a where
  unparse :: a -> String

instance (Parse a) => Parse [a] where
  parse = many parse

instance (Parse a, Parse b) => Parse (Either a b) where
  parse = Left <$> parse <|> Right <$> parse

instance (Unparse a, Unparse b) => Unparse (Either a b) where
  unparse (Left a) = unparse a
  unparse (Right a) = unparse a

instance (Unparse a, Unparse b) => Unparse (a, b) where
  unparse (a, b) = unparse a ++ unparse b

instance (Unparse a) => Unparse [a] where
  unparse = concatMap unparse

instance (Unparse a) => Unparse (Maybe a) where
  unparse = maybe "" unparse

-- issues, severities, issue classes, etc.

data IssueKind = IssueKind {
    issueKindAnalysis :: String,
    issueKindBug :: String
 } deriving (Show, Eq)

data IssueSeverity = ISCritical | ISMayHarm | ISNitpicking | ISStyle deriving (Show, Eq)

data IssueConfidence = ICSure | ICLikely | ICPossible deriving (Show, Eq)

data Issue = Issue {
    issueTitle :: String,
    issueMessage :: String,
    issueFileName :: FilePath,
    issueClassName :: Maybe String,
    issueFunctionName :: Maybe String,
    issueLineNumber :: Int,
    issueKind :: IssueKind,
    issueSeverity :: IssueSeverity,
    issueConfidence :: IssueConfidence,
    issueContext :: [String] -- to identify the same issue even when sources change slightly
 } deriving Show

issueSame :: Issue -> Issue -> Bool
issueSame x y = foldl (\b f -> b && f x y) True checks
  where
    q f = \x y -> f x == f y
    checks = [q issueFileName, q issueClassName, q issueFunctionName,
              q issueKind, q issueSeverity, q issueContext]
