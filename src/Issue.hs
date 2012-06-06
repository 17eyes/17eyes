{-# LANGUAGE ExistentialQuantification #-}

-- | This is the module that defines Issue and friends. This module is intended
-- to be imported using a qualified import.

module Issue(
    ModuleName, KindId, Severity(..), Confidence(..), Kind(..), Issue(..),
    issueKindId, match, newKind, newKind_, new, issueMessage, issueTitle
) where

import Data.Typeable

-- some aliases to make type signatures more readable:
type ModuleName = String
type KindId = String

data Severity = Critical | MayHarm | Nitpicking | Style deriving (Show, Eq)

data Confidence = Sure | Likely | Possible | Unlikely deriving (Show, Eq)

data Kind a = Kind {
    kindModule :: String,
    kindId :: String,
    kindTitle :: String,
    kindSeverity :: Severity,
    kindConfidence :: Confidence,
    kindMessage :: a -> String
}

data Issue = forall a. Issue {
    issueKind :: Kind a,
    issueFileName :: Maybe FilePath,
    issueLineNumber :: Maybe Int,
    issueContext :: [String],
    issuePayload :: a
}

issueKindId :: Issue -> String
issueKindId (Issue kind _ _ _ _) = kindId kind

-- | To determine whether this is likely to be the same issue. This is based
-- on the issueKind and the kind-dependent context. Note that line number is
-- not taken into account (though file name is) since we want to match the
-- same issue after small changes to the source file.
match :: Issue -> Issue -> Bool
match x y = issueKindId x == issueKindId y
              && issueFileName x == issueFileName y
              && issueContext x == issueContext y

newKind :: ModuleName -> KindId -> String -> Severity -> Confidence -> (a -> String) -> Kind a
newKind mod kid title severity confidence message = Kind {
    kindModule = mod,
    kindId = kid,
    kindTitle = title,
    kindSeverity = severity,
    kindConfidence = confidence,
    kindMessage = message
}

newKind_ :: ModuleName -> KindId -> String -> Severity -> Confidence -> String -> Kind ()
newKind_ mod kid title severity confidence message =
    newKind mod kid title severity confidence (\() -> message)

new :: Kind a -> [String] -> a -> Issue
new kind ctx pload = Issue kind Nothing Nothing ctx pload

issueMessage :: Issue -> String
issueMessage (Issue kind _ _ _ pload) = kindMessage kind pload

issueTitle :: Issue -> String
issueTitle (Issue kind _ _ _ _) = kindTitle kind
