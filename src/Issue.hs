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

{-# LANGUAGE ExistentialQuantification #-}

-- | This is the module that defines Issue and friends. It's usually used to
-- create and manipulate Issue values. Creating issue kinds via newKind and
-- newKind_ should be only done in Kinds.hs.

module Issue(
    ModuleName, KindId, Severity(..), Confidence(..), Kind(..), Issue(..),
    issueKindId, match, newKind, newKind_, issueMessage, issueTitle
) where

import Data.Typeable

-- some aliases to make type signatures more readable:
type ModuleName = String
type KindId = String

-- | Severity represents how harmful the potential problem is. Do not confuse
-- this with Confidence.
data Severity = Critical | MayHarm | Nitpicking | Style deriving (Show, Eq)

-- | Confidence value represents how much the analysis is convinced that the
-- issue is correctly diagnosed. `Sure' should be used only when there is
-- absolutely no possibility of false positives.
data Confidence = Sure | Likely | Possible | Unlikely deriving (Show, Eq)

-- | Represents a class of issues created by the same analysis and representing
-- the same kind of problem with the code. All such values should be declared
-- in the module `Kinds' and created using `newKind' or `newKind_'.
--
-- `kindMessage' is a human-readable message that explains this issue to the
-- user. Sometimes it is desirable to customize some parts of the message for
-- a particular instance of the issue and provide a generic template in
-- `Kinds.hs'. To generate the final message, `kindMessage' is applied to the
-- payload stored in the Issue (as `issuePayload').
data Kind a = Kind {
    kindModule :: String, -- | source module of the analysis (e.g. Lang.Php...)
    kindId :: String,     -- | a string that uniquely identifies the issue
    kindTitle :: String,  -- | a human-readable title
    kindSeverity :: Severity,
    kindConfidence :: Confidence,
    kindMessage :: a -> String
}

-- | Represents a concrete, emitted issue. You can use the constructor to
-- create issues in simple cases. There are also shorter alternatives for
-- specific kinds of analyses (like `emitIssue' in Lang.Php.Ast.Traversal).
data Issue = forall a. Issue {
    issueKind :: Kind a,
    issueFileName :: Maybe FilePath,
    issueLineNumber :: Maybe Int,
    issueContext :: [String], -- | identifies this issue for `match'
    issuePayload :: a -- | used to generate the message via `kindMessage'
}

-- | `kindId' of the issue's Kind. Useful since `kindId . issueKind' won't
-- typecheck because of the existential type.
issueKindId :: Issue -> String
issueKindId (Issue kind _ _ _ _) = kindId kind

-- | To determine whether this is likely to be the same issue. This is based
-- on the `issueKind' and the kind-dependent context. Note that line number is
-- not taken into account (though file name is) since we want to match the
-- same issue after small changes to the source file.
match :: Issue -> Issue -> Bool
match x y = issueKindId x == issueKindId y
              && issueFileName x == issueFileName y
              && issueContext x == issueContext y

-- | Declares an issue kind. Essentially equivalent to the constructor but
-- provided for future compatibility.
newKind :: ModuleName -> KindId -> String -> Severity -> Confidence -> (a -> String) -> Kind a
newKind mod kid title severity confidence message = Kind {
    kindModule = mod,
    kindId = kid,
    kindTitle = title,
    kindSeverity = severity,
    kindConfidence = confidence,
    kindMessage = message
}

-- | A shorter alternative to `newKind' for kinds that do not need a message
-- payload (have type `Kind ()').
newKind_ :: ModuleName -> KindId -> String -> Severity -> Confidence -> String -> Kind ()
newKind_ mod kid title severity confidence message =
    newKind mod kid title severity confidence (\() -> message)

-- | Human-readable message carried with the issue. Generated using the template
-- in `Kind' and the issue's payload.
issueMessage :: Issue -> String
issueMessage (Issue kind _ _ _ pload) = kindMessage kind pload

-- | Helper function to extract the title of an issue. Conceptually equivalent
-- to `kindTitle . issueKind' but the latter won't typecheck because of the
-- existential type in `Issue'.
issueTitle :: Issue -> String
issueTitle (Issue kind _ _ _ _) = kindTitle kind
