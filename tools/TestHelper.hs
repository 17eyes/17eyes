{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}

-- | This module provides helper functions to the .hutest tests. All tests
-- can use 'assertIssue' and 'assertNoIssue' assertions predefined using
-- 'createIssueAsserts'. The query passed to these assertions can be
-- anything of class IssueQuery.
--
-- Two kinds of queries are supported currently: matching to the issueKind
-- value and testing issueLineNumber. Other query types should be added if
-- required.

module TestHelper(
    module Test.HUnit,
    module Common,
    runTests,
    IssueQuery(..), createIssueAsserts, atLine,
    isDelimiter, markerLines
) where

import Test.HUnit
import System.Exit(exitFailure,exitSuccess)
import Text.Regex.Posix

import Lang.Php.Ast
import Pipeline
import Common

class IssueQuery a where
    iqMatches :: a -> Issue -> Bool -- | does issue match this query
    iqRequirements :: a -> [String] -- | helpful messages emitted in case of error

instance IssueQuery IssueKind where
    iqMatches ik issue = ik == issueKind issue
    iqRequirements ik = ["issueKind is equal to " ++ show ik]

type IssueAssertion = IssueQuery a => a -> Assertion

data LineQuery aq = MkLineQuery Int aq
atLine q x = MkLineQuery x q -- handy alias

instance IssueQuery aq => IssueQuery (LineQuery aq) where
    iqMatches (MkLineQuery line q) issue =
        (issueLineNumber issue == Just line) && (iqMatches q issue)

    iqRequirements (MkLineQuery line q) =
        ("issue was found at line " ++ show line):(iqRequirements q)

createIssueAsserts :: String -> (IssueAssertion, IssueAssertion)
createIssueAsserts source = (assertIssue, assertNoIssue)
 where
    issues = analyzeFile "<hunit test>" source
    assertIssue q = assertBool (msg_issue q) (any (iqMatches q) issues)
    assertNoIssue q = assertBool (msg_no_issue q) (all (not . iqMatches q) issues)

    msg_issue q = "Expected to find an issue such that:\n"
               ++ (unlines $ map ("\t* "++) (iqRequirements q))

    msg_no_issue q = "Expected to find *no* issue such that:\n"
               ++ (unlines $ map ("\t* "++) (iqRequirements q))


-- | True, if a given line is the '=== BEGIN CODE ===' delimiter.
isDelimiter :: String -> Bool
isDelimiter = (=~ regex)
 where regex = "^\\s*=+\\s*BEGIN TEST\\s*=+\\s*$"

-- | Returns a list of all line numbers in the input that contain a given
-- string and are in the '=== BEGIN CODE ===' section.
markerLines :: String -> String -> [Int]
markerLines marker text =
    map (+ (idx+2)) $ findIndices (marker `isInfixOf`) (drop (idx+1) xs)
 where
    xs = lines text
    idx = maybe (error "No '== BEGIN CODE ==' marker in the file.")
                id
                (findIndex isDelimiter xs)

markerLine :: String -> String -> Int
markerLine marker source = case markerLines marker source of
    [x] -> x
    []  -> error "No marker present."
    _   -> error "More than one marker occurrence."

runTests tests = do
    x <- runTestTT (test tests)
    if errors x > 0 || failures x > 0
        then exitFailure
        else exitSuccess
