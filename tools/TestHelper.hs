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
    IssueQuery(..), createIssueAsserts, atLine
) where

import Test.HUnit
import System.Exit(exitFailure,exitSuccess)

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

runTests tests = do
    x <- runTestTT (test tests)
    if errors x > 0 || failures x > 0
        then exitFailure
        else exitSuccess
