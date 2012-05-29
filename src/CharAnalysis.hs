-- | This module implements character-level "analyses", like checking proper
-- indentation, column width, etc.
module CharAnalysis(runAnalyses) where

import Data.Char
import qualified Data.Map as Map
import Data.Map((!))
import Control.Monad

import Common

runAnalyses :: FilePath -> String -> [Issue]
runAnalyses = tabsVsSpaces

tabsVsSpaces fn str = if emit_issue
    then [Issue {
            issueTitle = "code mixes tabs with spaces",
            issueMessage = "The code in this file is indented mostly with "
                        ++ s_chosen ++ " but still contains some " ++ s_other
                        ++ ". This may be confusing for some editors and "
                        ++ "source control programs.",
            issueFileName = Just fn,
            issueFunctionName = Nothing,
            issueLineNumber = Nothing,
            issueKind = IssueKind "CharAnalysis" "tabsVsSpaces",
            issueSeverity = ISStyle,
            issueConfidence = ICLikely,
            issueContext = []
          }]
    else []
 where
    ws = [' ', '\t'] -- all "interesting" whitespace

    -- all tabs and spaces in the input
    all_white = concatMap (takeWhile (`elem` ws)) (lines str )

    count_map :: Map.Map Char Int
    count_map = foldr (Map.adjust (1+)) init_map all_white
    init_map = Map.fromList [(x,0) | x <- ws]

    count_tabs = count_map ! '\t'
    count_spaces = count_map ! ' '
    use_tabs = count_tabs > count_spaces
    emit_issue = if use_tabs then count_spaces > 0 else count_tabs > 0

    s_chosen = if use_tabs then "tabs" else "spaces"
    s_other = if use_tabs then "spaces" else "tabs"
