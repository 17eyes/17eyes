--
-- Copyright (c) 2012 by Tomasz Dudziak, Mateusz Kocielski
-- www.17eyes.com, hello@17eyes.com
--

-- | This module implements character-level "analyses", like checking proper
-- indentation, column width, etc.
module CharAnalysis(runAnalyses) where

import Data.Char
import qualified Data.Map as Map
import Data.Map((!))
import Data.List(findIndex)
import Control.Monad
import Data.Functor

import Common
import qualified Kinds
import qualified Issue as Issue
import Issue(Issue(Issue))

runAnalyses :: FilePath -> String -> [Issue]
runAnalyses fn str = tabsVsSpaces fn str ++ lineLengthCheck fn str

tabsVsSpaces fn str = if emit_issue
    then [Issue Kinds.tabsVsSpaces (Just fn) Nothing [] (s_chosen, s_other)]
    else []
 where
    ws = [' ', '\t'] -- all "interesting" whitespace

    -- all tabs and spaces in the input
    all_white = concatMap (takeWhile (`elem` ws)) (lines str)

    count_map :: Map.Map Char Int
    count_map = foldr (Map.adjust (1+)) init_map all_white
    init_map = Map.fromList [(x,0) | x <- ws]

    count_tabs = count_map ! '\t'
    count_spaces = count_map ! ' '
    use_tabs = count_tabs > count_spaces
    emit_issue = if use_tabs then count_spaces > 0 else count_tabs > 0

    s_chosen = if use_tabs then "tabs" else "spaces"
    s_other = if use_tabs then "spaces" else "tabs"

lineLengthCheck :: FilePath -> String -> [Issue]
lineLengthCheck fn str = issues
 where
    lens = map length (lines str)

    -- typical conventions on maximal line length (must be sorted)
    magic_lens = [78, 80, 85, 90, 100, 125]

    -- number of source lines that are shorter than ith limit from magic_lens
    popularity = map (\x -> length $ filter (<= x) lens) magic_lens

    -- 95% of all lines dictate the convention
    threshold = (length lens) * 95 `div` 100

    -- infer the tightest limit satisfied by <threshold> lines
    convention :: Maybe Int
    convention = (magic_lens !!) <$> (findIndex (>= threshold) popularity)

    issues = case convention of
        Nothing -> [Issue Kinds.lineLengthInconsistent (Just fn) Nothing [] ()]
        Just limit -> [mkIssue limit idx | (idx,x) <- zip [0..] lens, x >= limit]

    mkIssue lim idx = Issue Kinds.lineLength (Just fn) (Just (idx+1)) [] (show lim)
