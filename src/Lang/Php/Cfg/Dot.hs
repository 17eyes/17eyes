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

{-# LANGUAGE MultiParamTypeClasses, GADTs #-}

-- | Dump control flow graph in the Graphviz Dot format.
module Lang.Php.Cfg.Dot(cfgToDot) where

import Data.List(intersperse)
import Compiler.Hoopl
import Lang.Php.Cfg.Types

-- | Converts graphs and subgraphs to a string that can be fed to Graphviz's
-- `dot' utility and used to produce a graphical representation of the control
-- flow graph.
cfgToDot :: Graph InstrPos e x -> String
cfgToDot g = "digraph CFG {\nnode[" ++ nodeopts ++ "];\n" ++ dumpGraph g ++ "}"
 where nodeopts = "shape=box"

dumpGraph :: Graph InstrPos e x -> String
dumpGraph GNil = ""
dumpGraph (GUnit block) = dumpBlock block
dumpGraph (GMany entry blocks exit) =
     maybeO "" dumpBlock entry
  ++ concatMap dumpBlock (mapElems blocks)
  ++ maybeO "" dumpBlock exit

dumpBlock :: Block InstrPos e x -> String
dumpBlock block =
     dot_label ++ "[label=\""
  ++ header
  ++ concatMap dumpNode middle
  ++ maybeC "\\l(END)" dumpNode m_last
  ++ "\"];\n"
  ++ maybeC "" (outEdges dot_label) m_last
 where
  header = maybeC "START:" mkHeader m_first
  mkHeader (IP _ (IFuncEntry name args lab)) =
       show lab ++ ": [" ++ name ++ "("
    ++ concat (intersperse ", " (map show args))
    ++ ")]"
  mkHeader (IP _ (ICatchException lab reg name)) =
       show lab ++ ": [catch " ++ name ++ " to " ++ show reg ++ "]"
  mkHeader _ = dot_label ++ ":"

  (m_first, middle, m_last) = blockToNodeList block
  dot_label = maybeC "START" (show . entryLabel) m_first

outEdges :: String -> InstrPos e C -> String
outEdges lab = concatMap (\x -> lab ++ "->" ++ show x ++ "\n") . successors

dumpNode :: InstrPos e x -> String
dumpNode (IP m_pos instr) = "\\l" ++ escape (show instr) ++ case m_pos of
  Just (_, x) -> " [" ++ show x ++ "]"
  Nothing -> ""

------------------------------------------------------------------------------
--                           Utility functions                              --
------------------------------------------------------------------------------

maybeO :: b -> (t -> b) -> MaybeO e t -> b
maybeO _ f (JustO x) = f x
maybeO x _ NothingO  = x

maybeC :: b -> (t -> b) -> MaybeC e t -> b
maybeC _ f (JustC x) = f x
maybeC x _ NothingC = x

escape :: String -> String
escape = dropLast . drop 1 . show
 where dropLast []     = error "dropLast: not enough elements"
       dropLast [x]    = []
       dropLast (x:xs) = x:dropLast xs
