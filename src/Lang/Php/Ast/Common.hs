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
{-
Copyright (c) 2009, Dan Corson, Sasha Rush, Facebook, inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of Facebook, inc. nor the names of its contributors may be
  used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  
-}

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances,
             FlexibleInstances, FlexibleContexts, OverlappingInstances,
             UndecidableInstances #-}

module Lang.Php.Ast.Common (
  module Common,
  module Control.Applicative,
  module Control.Arrow,
  module Control.Monad,
  module Data.Binary,
  module Data.Char,
  module Data.Data,
  module Data.DeriveTH,
  module Data.List,
  module Data.Maybe,
  WS, WS2, WSElem(..), WSCap(..), WSCap2, capify, wsNoNLParser, w2With,
  discardWS, StoredPos(..),
  rePairLeft, rePairRight, swap, uncons,
  upToCharsOrEndParser) where

import Common
import Control.Applicative hiding ((<|>), many, optional, Const)
import Control.Arrow
import Control.Monad
import Data.Binary
import Data.Char
import Data.Data hiding (Prefix, Infix)
import Data.DeriveTH
import Data.List
import Data.Maybe
import Text.Parsec.Pos(newPos)
import qualified Data.Intercal as IC

-- | Allows to wrap AST elements with additional information about position in
-- the source code. Thanks to several instance declarations, decorating parts
-- of the AST with the position should be as painless as possible.
data StoredPos a = StoredPos SourcePos a
  deriving (Show, Eq, Typeable, Data)

-- Since we want to derive Binary for StoredPos, we need to define it for
-- SourcePos somehow.
instance Binary SourcePos where
  put x = put (sourceName x, sourceLine x, sourceColumn x)
  get = do
    (name, line, column) <- get
    return (newPos name line column)

instance Parse (a, WS) => Parse (StoredPos a, WS) where
  parse = do
    pos <- getPosition
    (x, ws) <- parse
    return (StoredPos pos x, ws)

instance Parse a => Parse (StoredPos a) where
  parse = liftM2 StoredPos getPosition parse

instance Unparse a => Unparse (StoredPos a) where
  unparse (StoredPos _ x) = unparse x

type WS = [WSElem]

data WSElem = WS String | LineComment Bool String | Comment String
  deriving (Show, Eq, Typeable, Data)

type WS2 = (WS, WS)

w2With :: (Unparse t, Unparse t1) => String -> (t, t1) -> String
w2With s (w1, w2) = unparse w1 ++ s ++ unparse w2

instance Parse WSElem where
  parse = WS <$> many1 space <|>
    Comment <$> (tokStartCommentP >> upToCharsParser '*' '/') <|> do
      isSlash <- (tokLineCommentP >> return True) <|>
        (tokPoundP >> return False)
      (gotChars, c) <- upToCharsOrEndParser (/= '\n') '?' '>'
      -- hackily put the "?>" back; this should be rare and frowned upon
      -- and i can't believe php works this way with // vs ?>
      when gotChars $ do
        setInput =<< ("?>" ++) <$> getInput
        pos <- getPosition
        setPosition . setSourceColumn pos $ sourceColumn pos - 2
      return $ LineComment isSlash c

-- FIXME: yikes, these can't be in Lex.hs currently, reorg needed?
tokStartComment = "/*"
tokStartCommentP = try $ string tokStartComment
tokLineComment = "//";
tokLineCommentP = try $ string tokLineComment
tokEndComment = "*/"
tokEndCommentP = try $ string tokEndComment
tokPound = "#"
tokPoundP = string tokPound

upToCharsParser c1 c2 = do
  (gotChars, r) <- upToCharsOrEndParser (const True) c1 c2
  if gotChars then return r
    else fail $ "Unexpected <eof>, expecting " ++ [c1, c2] ++ "."

upToCharsOrEndParser f c1 c2 = do
  s <- many (satisfy (\ x -> x /= c1 && f x))
  r1Mb <- optionMaybe (char c1)
  second (s ++) <$> case r1Mb of
    Nothing -> return (False, "")
    Just _ -> upToCharsOrEndParserC2 f c1 c2

upToCharsOrEndParserC2 f c1 c2 = do
  r2Mb <- optionMaybe $ satisfy f
  case r2Mb of
    Nothing -> return (False, [c1])
    Just r2 -> if r2 == c2
      then return (True, "")
      else second (c1:) <$> if r2 == c1
        then upToCharsOrEndParserC2 f c1 c2
        else second (r2:) <$> upToCharsOrEndParser f c1 c2

instance Unparse WSElem where
  unparse (WS a) = a
  unparse (Comment a) = tokStartComment ++ a ++ tokEndComment
  unparse (LineComment isSlash a) =
    (if isSlash then tokLineComment else tokPound) ++ a

wsNoNLParser :: Parser String
wsNoNLParser = many (satisfy (\ x -> isSpace x && x /= '\n'))

data WSCap a = WSCap {
  wsCapPre :: WS,
  wsCapMain :: a,
  wsCapPost :: WS}
  deriving (Show, Eq, Typeable, Data)

instance (Unparse a) => Unparse (WSCap a) where
  unparse (WSCap a b c) = concat [unparse a, unparse b, unparse c]

instance Functor WSCap where
  fmap f w = w {wsCapMain = f $ wsCapMain w}

capify :: WS -> (a, WS) -> WSCap a
capify a (b, c) = WSCap a b c

instance (Parse (a, WS)) => Parse (WSCap a) where
  parse = liftM2 capify parse parse

instance Parse a => Parse (a, WS) where
  parse = liftM2 (,) parse parse

type WSCap2 a = WSCap (WSCap a)

$(derive makeBinary ''WSElem)
$(derive makeBinary ''WSCap)
$(derive makeBinary ''StoredPos)

discardWS :: Parser ()
discardWS = (parse :: Parser WS) >> return ()

-- Following definitions are from FUtil (https://github.com/facebook/futil):

rePairRight :: ((a, b), c) -> (a, (b, c))
rePairRight ((a, b), c) = (a, (b, c))

rePairLeft :: (a, (b, c)) -> ((a, b), c)
rePairLeft (a, (b, c)) = ((a, b), c)

swap :: (t, t1) -> (t1, t)
swap (x, y) = (y, x)

uncons :: [a] -> (a, [a])
uncons (x:xs) = (x, xs)
uncons [] = error "uncons: empty list"
