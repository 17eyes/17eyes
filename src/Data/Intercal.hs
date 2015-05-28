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

{-# LANGUAGE DeriveDataTypeable #-}

module Data.Intercal where

import Common
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Data
import Prelude hiding (concatMap, map)
import qualified Prelude

data Intercal a b = Intercal a b (Intercal a b) | Interend a
  deriving (Eq, Show, Typeable, Data)

-- we're using method that should be faster-but-bigger instead of storing
-- length.  this is probably the same as the derive one, just use that?
instance (Binary a, Binary b) => Binary (Intercal a b) where
  put (Intercal x y r) = put (0 :: Word8) >> put x >> put y >> put r
  put (Interend x)     = put (1 :: Word8) >> put x
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM3 Intercal get get get
      1 -> liftM  Interend get

intercalParser :: Parser a -> Parser b -> Parser (Intercal a b)
intercalParser a b = do
  aRes <- a
  bResMb <- optionMaybe b
  case bResMb of
    Nothing -> return $ Interend aRes
    Just bRes -> liftM (Intercal aRes bRes) $ intercalParser a b

instance (Parse a, Parse b) => Parse (Intercal a b) where
  parse = intercalParser parse parse

intercalUnparser :: (a -> String) -> (b -> String) -> Intercal a b -> String
intercalUnparser f g i =
  Prelude.concatMap (\ (a, b) -> f a ++ g b) xInit ++ f xEnd where
  (xInit, xEnd) = breakEnd i

instance (Unparse a, Unparse b) => Unparse (Intercal a b) where
  unparse = intercalUnparser unparse unparse

concatMapM :: (Monad m) => (a -> b -> a -> m (Intercal a b)) ->
  Intercal a b -> m (Intercal a b)
concatMapM _f i@(Interend _) = return i
concatMapM f (Intercal x1 y (Interend x2)) = f x1 y x2
concatMapM f (Intercal x1 y (Intercal x2 y2 rest)) = do
  (fResMain, fResEnd) <- liftM breakEnd $ f x1 y x2
  liftM (prepend fResMain) . concatMapM f $ Intercal fResEnd y2 rest

concatMap :: (a -> b -> a -> Intercal a b) -> Intercal a b -> Intercal a b
concatMap _f i@(Interend _) = i
concatMap f (Intercal x1 y (Interend x2)) = f x1 y x2
concatMap f (Intercal x1 y (Intercal x2 y2 rest)) =
  prepend fResMain . concatMap f $ Intercal fResEnd y2 rest
  where
  (fResMain, fResEnd) = breakEnd $ f x1 y x2

prepend :: [(a, b)] -> Intercal a b -> Intercal a b
prepend [] = id
prepend ((x, y):rest) = Intercal x y . prepend rest

breakEnd :: Intercal a b -> ([(a, b)], a)
breakEnd (Interend x) = ([], x)
breakEnd (Intercal x y rest) = first ((x, y):) $ breakEnd rest

unbreakEnd :: [(a, b)] -> a -> Intercal a b
unbreakEnd [] xEnd = Interend xEnd
unbreakEnd ((x, y):xys) xEnd = Intercal x y $ unbreakEnd xys xEnd

toList1 :: Intercal a b -> [a]
toList1 ic = case breakStart ic of
    (x, xs) -> x:(Prelude.map snd xs)

toList2 :: Intercal a b -> [b]
toList2 = Prelude.map snd . fst . breakEnd

rePairStart :: a -> [(b, a)] -> ([(a, b)], a)
rePairStart x0 [] = ([], x0)
rePairStart x0 ((y, x):yxs) = first ((x0, y):) $ rePairStart x yxs

rePairEnd :: [(a, b)] -> a -> (a, [(b, a)])
rePairEnd [] xEnd = (xEnd, [])
rePairEnd ((x, y):xys) xEnd = (x, (y, xys0):xysRest) where
  (xys0, xysRest) = rePairEnd xys xEnd

breakStart :: Intercal a b -> (a, [(b, a)])
breakStart = uncurry rePairEnd . breakEnd

unbreakStart :: a -> [(b, a)] -> Intercal a b
unbreakStart = (uncurry unbreakEnd .) . rePairStart

map :: (a -> c) -> (b -> d) -> Intercal a b -> Intercal c d
map f _g (Interend x) = Interend (f x)
map f  g (Intercal x y rest) = Intercal (f x) (g y) (map f g rest)

mapA :: (Applicative m) => (a -> m c) -> (b -> m d) -> Intercal a b ->
  m (Intercal c d)
mapA f _g (Interend x) = liftA Interend (f x)
mapA f  g (Intercal x y rest) = liftA3 Intercal (f x) (g y) (mapA f g rest)

-- this is a singleton "in b" and you would just use Interend if you wanted a
-- singleton "in a"
singleton :: a -> b -> a -> Intercal a b
singleton a1 b a2 = Intercal a1 b $ Interend a2

append :: a -> b -> Intercal b a -> Intercal b a
append a b (Interend b0) = Intercal b0 a $ Interend b
append a b (Intercal b0 a0 rest) = Intercal b0 a0 $ append a b rest

