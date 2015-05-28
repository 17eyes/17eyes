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

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Ast.ArgList where

import Data.Either.Utils
import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex
import qualified Data.Intercal as IC

-- e.g. ($a, $b, $c) in f($a, $b, $c) or () in f()
argListParser :: Parser (a, WS) -> Parser (Either WS [WSCap a])
argListParser = fmap (map fromRight <$>) .
  genArgListParser False False True True

-- e.g. ($a, $b, $c,) in array($a, $b, $c,) or () in array()
arrListParser :: Parser (a, WS) -> Parser (Either WS ([WSCap a], Maybe WS))
arrListParser = fmap (f <$>) . genArgListParser False True True True
  where
  f args = first (map fromRight) $ case last args of
    Left ws -> (init args, Just ws)
    _ -> (args, Nothing)

-- e.g. ($a, , $c) in list($a, , $c) = array(1, 'bye', 3)
mbArgListParser :: Parser (a, WS) -> Parser (Either WS [Either WS (WSCap a)])
mbArgListParser = genArgListParser True False True True

-- e.g. ($a, $b, $c) in isset($a, $b, $c)
issetListParser :: Parser (a, WS) -> Parser [WSCap a]
issetListParser = fmap (map fromRight . fromRight) .
  genArgListParser False False False True

-- todo: this can just be separate right?
-- e.g. ($a) in exit($a) or () in exit()
exitListParser :: Parser (a, WS) -> Parser (Either WS (WSCap a))
exitListParser = fmap (fmap (fromRight . head)) .
  genArgListParser False False True False

genArgListParser :: Bool -> Bool -> Bool -> Bool -> Parser (a, WS) ->
  Parser (Either WS [Either WS (WSCap a)])
genArgListParser emptyElemsAllowed finalCommaAllowed singleWSPoss
    overOneArgAllowed p = do
  tokLParenP
  args <- grabArgs emptyElemsAllowed finalCommaAllowed singleWSPoss
    overOneArgAllowed p
  return $ case args of
    [Left ws] -> Left ws
    _ -> Right args

grabArgs :: Bool -> Bool -> Bool -> Bool -> Parser (a, WS) ->
  Parser [Either WS (WSCap a)]
grabArgs emptyElemsAllowed finalCommaAllowed isFirstArgAndWSPoss
    overOneArgAllowed p = do
  ws <- parse
  (arg, canContinue) <-
    if emptyElemsAllowed || finalCommaAllowed || isFirstArgAndWSPoss
    then do
      pResMb <- optionMaybe p
      return $ case pResMb of
        Just pRes -> (Right $ capify ws pRes, overOneArgAllowed)
        _ -> (Left ws, emptyElemsAllowed && overOneArgAllowed)
    else do
      pRes <- p
      return (Right $ capify ws pRes, overOneArgAllowed)
  let
    cont =
      grabArgs emptyElemsAllowed finalCommaAllowed False overOneArgAllowed p
  (arg:) <$> (if canContinue then ((tokCommaP >> cont) <|>) else id)
    (tokRParenP >> return [])

