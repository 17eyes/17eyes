--
-- Copyright (c) 2012 by Tomasz Dudziak, Mateusz Kocielski
-- www.17eyes.com, hello@17eyes.com
--

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Common (
  module Text.ParserCombinators.Parsec,
  Oper, Parse(..), Unparse(..)) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Text.ParserCombinators.Parsec hiding (State, parse, choice)
import Text.ParserCombinators.Parsec.Expr

-- parsec 2 vs 3 stuff

type Oper a = Operator Char () a

class Parse a where
  parse :: Parser a

class Unparse a where
  unparse :: a -> String

instance (Parse a) => Parse [a] where
  parse = many parse

instance (Parse a, Parse b) => Parse (Either a b) where
  parse = Left <$> parse <|> Right <$> parse

instance (Unparse a, Unparse b) => Unparse (Either a b) where
  unparse (Left a) = unparse a
  unparse (Right a) = unparse a

instance (Unparse a, Unparse b) => Unparse (a, b) where
  unparse (a, b) = unparse a ++ unparse b


instance (Unparse a) => Unparse [a] where
  unparse = concatMap unparse

instance Unparse Char where
  unparse x = [x]

instance (Unparse a) => Unparse (Maybe a) where
  unparse = maybe "" unparse
