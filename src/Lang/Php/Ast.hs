{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Ast (
  module Lang.Php.Ast.Common,
  module Lang.Php.Ast.Expr,
  module Lang.Php.Ast.Lex,
  module Lang.Php.Ast.Types,
  Ast(..)
  ) where

import Common
import Control.Applicative hiding ((<|>), many)
import Control.Arrow
import Control.Monad
import Data.Char
import Lang.Php.Ast.Common
import Lang.Php.Ast.Expr
import Lang.Php.Ast.Lex
import Lang.Php.Ast.Types
import qualified Data.ByteString as BS
import qualified Data.Intercal as IC

data Ast = Ast FilePath TopLevel StmtList
  deriving (Eq, Show, Typeable, Data)

instance Unparse Ast where
  unparse (Ast _ t s) = unparse t ++ unparse s

instance Parse Ast where
  parse = liftM3 Ast fname parse stmtListParser
   where fname = sourceName <$> getPosition

$(derive makeBinary ''Ast)

