{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances #-}

module Lang.Php.Cfg.Types(
      Register(..)
    , Visibility(..)
    , Declarable(..)
    , InstrPos(..)
    , Instr(..)
    , Callable(..)
    , Cfg
) where

import Data.Functor((<$>))
import Compiler.Hoopl

import Lang.Php.Ast(IncOrReq, OnceOrNot)

data Register = RVar String | RTemp Unique | RNull

instance Show Register where
  show (RVar x) = "$" ++ x
  show (RTemp x) = "r" ++ show x
  show RNull = "_"

data Visibility = Public | Private | Protected deriving Show

data Declarable =
    DFunction String Label (Graph InstrPos C C)
  | DClass {
      -- FIXME: this obviously isn't everything that can live inside a PHP class
      dclsName :: String,
      dclsMethods :: [(Visibility, String, Label, Graph InstrPos C C)],
      dclsFields :: [(Visibility, String)]
    }
  deriving Show

-- We would like to derive Show Declarable automatically but there is no
-- reasonable representation of graphs in textual from. For convenience we
-- declare Show for graphs like this:

instance Show (Graph InstrPos C C) where
  show _ = "<Graph InstrPos C C>"

instance Show (Graph InstrPos O O) where
  show _ = "<Graph InstrPos O O>"

data InstrPos e x = IP (Maybe (FilePath, Int)) (Instr e x) deriving Show

instrFile :: InstrPos e x -> Maybe FilePath
instrFile (IP pos _) = fst <$> pos

instrLine :: InstrPos e x -> Maybe Int
instrLine (IP pos _) = snd <$> pos

instr :: InstrPos e x -> Instr e x
instr (IP _ x) = x

data Instr e x where
  ILabel :: Label -> Instr C O
  IFuncEntry :: String -> [Register] -> Label -> Instr C O
  IJump :: Label -> Instr O C
  ICondJump :: Register -> Label -> Label -> Instr O C
  IReturn :: Maybe Register -> Instr O C
  ICall :: Show a => Register -> Callable Register a -> a -> Instr O O

  ICatchException :: Label -> Register -> String -> Instr C O
  IThrow :: Register -> Instr O C

  ILoadString :: Register -> String -> Instr O O
  ILoadNum :: Register -> String -> Instr O O -- TODO: a numeric type perhaps?
  ILoadConst :: Register -> String -> Instr O O
  ICopyVar :: Register -> Register -> Instr O O
  IDeclare :: Declarable -> Instr O O

  -- IUnknown is used to represent an instruction that can have *any* effect.
  -- Using it, we can generate the CFG somewhat robustly even in presence of
  -- unimplemented constructions in the input code.
  IUnknown :: Instr O O

deriving instance Show (Instr e x)

instance NonLocal InstrPos where
  entryLabel (IP _ (ILabel x)) = x
  entryLabel (IP _ (IFuncEntry _ _ x)) = x
  entryLabel (IP _ (ICatchException x _ _)) = x
  successors (IP _ (IJump x)) = [x]
  successors (IP _ (ICondJump _ x y)) = [x, y]
  successors (IP _ (IReturn _)) = []
  successors (IP _ (IThrow _)) = []

instance HooplNode InstrPos where
  mkBranchNode x = IP Nothing (IJump x)
  mkLabelNode x = IP Nothing (ILabel x)

data Callable tArg aSpec where
  --                        UNARY OPERATORS
  ----------------------------------------------------------------------------
  -- PrNot is translated into a branch instruction.  Incrementation and decre-
  -- -mentation operators are replaced by assignments $x = $x +/- 1, though
  -- this isn't 100% semantically correct.  PrPos is translated into (0+e).
  -- We cannot translate PrNegate into (0-e) since (-) is implemented using
  -- negation.
  --
  -- TODO: how to handle PrAt/PrSuppress?
  CPrint  :: Callable t t -- note that `echo' is translated to CPrints
  CBitNot :: Callable t t
  CClone  :: Callable t t
  CNegate :: Callable t t

  --                       ERROR SUPPRESSION
  ----------------------------------------------------------------------------
  -- Following callables are used to implement the error suppression operator
  -- ('@'). Note that an exception may cause a corresponding CErrorsRestore
  -- call to be omitted. This (probably buggy) behavior is consistent with
  -- Zend PHP implementation.
  --
  -- CErrorsRestore suppresses error messages and returns a boolean PHP value.
  -- TRUE means that errors were already suppressed. This is used as an
  -- argument to CErrorsRestore later on.
  --
  -- CErrorsRestore accepts a boolean argument. If TRUE, the errors remain
  -- suppressed; if it's FALSE, then errors will be shown.
  --
  -- Note that storing the result of CErrorsRestore is needed to correctly
  -- implement nested '@' operators.
  CErrorsSuppress :: Callable t ()
  CErrorsRestore :: Callable t t

  --                        BINARY OPERATORS
  ----------------------------------------------------------------------------
  -- Some of the logical operators from AST's BinOp are translated into
  -- branching instructions.  This includes: BAnd*, BOr*, and BXorWd. BNE* and
  -- BNI are translated into negated BEq and BId.
  CEq :: Callable t (t,t)
  CId :: Callable t (t,t)
  -- BGE and BGT are translated into CLe and CLt with inverted arguments (but
  -- respecting the evaluation order).
  CLe :: Callable t (t,t) -- lower or equal
  CLt :: Callable t (t,t) -- lower than
  CBitAnd :: Callable t (t,t)
  -- BBitOr from BinOpBy is translated into bit-negated CBitAnd.
  CConcat :: Callable t (t,t)
  CDiv :: Callable t (t,t)
  -- BMinus is translated into negated CAdd
  CMod :: Callable t (t,t)
  CMul :: Callable t (t,t)
  CAdd :: Callable t (t,t)
  CShiftL :: Callable t (t,t)
  CShiftR :: Callable t (t,t)
  -- BXor is translated into other bit operators

  --               BUILTINS USED TO IMPLEMENT `foreach'
  ----------------------------------------------------------------------------
  CItrGet :: Callable t t
  CItrCurrent :: Callable t t
  CItrKey :: Callable t t
  CItrValid :: Callable t t
  CItrNext :: Callable t t

  --                      CALLS TO PHP FUNCTIONS
  ----------------------------------------------------------------------------
  CPhp :: String -> Callable t [t]
  CPhpStatic :: [String] -> String -> Callable t [t]

  --                        ARRAY OPERATIONS
  ----------------------------------------------------------------------------
  -- TODO: specify behavior on undefined array variable
  CArrayPush :: Callable t (t,t)  -- r1[] = r2
  CArrayEmpty :: Callable t ()    -- array()
  CArrayGet :: Callable t (t,t)   -- r1[r2]
  CArraySet :: Callable t (t,t,t) -- r1[r2] = r3

  -- Type in the casting operator is kept as string. Maybe if we're going to
  -- have some datatype to represent PHP types it should be used here?
  CCast :: Callable t (String, t)
  CEval :: Callable t t
  CIsset :: Callable t [t]
  CInclude :: IncOrReq -> OnceOrNot -> Callable t t

deriving instance Show tArg => Show (Callable tArg aSpec)

type Cfg = Graph InstrPos O O
