{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Lang.Php.Cfg.Types(
      Register(..)
    , InstrPos(..)
    , Instr(..)
    , Callable(..)
    , Cfg
) where

import Data.Functor((<$>))
import Compiler.Hoopl

data Register = RVar String | RTemp Unique | RNull

instance Show Register where
  show (RVar x) = "$" ++ x
  show (RTemp x) = "r" ++ show x
  show RNull = "_"

data InstrPos e x = IP (Maybe (FilePath, Int)) (Instr e x) deriving Show

instrFile :: InstrPos e x -> Maybe FilePath
instrFile (IP pos _) = fst <$> pos

instrLine :: InstrPos e x -> Maybe Int
instrLine (IP pos _) = snd <$> pos

instr :: InstrPos e x -> Instr e x
instr (IP _ x) = x

data Instr e x where
  ILabel :: Label -> Instr C O
  IJump :: Label -> Instr O C
  ICondJump :: Register -> Label -> Label -> Instr O C
  IReturn :: Maybe Register -> Instr O C
  ICall :: Show a => Register -> Callable Register a -> a -> Instr O O

  ILoadString :: Register -> String -> Instr O O
  ILoadNum :: Register -> String -> Instr O O -- TODO: a numeric type perhaps?
  ILoadConst :: Register -> String -> Instr O O
  ICopyVar :: Register -> Register -> Instr O O

deriving instance Show (Instr e x)

instance NonLocal InstrPos where
  entryLabel (IP _ (ILabel x)) = x
  successors (IP _ (IJump x)) = [x]
  successors (IP _ (ICondJump _ x y)) = [x, y]
  successors (IP _ (IReturn _)) = []

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

deriving instance Show tArg => Show (Callable tArg aSpec)

type Cfg = Graph InstrPos O O
