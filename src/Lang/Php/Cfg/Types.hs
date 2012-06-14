{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Lang.Php.Cfg.Types(
      Variable(..)
    , InstrPos(..)
    , Instr(..)
    , Callable(..)
    , Cfg
) where

import Data.Functor((<$>))
import Compiler.Hoopl

data Variable = VUser String | VUnique Unique deriving Show

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
  ICondJump :: Variable -> Label -> Label -> Instr O C
  IReturn :: Maybe Variable -> Instr O C
  ICall :: Callable -> [Variable] -> Instr O O
  ILoad :: Variable -> Instr O O -- FIXME: this is temporary, remove when expressions are implemented

deriving instance Show (Instr e x)

instance NonLocal InstrPos where
  entryLabel (IP _ (ILabel x)) = x
  successors (IP _ (IJump x)) = [x]
  successors (IP _ (ICondJump _ x y)) = [x, y]
  successors (IP _ (IReturn _)) = []

instance HooplNode InstrPos where
  mkBranchNode x = IP Nothing (IJump x)
  mkLabelNode x = IP Nothing (ILabel x)

data Callable = CEcho deriving Show -- TODO

type Cfg = Graph InstrPos O O
