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

data Register = RVar String | RTemp Unique

instance Show Register where
  show (RVar x) = "$" ++ x
  show (RTemp x) = "r" ++ show x

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
  ICall :: Show a => Callable Register a -> a -> Instr O O

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

data Callable tArg aSPec where
  CEcho :: Callable t [t]

deriving instance Show tArg => Show (Callable tArg aSpec)

type Cfg = Graph InstrPos O O
