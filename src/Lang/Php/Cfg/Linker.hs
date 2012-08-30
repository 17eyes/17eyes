{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Lang.Php.Cfg.Linker(linkCfg, adjustUniques) where

import Control.Monad.State
import qualified Control.Monad.State as State

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Applicative((<$>))
import Control.Arrow(second)

import Compiler.Hoopl
import Compiler.Hoopl.GHC

import Lang.Php.Cfg.Types
import Lang.Php.Cfg.Utils
import Codebase

data LinkerState = LS {
    lsSafeUnique :: Int,
    lsFunctions :: Map String [Label],
    lsFragments :: [Graph InstrPos C C]
 }

type LinkerMonad = StateT LinkerState IO

instance UniqueMonad LinkerMonad where
  freshUnique = do
    old_su <- lsSafeUnique <$> get
    modify $ \x -> x { lsSafeUnique = old_su + 1 }
    return (intToUnique old_su)

linkCfg :: Codebase' -> FilePath -> IO Cfg
linkCfg codebase filepath = do
  (cfg, safe_unique) <- adjustUniques 0 <$> resolveFile codebase filepath
  mod_functions <- moduleFunctions codebase filepath
  let init_state = LS {
     lsSafeUnique = safe_unique,
     lsFunctions = Map.fromAscList $ map (\(x,y) -> (x,[y])) mod_functions,
     lsFragments = []
   }
  fst <$> runStateT (workInsideLM cfg) init_state
 where
   workInsideLM :: Cfg -> LinkerMonad Cfg
   workInsideLM cfg = do
     cfg_mapped <- mapGraphM procNode cfg
     fragments <- lsFragments <$> get
     let fun_cfgs = foldl (|*><*|) emptyClosedGraph fragments
     addBlocks' cfg_mapped fun_cfgs

   procNode :: InstrPos e x -> LinkerMonad (InstrPos e x)
   procNode (IP pos (ICall r_res (CPhp name) args)) = do
     m_label <- Map.lookup name <$> lsFunctions <$> get
     labels <- case m_label of
       Just labels -> return labels
       Nothing -> do
         -- retrieve CFGs of functions with this name from the database
         (labels, cfgs) <- liftIO $ unzip <$> resolveFunction codebase name
         -- add these CFGs to the current state
         modify (\ls -> ls { lsFragments = cfgs ++ lsFragments ls })
         return labels
     return (IP pos $ ICallLabel r_res (CPhp name) args labels)

   procNode x = return x

-------------------------------------------------------------------------------
--                           adjustUniques
-------------------------------------------------------------------------------
-- Traverses a given CFG fragment and adjusts all uniques such that they are
-- larger or equal to a given start value. Returns a modified graph and a new
-- "safe" unique value.

-- A State monad is used during the traversal. The map keeps the mapping
-- between old and new values.
type AUM a = State (Int, Map Int Int) a

class Adjustable a where
  adjust :: a -> AUM a

instance Adjustable a => Adjustable [a] where
  adjust = mapM adjust

instance Adjustable a => Adjustable (Maybe a) where
  adjust Nothing = return Nothing
  adjust (Just x) = Just <$> adjust x

instance Adjustable () where
  adjust () = return ()

instance Adjustable a => Adjustable (a, a) where
  adjust (x, y) = do
    x' <- adjust x
    y' <- adjust y
    return (x', y')

instance Adjustable a => Adjustable (a, a, a) where
  adjust (x, y, z) = do
    x' <- adjust x
    y' <- adjust y
    z' <- adjust z
    return (x', y', z')

instance Adjustable Unique where
  adjust x = do
    (safe, known) <- State.get
    case Map.lookup (uniqueToInt x) known of
      (Just x') -> return (intToUnique x')
      Nothing -> do
        let known' = Map.insert (uniqueToInt x) safe known
        let safe' = safe + 1
        State.put (safe', known')
        return (intToUnique safe)

instance Adjustable Label where
  adjust lab = uniqueToLbl <$> adjust (lblToUnique lab)

instance Adjustable Register where
  adjust (RTemp uniq) = RTemp <$> adjust uniq
  adjust x = return x

instance Adjustable (InstrPos e x) where
  adjust (IP pos instr) = IP pos <$> adjust instr

instance Adjustable (Instr e x) where
  adjust (ILabel lab) = ILabel <$> adjust lab

  adjust (IFuncEntry name regs lab) = do
    regs' <- adjust regs
    lab' <- adjust lab
    return (IFuncEntry name regs' lab')

  adjust (IJump lab) = IJump <$> adjust lab

  adjust (ICondJump reg lab1 lab2) = do
    reg' <- adjust reg
    lab1' <- adjust lab1
    lab2' <- adjust lab2
    return (ICondJump reg' lab1' lab2')

  adjust (IReturn m_reg) = IReturn <$> adjust m_reg

  adjust (ICall reg cble args) = do
    reg' <- adjust reg
    (cble', args') <- adjust (cble, args)
    return (ICall reg' cble' args')

  adjust (ICatchException lab reg name) = do
    lab' <- adjust lab
    reg' <- adjust reg
    return (ICatchException lab' reg' name)

  adjust (IThrow reg) = IThrow <$> adjust reg

  adjust (ILoadString reg str) = do
    reg' <- adjust reg
    return (ILoadString reg' str)

  adjust (ILoadNum reg x) = do
    reg' <- adjust reg
    return (ILoadNum reg' x)

  adjust (ILoadConst reg x) = do
    reg' <- adjust reg
    return (ILoadConst reg' x)

  adjust (ICopyVar r1 r2) = do
    r1' <- adjust r1
    r2' <- adjust r2
    return (ICopyVar r1' r2')

  adjust (IDeclare dable) = IDeclare <$> adjust dable

  adjust IUnknown = return IUnknown

instance Adjustable Declarable where
  adjust (DFunction name lab graph) = do
    lab' <- adjust lab
    graph' <- adjust graph
    return (DFunction name lab' graph')

  adjust (DClass name meths fields) = do
    meths' <- forM meths $ \(vis, name, lab, graph) -> do
      lab' <- adjust lab
      graph' <- adjust graph
      return (vis, name, lab', graph')
    return (DClass name meths' fields)

-- This is sort of redundant but I see no other way of doing this (without TH).
instance Adjustable (Callable Register a, a) where
  adjust x@(CPrint, _)  = adjustC x
  adjust x@(CBitNot, _) = adjustC x
  adjust x@(CClone, _)  = adjustC x
  adjust x@(CNegate, _) = adjustC x

  adjust x@(CErrorsSuppress, _) = adjustC x
  adjust x@(CErrorsRestore, _)  = adjustC x

  adjust x@(CEq, _) = adjustC x
  adjust x@(CId, _) = adjustC x
  adjust x@(CLe, _) = adjustC x
  adjust x@(CLt, _) = adjustC x
  adjust x@(CBitAnd, _) = adjustC x
  adjust x@(CConcat, _) = adjustC x
  adjust x@(CDiv, _) = adjustC x
  adjust x@(CMod, _) = adjustC x
  adjust x@(CMul, _) = adjustC x
  adjust x@(CAdd, _) = adjustC x
  adjust x@(CShiftL, _) = adjustC x
  adjust x@(CShiftR, _) = adjustC x

  adjust x@(CItrGet, _) = adjustC x
  adjust x@(CItrCurrent, _) = adjustC x
  adjust x@(CItrKey, _) = adjustC x
  adjust x@(CItrValid, _) = adjustC x
  adjust x@(CItrNext, _) = adjustC x

  adjust x@(CPhp _, _) = adjustC x
  adjust x@(CPhpStatic _ _, _) = adjustC x

  adjust x@(CArrayPush, _) = adjustC x
  adjust x@(CArrayEmpty, _) = adjustC x
  adjust x@(CArrayGet, _) = adjustC x
  adjust x@(CArraySet, _) = adjustC x

  adjust x@(CCast, (str, reg)) = do
    reg' <- adjust reg
    return (CCast, (str, reg))

  adjust x@(CEval, _) = adjustC x
  adjust x@(CIsset, _) = adjustC x
  adjust x@(CInclude _ _, _) = adjustC x

adjustC :: Adjustable a => (Callable Register a, a) -> AUM (Callable Register a, a)
adjustC (cble, args) = (,) cble <$> adjust args

instance Adjustable (Graph InstrPos e x) where
  adjust = mapGraphM adjust

adjustUniques :: Int -> Graph InstrPos e x -> (Graph InstrPos e x, Int)
adjustUniques start graph = case runState (adjust graph) (start, Map.empty)
  of (graph', (next, _)) -> (graph', next)
