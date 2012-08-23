{-# LANGUAGE RankNTypes, GADTs, TypeSynonymInstances, FlexibleInstances #-}

module Lang.Php.Cfg.Utils(
    mapGraphM, mapBlockM, mapMaybeOM, mapMaybeCM,
    InitializedUniqueMonad, runIUM,
    concatGraph, addBlocks',
    CaseOC(..)
 ) where

import Data.Functor
import Data.Functor.Identity
import Control.Monad(liftM)
import Control.Monad.State

import Compiler.Hoopl
import Compiler.Hoopl.GHC

import Lang.Php.Cfg.Types

-------------------------------------------------------------------------------
--                    Monadic mapping over graphs
-------------------------------------------------------------------------------

-- Similar to mapGraph but a monadic actions is mapped over the graph's nodes
-- (instead of a pure function).
mapGraphM :: (Monad m, NonLocal node) => (forall e x. node e x -> m (node e x))
             -> Graph node e x -> m (Graph node e x)

mapGraphM f GNil = return GNil
mapGraphM f (GUnit block) = liftM GUnit (mapBlockM f block)
mapGraphM f (GMany m_entry blockMap m_exit) = do
  m_entry' <- mapMaybeOM (mapBlockM f) m_entry
  blockMap' <- liftM mapFromList $ forM (mapToList blockMap) $ \(k, v) -> do
    v' <- mapBlockM f v
    return (entryLabel v', v')
  m_exit' <- mapMaybeOM (mapBlockM f) m_exit
  return (GMany m_entry' blockMap' m_exit')

-- Monadic variant of mapBlock
mapBlockM :: Monad m => (forall e x. node e x -> m (node e x))
             -> Block node e x -> m (Block node e x)
mapBlockM f block = case blockToNodeList block of
  (m_start, middle, m_end) -> do
    m_start' <- mapMaybeCM f m_start
    middle' <- mapM f middle
    m_end' <- mapMaybeCM f m_end
    return $ blockOfNodeList (m_start', middle', m_end')

-- Monadic variant of fmap for MaybeO
mapMaybeOM :: Monad m => (a -> m b) -> MaybeO ex a -> m (MaybeO ex b)
mapMaybeOM f NothingO = return NothingO
mapMaybeOM f (JustO x) = liftM JustO (f x)

-- Monadic variant of fmap for MaybeC
mapMaybeCM :: Monad m => (a -> m b) -> MaybeC ex a -> m (MaybeC ex b)
mapMaybeCM f NothingC = return NothingC
mapMaybeCM f (JustC x) = liftM JustC (f x)

-------------------------------------------------------------------------------
--                      InitializedUniqueMonad
-------------------------------------------------------------------------------
-- Very similar to SimpleUniqueMonad from Compiler.Hoopl but allows to extend
-- a graph which already has some uniques in it while avoiding duplication.
-- Function runIUM can be given a `starting' unique value n and all uniques
-- generated inside the monad will be equal to n or greater.
--
-- To use it you first have to determine a "maximal" unique value in the input
-- graph and provide something greater as an initializer.

data InitializedUniqueMonad a = IUM {unIUM :: State Int a}

instance Monad InitializedUniqueMonad where
  (IUM x) >>= f = IUM (x >>= (unIUM . f))
  return = IUM . return

instance UniqueMonad InitializedUniqueMonad where
  freshUnique = IUM $ do
    x <- get
    put (x + 1)
    return (intToUnique x)

runIUM :: Int -> InitializedUniqueMonad a -> a
runIUM initial (IUM x) = fst (runState x initial)

-------------------------------------------------------------------------------
--                          concatGraph
-------------------------------------------------------------------------------
-- Just like the standard function concat :: [[a]] -> [a] flattens a list of
-- lists, this function can be used to flatten a graph of graphs (since a
-- whole graph can be used as a Hoopl node).

concatGraph :: NonLocal n => Graph (Graph n) e x -> Graph n e x
concatGraph GNil = GNil
concatGraph (GUnit block) = concatBlock block
concatGraph (GMany NothingO blockMap NothingO) = concatLabelMap blockMap
concatGraph (GMany NothingO blockMap (JustO end)) =
  concatLabelMap blockMap |*><*| concatBlock end
concatGraph (GMany (JustO start) blockMap NothingO) =
  concatBlock start |*><*| concatLabelMap blockMap
concatGraph (GMany (JustO start) blockMap (JustO end)) =
  concatBlock start |*><*| concatLabelMap blockMap |*><*| concatBlock end

concatLabelMap :: NonLocal n => LabelMap (Block (Graph n) C C) -> Graph n C C
concatLabelMap = foldl (|*><*|) emptyClosedGraph . map concatBlock . mapElems

concatBlock :: NonLocal n => Block (Graph n) e x -> Graph n e x
concatBlock block = case blockToNodeList block of
  (NothingC, middle, NothingC)     -> catG middle
  (NothingC, middle, JustC end)    -> catG middle <*> end
  (JustC start, middle, NothingC)  -> start <*> catG middle
  (JustC start, middle, JustC end) -> start <*> catG middle <*> end
 where
   catG :: NonLocal n => [Graph n O O] -> Graph n O O
   catG = foldl (<*>) emptyGraph

addBlocks' :: (HooplNode n, UniqueMonad m) => Graph n e x -> Graph n C C -> m (Graph n e x)
addBlocks' g closed = graphOfAGraph $ addBlocks (aGraphOfGraph g) (aGraphOfGraph closed)

-------------------------------------------------------------------------------
--                                CaseOC
-------------------------------------------------------------------------------
-- I find these functions necessary in many places during Hoopl graph manipu-
-- -lation. I don't know why these aren't in Hoopl -- maybe the same thing
-- can be achieved in some other way but I can't figure it out.
--
-- liftOC can be used to construct generalized functions working over all
-- types of graphs/blocks/nodes (like the one required as an argument to
-- foldGraphNodes). Other variants are just for convenience and can be left
-- as defaults during implementation.

class CaseOC thing where
  liftOC :: (thing O O -> c)
         -> (thing O C -> c)
         -> (thing C O -> c)
         -> (thing C C -> c)
         -> thing e x -> c

  -- Swapped-argument variant of liftOC that looks more like a `case'.
  caseOC :: thing e x
         -> (thing O O -> c)
         -> (thing O C -> c)
         -> (thing C O -> c)
         -> (thing C C -> c)
         -> c
  caseOC x fOO fOC fCO fCC = liftOC fOO fOC fCO fCC x

  onlyOO :: thing e x -> c -> (thing O O -> c) -> c
  onlyOO x val f = liftOC f (const val) (const val) (const val) x

  onlyOC :: thing e x -> c -> (thing O C -> c) -> c
  onlyOC x val f = liftOC (const val) f (const val) (const val) x

  onlyCO :: thing e x -> c -> (thing C O -> c) -> c
  onlyCO x val f = liftOC (const val) (const val) f (const val) x

  onlyCC :: thing e x -> c -> (thing C C -> c) -> c
  onlyCC x val f = liftOC (const val) (const val) (const val) f x


instance CaseOC (Block thing) where
  liftOC fOO fOC fCO fCC x = case blockToNodeList x of
    (NothingC, _, NothingC) -> fOO x
    (NothingC, _, JustC _ ) -> fOC x
    (JustC _ , _, NothingC) -> fCO x
    (JustC _ , _, JustC _ ) -> fCC x

instance CaseOC (Graph thing) where
  liftOC fOO fOC fCO fCC x = impl x
   where
     impl GNil = fOO x
     impl (GUnit _) = fOO x
     impl (GMany (JustO _) _ (JustO _)) = fOO x
     impl (GMany (JustO _) _ NothingO ) = fOC x
     impl (GMany NothingO  _ (JustO _)) = fCO x
     impl (GMany NothingO  _ NothingO ) = fCC x

-- Instance declaration for InstrPos maybe belongs more in Types.hs but to
-- avoid unnecessary dependency cycle it's given here. It feels kind of dumb
-- to have to list all possible instructions here but I don't see any other
-- way (aside from TH maybe).
instance CaseOC InstrPos where
  liftOC fOO fOC fCO fCC x@(IP _ instr) = impl instr
   where
     impl (ILabel _) = fCO x
     impl (IFuncEntry _ _ _) = fCO x
     impl (IJump _) = fOC x
     impl (ICondJump _ _ _) = fOC x
     impl (IReturn _) = fOC x
     impl (ICall _ _ _) = fOO x
     impl (ICatchException _ _ _) = fCO x
     impl (IThrow _) = fOC x
     impl (ILoadString _ _) = fOO x
     impl (ILoadNum _ _) = fOO x
     impl (ILoadConst _ _) = fOO x
     impl (ICopyVar _ _) = fOO x
     impl (IDeclare _) = fOO x
     impl IUnknown = fOO x
