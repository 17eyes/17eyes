{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts,
             UndecidableInstances #-}

module Lang.Php.Cfg.Generation where

import Lang.Php.Cfg.Types
import qualified Data.Intercal as IC

import qualified Lang.Php.Ast as Ast
import Lang.Php.Ast hiding ((<*>), Label)

import Control.Monad
import Control.Monad.State
import Data.Functor
import Compiler.Hoopl

data GS = GS -- TODO: define generation state

type GMonad = StateT GS SimpleUniqueMonad

runGMonad :: GMonad a -> a
runGMonad = fst . runSimpleUniqueMonad . (\x -> runStateT x GS)

instance UniqueMonad GMonad where
  freshUnique = lift freshUnique

-- FIXME: For some reason SimpleUniqueMonad wasn't declared a functor.
-- Is this correct?
instance Functor SimpleUniqueMonad where
  fmap f mx = mx >>= return . f

class CfgAble a where
  toCfg :: a -> GMonad Cfg

------------------------------------------------------------------------------
--                       Generic instances of CfgAble                       --
------------------------------------------------------------------------------

instance CfgAble WS where
  toCfg _ = return emptyGraph

instance (CfgAble a, CfgAble b) => CfgAble (IC.Intercal a b) where
  toCfg ic = case IC.breakStart ic of
    (x, xs) -> liftM2 (<*>) (toCfg x) (foldM op emptyGraph xs)
   where
     old `op` (x1, x2) = do
        new <- liftM2 (<*>) (toCfg x1) (toCfg x2)
        return (old <*> new)

-- FIXME: This needs UndecidableInstances. Is it worth it?
instance CfgAble (StoredPos a) => CfgAble (Ast.Block a) where
  toCfg (Ast.Block x) = toCfg x

instance CfgAble a => CfgAble (WSCap a) where
  toCfg = toCfg . wsCapMain

instance (CfgAble a, CfgAble b) => CfgAble (Either a b) where
  toCfg (Left x) = toCfg x
  toCfg (Right x) = toCfg x

instance (CfgAble a) => CfgAble (Maybe a) where
  toCfg (Just x) = toCfg x
  toCfg Nothing = return emptyGraph

instance (CfgAble a, CfgAble b) => CfgAble (a, b) where
  toCfg (x, y) = liftM2 (<*>) (toCfg x) (toCfg y)

------------------------------------------------------------------------------
--                             Expressions                                  --
------------------------------------------------------------------------------

-- TODO: implement
makeTac :: SourcePos -> Expr -> Variable -> GMonad Cfg
makeTac pos _ v = return (mkMiddle (sp2ip pos $ ILoad v))

------------------------------------------------------------------------------
--                              Statements                                  --
------------------------------------------------------------------------------

-- TODO: implement top-levels
instance CfgAble Ast where
  toCfg (Ast _ _ sl) = toCfg sl

instance CfgAble (StoredPos Stmt) where
  toCfg (StoredPos _ (StmtBlock b)) = toCfg b
  toCfg (StoredPos _ (StmtNothing _)) = return emptyGraph

  toCfg (StoredPos pos (StmtEcho args _)) = do
    (vars, gs) <- fmap unzip $ forM args $ \wexpr -> do
        var <- VUnique <$> freshUnique
        g <- makeTac pos (wsCapMain wexpr) var
        return (var, g)
    let call_node = mkMiddle $ sp2ip pos (ICall CEcho vars)
    return $ (foldl (<*>) emptyGraph gs) <*> call_node

  toCfg (StoredPos pos (StmtWhile (While wexpr block _))) = do
    lstart <- freshLabel
    lbody <- freshLabel
    lend <- freshLabel
    body <- toCfg block
    var_cond <- VUnique <$> freshUnique
    head_exp <- makeTac pos (wsCapMain $ wsCapMain wexpr) var_cond
    let head = (mkLabel lstart)
           <*> head_exp
           <*> (mkLast $ sp2ip pos (ICondJump var_cond lbody lend))
    let body_block = (mkLabel lbody) <*> body <*> (mkBranch lstart)
    return (mkBranch lstart |*><*| head |*><*| body_block |*><*| mkLabel lend)

  toCfg (StoredPos pos (StmtIf (If _ bls el))) = do
      after <- freshLabel
      graph <- gen_if bls el after
      return (graph |*><*| mkLabel after)
    where
      gen_if :: [IfBlock] -> (Maybe (BlockOrStmt, WS)) -> Label -> GMonad (Graph InstrPos O C)
      gen_if [] Nothing after = return (mkBranch after)
      gen_if [] (Just (block,_)) after = fmap (<*> mkBranch after) (toCfg block)
      gen_if (x:xs) el after = do
        ltrue <- freshLabel
        lfalse <- freshLabel
        var_cond <- VUnique <$> freshUnique
        then_block <- toCfg (ifBlockBlock x)
        else_block <- gen_if xs el after
        cond_graph <- makeTac pos (snd $ wsCapMain $ ifBlockExpr x) var_cond
        return $
          (cond_graph <*> mkLast (sp2ip pos (ICondJump var_cond ltrue lfalse)))
            |*><*| (mkLabel ltrue <*> then_block <*> mkBranch after)
            |*><*| (mkLabel lfalse <*> else_block)

------------------------------------------------------------------------------
--                          Helper functions                                --
------------------------------------------------------------------------------

sp2ip :: Ast.SourcePos -> Instr e x -> InstrPos e x
sp2ip pos = IP (Just (sourceName pos, sourceLine pos))
