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

class TacAbleR a where
  toTacR :: a -> SourcePos -> GMonad (Register, Cfg)

class TacAbleL a where
  toTacL :: a -> SourcePos -> Register -> GMonad Cfg

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
--                       Generic instances of TacAble*                      --
------------------------------------------------------------------------------

instance TacAbleR a => TacAbleR (WSCap a) where
  toTacR = toTacR . wsCapMain

instance (TacAbleR a, TacAbleR b) => TacAbleR (Either a b) where
  toTacR (Left x) = toTacR x
  toTacR (Right x) = toTacR x

instance TacAbleL a => TacAbleL (WSCap a) where
  toTacL = toTacL . wsCapMain

instance (TacAbleL a, TacAbleL b) => TacAbleL (Either a b) where
  toTacL (Left x) = toTacL x
  toTacL (Right x) = toTacL x

------------------------------------------------------------------------------
--                              R-Values                                    --
------------------------------------------------------------------------------

instance TacAbleR Expr where
  toTacR (ExprNumLit (NumLit x)) pos = do
    var <- RTemp <$> freshUnique
    return (var, mkMiddle (sp2ip pos $ ILoadNum var x))

  toTacR (ExprRVal x) pos = toTacR x pos
  toTacR (ExprParen wse) pos = toTacR (wsCapMain wse) pos

  toTacR (ExprAssign Nothing lval _ expr) pos = do
    (var, graphR) <- toTacR expr pos -- code to evaluate expr and store in var
    graphL <- toTacL lval pos var    -- code to store var in lval
    return (var, graphR <*> graphL)

  toTacR (ExprAssign (Just _) lval _ expr) pos = error "TODO: implement binops"


  toTacR _ pos = error "TacAbleR Expr not fully implmented"

instance TacAbleR RVal where
  toTacR (RValROnlyVal x) = toTacR x
  toTacR (RValLRVal x) = toTacR x

instance TacAbleR ROnlyVal where
  toTacR (ROnlyValConst (Const [] w_name)) pos = do
    var <- RTemp <$> freshUnique
    return (var, mkMiddle $ sp2ip pos $ ILoadConst var (wsCapMain w_name))

  toTacR (ROnlyValConst _) pos = error "TODO: implement class member constants"
  toTacR (ROnlyValFunc _ _ _) pos = error "TODO: implement function calls"

-- Not that this translates an LRVal when used as an r-value. L-values are
-- handled differently.
instance TacAbleR LRVal where
  toTacR (LRValVar (DynConst [] wsc)) = case wsCapMain wsc of
    (Left _) -> error "TODO: implement TacAbleR LRVal"
    (Right x) -> toTacR x

  toTacR _ = error "TODO: implement TacAbleR LRVal"

instance TacAbleR Var where
  toTacR (Var name []) pos = return (RVar name, emptyGraph)
  toTacR (Var _ _) _ = error "TODO: implement indexed variables (as r-values)"
  toTacR _ _ = error "TODO: implement dyanmic variables (as r-values)"

------------------------------------------------------------------------------
--                              L-Values                                    --
------------------------------------------------------------------------------

instance TacAbleL LVal where
  toTacL (LValLOnlyVal x) = error "TODO: implement special l-values"
  toTacL (LValLRVal x) = toTacL x

instance TacAbleL LRVal where
  toTacL (LRValVar (DynConst [] wsc)) = case wsCapMain wsc of
    Right var -> toTacL var
    _ -> error "TODO: implement complex l-values"

  toTacL _ = error "TODO: implement complex l-values"

instance TacAbleL Var where
  toTacL (Var name []) pos var =
    return $ mkMiddle $ sp2ip pos $ ICopyVar (RVar name) var

  toTacL _ _ _ = error "TODO: implement indexed and dynamic variables"

------------------------------------------------------------------------------
--                              Statements                                  --
------------------------------------------------------------------------------

-- TODO: implement top-levels in Ast
instance CfgAble Ast where
  toCfg (Ast _ _ sl) = toCfg sl

instance CfgAble StmtEnd where
  toCfg StmtEndSemi = return emptyGraph
  toCfg (StmtEndClose _) = error "TODO: implement top levels"

instance CfgAble (StoredPos Stmt) where
  toCfg (StoredPos _ (StmtBlock b)) = toCfg b
  toCfg (StoredPos _ (StmtNothing _)) = return emptyGraph

  toCfg (StoredPos pos (StmtExpr e _ end)) = do
    (_, g1) <- toTacR e pos -- variable is discarded
    g2 <- toCfg end
    return (g1 <*> g2)

  toCfg (StoredPos pos (StmtEcho args _)) = do
    (vars, gs) <- fmap unzip $ forM args $
      \wexpr -> toTacR (wsCapMain wexpr) pos
    let call_node = mkMiddle $ sp2ip pos (ICall CEcho vars)
    return $ (foldl (<*>) emptyGraph gs) <*> call_node

  toCfg (StoredPos pos (StmtWhile (While wexpr block _))) = do
    lstart <- freshLabel
    lbody <- freshLabel
    lend <- freshLabel
    body <- toCfg block
    (var_cond, head_exp) <- toTacR (wsCapMain $ wsCapMain wexpr) pos
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
        then_block <- toCfg (ifBlockBlock x)
        else_block <- gen_if xs el after
        (var_cond, cond_graph) <- toTacR (snd $ wsCapMain $ ifBlockExpr x) pos
        return $
          (cond_graph <*> mkLast (sp2ip pos (ICondJump var_cond ltrue lfalse)))
            |*><*| (mkLabel ltrue <*> then_block <*> mkBranch after)
            |*><*| (mkLabel lfalse <*> else_block)

------------------------------------------------------------------------------
--                          Helper functions                                --
------------------------------------------------------------------------------

sp2ip :: Ast.SourcePos -> Instr e x -> InstrPos e x
sp2ip pos = IP (Just (sourceName pos, sourceLine pos))
