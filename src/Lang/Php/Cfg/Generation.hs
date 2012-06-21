{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts,
             UndecidableInstances #-}

module Lang.Php.Cfg.Generation where

import Lang.Php.Cfg.Types
import qualified Data.Intercal as IC

import qualified Lang.Php.Ast as Ast
import Lang.Php.Ast hiding ((<*>), Label, get, put)

import Control.Monad
import Control.Monad.State
import Data.Functor
import Compiler.Hoopl

data GS = GS {
    gsBreakTargets :: [Label],
    gsContinueTargets :: [Label]
}

initialGS = GS {
    gsBreakTargets = [],
    gsContinueTargets = []
}

type GMonad = StateT GS SimpleUniqueMonad

runGMonad :: GMonad a -> a
runGMonad = fst . runSimpleUniqueMonad . (\x -> runStateT x initialGS)

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

  -- Assignment with binary operator (like $x += 5) can be unfolded into a
  -- simple assignment (like $x = $x + 5).
  toTacR (ExprAssign (Just binop) lval@(LValLRVal lrval) _ expr) pos = toTacR e' pos
   where
     e' = ExprAssign Nothing lval ([],[]) expr'
     expr' = ExprBinOp (BByable binop) e_lrval ([],[]) expr
     e_lrval = ExprRVal (RValLRVal lrval)

  -- Simple binary operators are just mapped to corresponding callables.
  toTacR (ExprBinOp BEQ e1 _ e2) pos = simpleBinop CEq e1 e2 pos
  toTacR (ExprBinOp BID e1 _ e2) pos = simpleBinop CId e1 e2 pos
  toTacR (ExprBinOp BLE e1 _ e2) pos = simpleBinop CLe e1 e2 pos
  toTacR (ExprBinOp BLT e1 _ e2) pos = simpleBinop CLt e1 e2 pos
  toTacR (ExprBinOp (BByable BBitAnd) e1 _ e2) pos = simpleBinop CBitAnd e1 e2 pos
  toTacR (ExprBinOp (BByable BConcat) e1 _ e2) pos = simpleBinop CConcat e1 e2 pos
  toTacR (ExprBinOp (BByable BDiv) e1 _ e2) pos = simpleBinop CDiv e1 e2 pos
  toTacR (ExprBinOp (BByable BMod) e1 _ e2) pos = simpleBinop CMod e1 e2 pos
  toTacR (ExprBinOp (BByable BMul) e1 _ e2) pos = simpleBinop CMul e1 e2 pos
  toTacR (ExprBinOp (BByable BPlus) e1 _ e2) pos = simpleBinop CAdd e1 e2 pos
  toTacR (ExprBinOp (BByable BShiftL) e1 _ e2) pos = simpleBinop CShiftL e1 e2 pos
  toTacR (ExprBinOp (BByable BShiftR) e1 _ e2) pos = simpleBinop CShiftR e1 e2 pos

  -- Some operators can be translated to negated callables (e.g. e1 != e2
  -- becomes !(e1 == e2)).
  toTacR (ExprBinOp BGE e1 _ e2) pos = negatedBinop CLt e1 e2 pos
  toTacR (ExprBinOp BGT e1 _ e2) pos = negatedBinop CLe e1 e2 pos
  toTacR (ExprBinOp BNE e1 _ e2) pos = negatedBinop CEq e1 e2 pos
  toTacR (ExprBinOp BNI e1 _ e2) pos = negatedBinop CId e1 e2 pos

  -- `Alternative syntax' operators are treated like the simple version.
  toTacR (ExprBinOp BAndWd e1 w e2) pos = toTacR (ExprBinOp BAnd e1 w e2) pos
  toTacR (ExprBinOp BNEOld e1 w e2) pos = toTacR (ExprBinOp BNE e1 w e2) pos
  toTacR (ExprBinOp BOrWd e1 w e2) pos = toTacR (ExprBinOp BOr e1 w e2) pos

  -- Logical lazy operators are treated like conditionals.
  toTacR (ExprBinOp BAnd e1 _ e2) pos = toTacR outer_if pos
   -- (e1 && e2) <=> (e1? (e2? TRUE : FALSE) : FALSE)
   where
     inner_if = ExprTernaryIf (TernaryIf e2 nows (Just e_true) nows e_false)
     outer_if = ExprTernaryIf (TernaryIf e1 nows (Just inner_if) nows e_false)
     e_true = makeExprConst "TRUE"
     e_false = makeExprConst "FALSE"
     nows = ([],[])

  toTacR (ExprBinOp BOr e1 _ e2) pos = toTacR outer_if pos
   -- (e1 || e2) <=> (e1? TRUE : (e2? TRUE : FALSE))
   where
     inner_if = ExprTernaryIf (TernaryIf e2 nows (Just e_true) nows e_false)
     outer_if = ExprTernaryIf (TernaryIf e1 nows (Just e_true) nows inner_if)
     e_true = makeExprConst "TRUE"
     e_false = makeExprConst "FALSE"
     nows = ([],[])

  toTacR (ExprBinOp BXorWd e1 _ e2) pos = toTacR outer_if pos
   -- (e1 xor e2) <=> (e1? !e2 : (e2? TRUE : FALSE))
   -- Note that `e2' occurs twice in the above expression but it will be always
   -- evaluated only once.
   where
     not_e2 = ExprPreOp PrNot [] e2
     inner_if = ExprTernaryIf (TernaryIf e2 nows (Just e_true) nows e_false)
     outer_if = ExprTernaryIf (TernaryIf e1 nows (Just not_e2) nows inner_if)
     e_true = makeExprConst "TRUE"
     e_false = makeExprConst "FALSE"
     nows = ([],[])

  -- Rewrite (e1 | e2) to ~(~e1 & ~e2).
  toTacR (ExprBinOp (BByable BBitOr) e1 _ e2) pos = toTacR (mkNot e_and) pos
   where
     mkNot e = ExprPreOp PrBitNot [] e
     e_and = ExprBinOp (BByable BBitAnd) (mkNot e1) ([],[]) (mkNot e2)

  -- We cannot rewrite (e1 ^ e2) to ~(~e1 & ~e2) & ~(e1 & e2) since evaluation
  -- of some subexpressions can have side effects.  Thus, we create the CFG
  -- manually for this operator, based on a following pseudocode:
  --   (g1) r1 := e1
  --   (g2) r2 := e2
  --   (g3) r_and := r1 & r2
  --   (g4) r_and := ~r_and
  --   (g5) r_n1 := ~r1
  --   (g6) r_andnot := ~r2
  --   (g7) r_andnot := r_n1 & r_andnot
  --   (g8) r_andnot := ~r_andnot
  --   (g9) r_and := r_and & r_andnot
  --   return r_and
  toTacR (ExprBinOp (BByable BXor) e1 _ e2) pos = do
    (r_and, r_andnot, r_n1) <- liftM3 (,,) freshR freshR freshR
    (r1, g1) <- toTacR e1 pos
    (r2, g2) <- toTacR e2 pos
    let g3 = mkM (ICall r_and CBitAnd (r1, r2))
    let g4 = mkM (ICall r_and CBitNot r_and)
    let g5 = mkM (ICall r_n1 CBitNot r1)
    let g6 = mkM (ICall r_andnot CBitNot r2)
    let g7 = mkM (ICall r_andnot CBitAnd (r_n1, r_andnot))
    let g8 = mkM (ICall r_andnot CBitNot r_andnot)
    let g9 = mkM (ICall r_and CBitAnd (r_and, r_andnot))
    return (r_and, g1 <*> g2 <*> g3 <*> g4 <*> g5 <*> g6 <*> g7 <*> g8 <*> g9)
   where
    freshR = RTemp <$> freshUnique
    mkM = mkMiddle . sp2ip pos

  toTacR (ExprBinOp (BByable BMinus) e1 _ e2) pos = toTacR e' pos
   -- rewrite (e1 - e2) to (e1 + (-e2))
   where
     e' = ExprBinOp (BByable BPlus) e1 ([],[]) neg_e2
     neg_e2 = ExprPreOp PrNegate [] e2

  -- Simple unary operators also have their own callables.
  toTacR (ExprPreOp PrPrint _ e) pos = simpleUnop CPrint e pos
  toTacR (ExprPreOp PrBitNot _ e) pos = simpleUnop CBitNot e pos
  toTacR (ExprPreOp PrClone _ e) pos = simpleUnop CClone e pos
  toTacR (ExprPreOp PrNegate _ e) pos = simpleUnop CNegate e pos

  -- !e <=> (e? FALSE : TRUE)
  toTacR (ExprPreOp PrNot _ e) pos = toTacR if_expr pos
   where
    if_expr = ExprTernaryIf (TernaryIf e nows (Just e_false) nows e_true)
    e_true = makeExprConst "TRUE"
    e_false = makeExprConst "FALSE"
    nows = ([],[])

  -- (+e) is translated into (0+e)
  toTacR (ExprPreOp PrPos _ e) pos = toTacR e' pos
   where e' = ExprBinOp (BByable BPlus) (ExprNumLit (NumLit "0")) ([],[]) e

  -- TODO: implement error suppression operators
  toTacR (ExprPreOp PrSuppress _ e) pos = error "error suppression operators not implemented"
  toTacR (ExprPreOp PrAt w e) pos = toTacR (ExprPreOp PrSuppress w e) pos

  -- Incrementation & decrementation is translated into addition.
  toTacR (ExprPreOp PrIncr _ e) pos = preIncDecOp 1 e pos
  toTacR (ExprPreOp PrDecr _ e) pos = preIncDecOp (-1) e pos
  toTacR (ExprPostOp PoIncr e _) pos = postIncDecOp 1 e pos
  toTacR (ExprPostOp PoDecr e _) pos = postIncDecOp (-1) e pos

  toTacR (ExprTernaryIf (TernaryIf cond _ m_then _ e_else)) pos = do
    r_res <- RTemp <$> freshUnique
    (l_end, l_then, l_else) <- liftM3 (,,) freshLabel freshLabel freshLabel
    (r_cond, g_cond) <- toTacR cond pos
    let b_cond = g_cond
             <*> (mkLast $ sp2ip pos $ ICondJump r_cond l_then l_else)

    -- `then' block: If it's present, evaluate it. Otherwise, copy the value
    -- of evaluated condition to r_res.
    b_then <- case m_then of
      (Just e_then) -> do
        (r_then, g_then) <- toTacR e_then pos
        return $ mkLabel l_then
                     <*> g_then
                     <*> (mkMiddle $ sp2ip pos $ ICopyVar r_res r_then)
                     <*> mkBranch l_end
      Nothing -> return $ mkLabel l_then
                      <*> (mkMiddle $ sp2ip pos $ ICopyVar r_res r_cond)
                      <*> mkBranch l_end

    -- `else' block
    (r_else, g_else) <- toTacR e_else pos
    let b_else = mkLabel l_else
             <*> g_else
             <*> (mkMiddle $ sp2ip pos $ ICopyVar r_res r_else)
             <*> mkBranch l_end

    let b_end = mkLabel l_end
    return (r_res, b_cond |*><*| b_then |*><*| b_else |*><*| b_end)

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
    (vars, gs1) <- fmap unzip $ forM args $
      \wexpr -> toTacR (wsCapMain wexpr) pos
    let gs2 = map (mkMiddle . sp2ip pos . ICall RNull CPrint) vars
    return $ foldl (<*>) emptyGraph (gs1 ++ gs2)

  toCfg (StoredPos pos (StmtWhile (While wexpr block _))) = do
    lstart <- freshLabel
    lbody <- freshLabel
    lend <- freshLabel
    -- setup break/continue targets before entering the loop body
    modify $ \gs -> gs {
        gsBreakTargets = lend:(gsBreakTargets gs),
        gsContinueTargets = lstart:(gsContinueTargets gs)
      }
    body <- toCfg block
    -- revert break/continue targets
    modify $ \gs -> gs {
        gsBreakTargets = tail (gsBreakTargets gs),
        gsContinueTargets = tail (gsContinueTargets gs)
      }
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

  -- `for' loop is translated into `while'
  -- FIXME TODO XXX: `continue' doesn't work properly (the incrementation code
  -- is note executed)
  toCfg (StoredPos pos (StmtFor (For ws_head block _))) = do
    let (fp_init, fp_cond, fp_inc) = wsCapMain ws_head
    let while_cond = foldConds (fpToExprs fp_cond)
    let init_stmts = exprsToStmts (fpToExprs fp_init)
    let inc_stmts = exprsToStmts (fpToExprs fp_inc)
    let while_body = Ast.Block $ stmtsFromList (for_body:inc_stmts)
    let while = While (mkWSC $ mkWSC while_cond) (mkWSC $ Right while_body)
                      StdSyntax
    toCfg $ stmtsFromList (init_stmts ++ [StoredPos pos (StmtWhile while)])
   where
     fpToExprs :: ForPart -> [Expr]
     fpToExprs (ForPart (Left _)) = []
     fpToExprs (ForPart (Right xs)) = map wsCapMain xs

     stmtsFromList :: [a] -> IC.Intercal WS a
     stmtsFromList xs = IC.unbreakEnd (map (\x -> ([], x)) xs) []

     exprsToStmts :: [Expr] -> [StoredPos Stmt]
     exprsToStmts = map (\x -> StoredPos pos (StmtExpr x [] StmtEndSemi))

     foldConds :: [Expr] -> Expr
     foldConds []     = makeExprConst "TRUE"
     foldConds [x]    = x
     foldConds (x:xs) = ExprBinOp BAnd x ([],[]) (foldConds xs)

     for_body = case wsCapMain block of
       Left spos -> spos
       Right iblock -> StoredPos pos (StmtBlock iblock)

     mkWSC :: a -> WSCap a
     mkWSC x = WSCap [] x []

  -- An empty `switch' statement is a boring case but we need to handle it
  -- correctly. This is equivalent to just evaluating the switched value.
  toCfg (StoredPos pos (StmtSwitch (Switch _ w2_e _ m_tl []))) =
    toCfg (StoredPos pos (StmtExpr (wsCapMain $ wsCapMain w2_e) [] stend))
   where
     stend = maybe StmtEndSemi (StmtEndClose . wsCapMain) m_tl

  -- TODO: toplevels?
  toCfg (StoredPos pos (StmtSwitch (Switch _ w2_e _ _ cases))) = do
    (r_val, g_val) <- toTacR w2_e pos
    l_end <- freshLabel
    -- interestingly, `continue' behaves just like `break' inside PHP's switch
    modify $ \gs -> gs {
        gsBreakTargets = l_end:gsBreakTargets gs,
        gsContinueTargets = l_end:gsContinueTargets gs
      }
    l_checks <- forM cases (const freshLabel)
    l_cases <- forM cases (const freshLabel)

    -- The list `checks' contains basic blocks that check whether r_val is
    -- equal to the value of an expression given in `case'. Each block passes
    -- control to the next one if the value is not equal, and to a block from
    -- l_cases if it is.
    checks <- forM (zip4 cases l_checks l_cases (tail l_checks ++ [l_end])) $
      \(cs, l_check, l_true, l_false) -> case cs of
        StoredPos pos (Case (Left _) _) -> -- `default:' block
          return (mkLabel l_check <*> mkBranch l_true)
        StoredPos pos (Case (Right w_e) _) -> do -- `case:'
          (r_comp, g_comp) <- toTacR w_e pos
          r_cond <- RTemp <$> freshUnique
          return $ mkLabel l_check
               <*> g_comp
               <*> (mkMiddle $ sp2ip pos $ ICall r_cond CEq (r_val, r_comp))
               <*> (mkLast $ sp2ip pos $ ICondJump r_cond l_true l_false)

    -- `cases' is a similar list of blocks. Each block contains code from the
    -- `case' body and passes control to the next one (which implements the
    -- standard fall-through semantics of `switch').
    cases <- forM (zip3 cases l_cases (tail l_cases ++ [l_end])) $
      \(StoredPos _ (Case _ stmts), l_case, l_next) -> do
        g_stmts <- toCfg stmts
        return (mkLabel l_case <*> g_stmts <*> mkBranch l_next)

    -- we finished generating the code inside `switch'
    modify $ \gs -> gs {
        gsBreakTargets = tail (gsBreakTargets gs),
        gsContinueTargets = tail (gsContinueTargets gs)
      }

    -- form the final graph from blocks in `checks' and `cases'
    return $ (g_val <*> mkBranch (head l_checks))
      |*><*| (foldl1 (|*><*|) (checks ++ cases))
      |*><*| (mkLabel l_end)


  -- TODO: translate StmtEnd properly (top-levels?)
  toCfg (StoredPos pos (StmtBreak m_lvl _ _)) = loopExit gsBreakTargets pos m_lvl
  toCfg (StoredPos pos (StmtContinue m_lvl _ _)) = loopExit gsContinueTargets pos m_lvl

------------------------------------------------------------------------------
--                          Helper functions                                --
------------------------------------------------------------------------------

sp2ip :: Ast.SourcePos -> Instr e x -> InstrPos e x
sp2ip pos = IP (Just (sourceName pos, sourceLine pos))

simpleBinop :: Callable Register (Register, Register)
               -> Expr -> Expr -> Ast.SourcePos -> GMonad (Register, Cfg)
simpleBinop c e1 e2 pos = do
  var <- RTemp <$> freshUnique
  (v_e1, g_e1) <- toTacR e1 pos
  (v_e2, g_e2) <- toTacR e2 pos
  let g_call = mkMiddle $ sp2ip pos $ ICall var c (v_e1, v_e2)
  return (var, g_e1 <*> g_e2 <*> g_call)

negatedBinop :: Callable Register (Register, Register)
                -> Expr -> Expr -> Ast.SourcePos -> GMonad (Register, Cfg)
negatedBinop c e1 e2 pos = do
  r_ret <- RTemp <$> freshUnique
  (r_cond, g1) <- simpleBinop c e1 e2 pos
  (ltrue, lfalse, lend) <- liftM3 (,,) freshLabel freshLabel freshLabel
  let g2 = mkLast $ sp2ip pos $ ICondJump r_cond ltrue lfalse
  let true_block = mkMiddle $ sp2ip pos $ ILoadConst r_ret "FALSE"
  let false_block = mkMiddle $ sp2ip pos $ ILoadConst r_ret "TRUE"
  let graph = (g1 <*> g2)
       |*><*| (mkLabel ltrue <*> true_block <*> mkBranch lend)
       |*><*| (mkLabel lfalse <*> false_block <*> mkBranch lend)
       |*><*| mkLabel lend
  return (r_ret, graph)

simpleUnop :: Callable Register Register -> Expr -> Ast.SourcePos -> GMonad (Register, Cfg)
simpleUnop c e pos = do
  r_out <- RTemp <$> freshUnique
  (r_in, g_e) <- toTacR e pos
  let g_call = mkMiddle $ sp2ip pos $ ICall r_out c r_in
  return (r_out, g_e <*> g_call)

preIncDecOp delta e@(ExprRVal (RValLRVal lv)) pos = do
  (r_e, g_e) <- toTacR e pos
  r_temp <- RTemp <$> freshUnique
  let g_load = mkM (ILoadNum r_temp (show delta))
  let g_add = mkM (ICall r_temp CAdd (r_e, r_temp))
  g_assign <- toTacL lv pos r_temp
  return (r_temp, g_e <*> g_load <*> g_add <*> g_assign)
 where
  mkM = mkMiddle . (sp2ip pos)

postIncDecOp delta e@(ExprRVal (RValLRVal lv)) pos = do
  (r_e, g_e) <- toTacR e pos
  r_temp <- RTemp <$> freshUnique
  r_saved <- RTemp <$> freshUnique
  let g_load = mkM (ILoadNum r_temp (show delta))
  let g_save = mkM (ICopyVar r_saved r_e)
  let g_add = mkM (ICall r_temp CAdd (r_e, r_temp))
  g_assign <- toTacL lv pos r_temp
  return (r_saved, g_e <*> g_load <*> g_save <*> g_add <*> g_assign)
 where
  mkM = mkMiddle . (sp2ip pos)

makeExprConst :: String -> Expr
makeExprConst name =
  ExprRVal $ RValROnlyVal $ ROnlyValConst (Const [] (WSCap [] name []))

-- This implements break/continue statements.  The first argument can be
-- either gsContinueTargets or gsBreakTargets.
loopExit :: (GS -> [Label]) -> SourcePos -> Maybe (WS, Expr) -> GMonad Cfg
loopExit tfun pos m_lvl = do
  targets <- tfun <$> get
  let lvl = case m_lvl of
              Nothing -> 1
              Just (_,e) -> e2const e
  if length targets < lvl || lvl <= 0
    then error "Invalid break/continue parameter."
    else return ()
  lab <- freshLabel
  return $ (mkLast $ sp2ip pos $ IJump (targets !! (lvl-1)))
    |*><*| mkLabel lab
 where
   e2const (ExprNumLit (NumLit x)) = read x :: Int
   e2const (ExprParen wsc) = e2const (wsCapMain wsc)
   e2const (ExprPreOp PrNegate [] e) = -(e2const e)
   e2const _ = error "Break/continue parameter is not a constant expression."
