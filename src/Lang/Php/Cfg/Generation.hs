{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts,
             UndecidableInstances #-}

module Lang.Php.Cfg.Generation where

import Lang.Php.Cfg.Types
import qualified Data.Intercal as IC

import qualified Lang.Php.Ast as Ast
import Lang.Php.Ast hiding ((<*>), Label, get, put)

import Lang.Php.Type.PhpNum

import Control.Monad
import Control.Monad.State
import Data.Functor
import Compiler.Hoopl
import Text.Parsec.Pos(newPos)

import qualified Data.Map as Map
import Data.Map(Map)

-- Since the generation of the CFG is not fully implemented for all AST nodes
-- and we will probably need an IUnknown instruction anyway, we might as well
-- generate it for unimplemented constructs.
--
-- However, we'll use Debug.Trace to emit a warning message when such
-- instruction is generated.
import Debug.Trace(trace)

class NotImplemented a where
  notImplemented :: String -> a

instance NotImplemented Cfg where
  notImplemented feature = trace msg mkMiddle (IP Nothing IUnknown)
    where msg = "WARNING: " ++ feature ++ " are not implemented"

instance NotImplemented (Register, Cfg) where
  notImplemented x = (RNull, notImplemented x)

-- Some code in this module generates Exprs. There is no reasonable equivalent
-- for IUnknown here so we signal an error.
instance NotImplemented Expr where
  notImplemented feature = error ("ERROR: " ++ feature ++ " are not implemented")

instance NotImplemented a => NotImplemented (GMonad a) where
  notImplemented = return . notImplemented

instance NotImplemented a => NotImplemented (b -> a) where
  notImplemented feature _ = notImplemented feature

-- In top-level code, the order of function or class declarations is not
-- significant.  This is implemented by storing IDeclare instructions in the
-- state GS and prepending them at the beginning of the graph afterwards.
data TLDecl = NotTopLevel | TLDecl [Declarable]

data GS = GS {
    gsBreakTargets :: [Label],
    gsContinueTargets :: [Label],
    gsTopLevelDecls :: TLDecl
}

initialGS = GS {
    gsBreakTargets = [],
    gsContinueTargets = [],
    gsTopLevelDecls = NotTopLevel
}

type GMonad = StateT GS SimpleUniqueMonad

-- runGMonad returns a list of declarations apart from the main value
runGMonad :: GMonad a -> (a, [Declarable])
runGMonad x = (result, decls)
 where
   (result, gs) = runSimpleUniqueMonad (runStateT x initialGS)
   decls = case gsTopLevelDecls gs of
     NotTopLevel -> []
     TLDecl xs -> xs

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
  toTacR (ExprNumLit (NumLit x _)) pos = do
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
   where 
    e' = ExprBinOp (BByable BPlus) (ExprNumLit (NumLit "0" (PhpNum $ Left 0)))
      ([],[]) e

  toTacR (ExprPreOp PrSuppress _ e) pos = do
    r_prev <- RTemp <$> freshUnique
    let g_set = mkMiddle $ sp2ip pos $ ICall r_prev CErrorsSuppress ()
    (r_e, g_e) <- toTacR e pos
    r <- RTemp <$> freshUnique
    -- This copy operation is necessary since the register r_e may be an
    -- undefined variable. If we were to return the same register it may
    -- generate a warning which would normally be suppressed.
    let g_copy = mkMiddle $ sp2ip pos $ ICopyVar r r_e
    let g_restore = mkMiddle $ sp2ip pos $ ICall RNull CErrorsRestore r_prev
    return (r, g_set <*> g_e <*> g_copy <*> g_restore)

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

  toTacR (ExprStrLit sl) pos = toTacR sl pos

  toTacR (ExprBackticks sl) pos = do
    (r_sl, g_sl) <- toTacR sl pos
    r_shell <- RTemp <$> freshUnique
    let g_shell = mkMiddle $ sp2ip pos $ ICall r_shell (CPhp "shell_exec") [r_sl]
    return (r_shell, g_sl <*> g_shell)

  toTacR (ExprCast ws_type _ e) pos = do
    (r_e, g_e) <- toTacR e pos
    r_res <- RTemp <$> freshUnique
    let g_call = mkMiddle $ sp2ip pos $ ICall r_res CCast (wsCapMain ws_type, r_e)
    return (r_res, g_e <*> g_call)

  -- Array literals may list both values and key-value pairs. They are transla-
  -- -ted to a sequence of CArraySet or CArrayPush on a fresh array created
  -- using CArrayEmpty. The initialization list is translated using a following
  -- algorithm:
  --
  -- $x = array(..., k => v)    is equivalent to    $x = array(...); $x[k] = v;
  -- $x = array(..., v)         is equivalent to    $x = array(...); $x[] = v;
  toTacR (ExprArray _ elem_spec) pos = do
    r <- RTemp <$> freshUnique
    let g_init = mkMiddle $ sp2ip pos $ ICall r CArrayEmpty ()
    g_sets <- foldl (<*>) emptyGraph <$> mapM (gen r) elems
    return (r, g_init <*> g_sets)
   where
     gen :: Register -> (Maybe Expr, Expr) -> GMonad Cfg
     gen r (Just e_key, e_val) = do
       (r_key, g_key) <- toTacR e_key pos
       (r_val, g_val) <- toTacR e_val pos
       let g_set = mkMiddle $ sp2ip pos $ ICall RNull CArraySet (r, r_key, r_val)
       return (g_key <*> g_val <*> g_set)

     gen r (Nothing, e_val) = do
       (r_val, g_val) <- toTacR e_val pos
       let g_push = mkMiddle $ sp2ip pos $ ICall RNull CArrayPush (r, r_val)
       return (g_val <*> g_push)

     elems = case elem_spec of
       Left _ -> []
       Right (xs, _) -> map (unArrow . wsCapMain) xs

     unArrow :: DubArrowMb -> (Maybe Expr, Expr)
     unArrow (DubArrowMb (Just (e_key, _)) e_val) = (Just e_key, e_val)
     unArrow (DubArrowMb Nothing e_val) = (Nothing, e_val)

  -- `empty($x)' is translated like `(boolean) $x'
  --
  -- FIXME: empty($x) doesn't warn if $x is not declared. This kind of beha-
  -- -vior is currently not preserved.
  toTacR (ExprEmpty _ ws_lrv) pos = do
    (r_lrv, g_lrv) <- toTacR ws_lrv pos
    r <- RTemp <$> freshUnique
    let g_c = mkMiddle $ sp2ip pos $ ICall r CCast ("boolean", r_lrv)
    return (r, g_lrv <*> g_c)

  toTacR (ExprEval _ ws_e) pos = do
    (r_e, g_e) <- toTacR ws_e pos
    r <- RTemp <$> freshUnique
    let g_c = mkMiddle $ sp2ip pos $ ICall r CEval r_e
    return (r, g_e <*> g_c)

  toTacR (ExprIsset _ ws_lrvals) pos = do
    (rs, gs) <- unzip <$> forM ws_lrvals (\x -> toTacR x pos)
    r <- RTemp <$> freshUnique
    let g_c = mkMiddle $ sp2ip pos $ ICall r CIsset rs
    return (r, foldr (<*>) g_c gs)

  toTacR (ExprInclude ir on _ e) pos = do
    (r_e, g_e) <- toTacR e pos
    r <- RTemp <$> freshUnique
    let g_c = mkMiddle $ sp2ip pos $ ICall r (CInclude ir on) r_e
    return (r, g_e <*> g_c)

  toTacR _ pos = notImplemented "some kinds of expressions"

instance TacAbleR RVal where
  toTacR (RValROnlyVal x) = toTacR x
  toTacR (RValLRVal x) = toTacR x

instance TacAbleR ROnlyVal where
  toTacR (ROnlyValConst (Const [] w_name)) pos = do
    var <- RTemp <$> freshUnique
    return (var, mkMiddle $ sp2ip pos $ ILoadConst var (wsCapMain w_name))

  toTacR (ROnlyValConst _) pos = notImplemented "class memeber constants"

  -- Ordinary function call: function name is a constant, parameters are r-values.
  toTacR (ROnlyValFunc (Right const) _ ws_args) pos = do
    (r_args, g_args) <- unzip <$> mapM (\x -> toTacR x pos) args
    r_res <- RTemp <$> freshUnique
    let graph = (foldl (<*>) emptyGraph g_args)
            <*> (mkMiddle $ sp2ip pos $ ICall r_res callable r_args)
    return (r_res, graph)
   where
     callable :: Callable t [t]
     callable = case const of
       Const [] wsc -> CPhp (wsCapMain wsc)
       Const wcls wsc -> CPhpStatic (map wsCapMain wcls) (wsCapMain wsc)

     args :: [Expr]
     args = case ws_args of
       Left _ -> []
       Right xs -> map (asExpr . wsCapMain) xs

     asExpr :: Either Expr LVal -> Expr
     asExpr (Left x) = x
     asExpr (Right lv) = notImplemented "l-values as function arguments"

  toTacR (ROnlyValFunc (Left _) _ _) pos =
    notImplemented "complex expressions as functions in function calls"

-- Not that this translates an LRVal when used as an r-value. L-values are
-- handled differently.
instance TacAbleR LRVal where
  toTacR (LRValVar (DynConst [] wsc)) = case wsCapMain wsc of
    (Left _) -> notImplemented "TacAbleR LRVal"
    (Right x) -> toTacR x

  toTacR _ = notImplemented "TacAbleR LRVal"

instance TacAbleR Var where
  toTacR (Var name []) pos = return (RVar name, emptyGraph)

  toTacR (Var name xs) pos = do
    let idx_exprs = map (wsCapMain . snd . snd) xs
    r <- RTemp <$> freshUnique
    let g_init = mkMiddle $ sp2ip pos $ ICopyVar r (RVar name)
    g_gets <- forM idx_exprs $ \e -> do
      (r_idx, g_idx) <- toTacR e pos
      let g = mkMiddle $ sp2ip pos $ ICall r CArrayGet (r, r_idx)
      return (g_idx <*> g)
    return (r, g_init <*> (foldl (<*>) emptyGraph g_gets))

  toTacR (VarDyn _ x) pos = notImplemented "VarDyn ($$x etc.)"
  toTacR (VarDynExpr _ x) pos = notImplemented "VarDynExpr (${foo()} etc)"

instance TacAbleR StrLit where
  -- Basic case of a simple string without embedded expressions.
  toTacR (StrLit (IC.Interend str)) pos = do
    r <- RTemp <$> freshUnique
    let str_noquote = tail $ init $ str
    return (r, mkMiddle $ sp2ip pos $ ILoadString r str_noquote)

  -- This is a bit overcomplicated since Strings inside StrLits hold also the
  -- quoting characters (" or ').  We strip those from the beginning and the
  -- end and translate:
  --
  --      "foo $a bar $b baz" ===> "foo " . $a . " bar " . $b . " baz"
  --
  -- Then, we invoke toTacR on such expression.  Strings inside it are always
  -- simple and get handled by the basic case above.
  toTacR (StrLit ic) pos = do
    let (StrLit ic') = strLitStripQuotes (StrLit ic)
    let (xs, str) = IC.breakEnd ic'
    toTacR (foldr comb (lit str) xs) pos
   where
     cat e1 e2 = ExprBinOp (BByable BConcat) e1 ([],[]) e2
     lit x = ExprStrLit (StrLit (IC.Interend ('"':x ++ "\"")))
     comb (str, (_, rval)) e = (lit str) `cat` (ExprRVal rval) `cat` e

instance TacAbleR TopLevel where
  toTacR (TopLevel str Nothing) pos = do
    (reg, graph) <- toTacR (strLitExpr str) pos
    return (RNull, graph <*> (mkMiddle $ IP Nothing (ICall RNull CPrint reg)))

  toTacR (TopLevel str (Just (Right _))) pos = toTacR (TopLevel str Nothing) pos

  -- <?= style tags
  toTacR (TopLevel str (Just (Left (ws_expr, stmt_end)))) pos = do
    -- first, just print the main textual content
    (_, g_str) <- toTacR (TopLevel str Nothing) pos
    -- next, print the expression inside the tags
    (r_expr, g_expr) <- toTacR (wsCapMain ws_expr) pos
    let g_print = mkMiddle $ sp2ip pos $ ICall RNull CPrint r_expr
    -- finally, there is possibly an another TopLevel inside the StmtEnd
    (_, g_stmt_end) <- toTacR stmt_end pos
    return (RNull, g_str <*> g_expr <*> g_print <*> g_stmt_end)

instance TacAbleR StmtEnd where
  toTacR StmtEndSemi _ = return (RNull, emptyGraph)
  toTacR (StmtEndClose top_level) pos = toTacR top_level pos

------------------------------------------------------------------------------
--                              L-Values                                    --
------------------------------------------------------------------------------

instance TacAbleL LVal where
  toTacL (LValLOnlyVal x) = toTacL x
  toTacL (LValLRVal x) = toTacL x

instance TacAbleL LOnlyVal where
  toTacL (LOnlyValAppend (LValLRVal lrv) _) pos var = do
    (r_lrt, g_lrt) <- toTacR lrv pos
    let g_append = mkMiddle $ sp2ip pos $ ICall RNull CArrayPush (r_lrt, var)
    return (g_lrt <*> g_append)

  toTacL (LOnlyValAppend (LValLOnlyVal lrv) _) pos var = do
    r_arr <- RTemp <$> freshUnique
    let g_create = mkMiddle $ sp2ip pos $ ICall r_arr CArrayEmpty ()
    let g_append = mkMiddle $ sp2ip pos $ ICall RNull CArrayPush (r_arr, var)
    g_assign <- toTacL lrv pos r_arr
    return $ g_create <*> g_append <*> g_assign

  -- a silly case: list() = <expression>; just discard the value
  toTacL (LOnlyValList _ (Left _)) _ _ = return emptyGraph

  -- NOTE: list(...) assigns values starting from the rightmost parameter
  toTacL (LOnlyValList _ (Right xs)) pos var = do
    graphs <- forM (reverse $ zip [0..] xs) $ \(idx, x) -> case x of
      Left _ -> return emptyGraph
      Right ws_lval -> do
        r <- RTemp <$> freshUnique
        r_idx <- RTemp <$> freshUnique
        let g_idx_load = mkMiddle $ sp2ip pos $ ILoadNum r_idx (show idx)
        let g_get = mkMiddle $ sp2ip pos $ ICall r CArrayGet (var, r_idx)
        g_main <- toTacL (wsCapMain ws_lval) pos r
        return $ g_idx_load <*> g_get <*> g_main
    return (foldl1 (<*>) graphs)

  toTacL (LOnlyValInd lov _ ws_e) pos var = notImplemented "LOnlyValInd (e.g. $x[][5] = 5)"
  toTacL (LOnlyValMemb lov _ memb) pos var = notImplemented "LOnlyValMemb (e.g. $x[]->a = 5)"

instance TacAbleL DynConst where
  toTacL (DynConst [] wsc) = case wsCapMain wsc of
    Right var -> toTacL var
    _ -> error "inconsistent AST" -- should not happen

  toTacL (DynConst xs x) = notImplemented "DynConst (A::$x = 5 etc.)"

instance TacAbleL LRVal where
  toTacL (LRValVar dc) = toTacL dc
  toTacL (LRValInd rval _ ws_expr) = notImplemented "LRValInd (e.g. foo()[0] = 5)"
  toTacL (LRValMemb rval _ memb) = notImplemented "LRValMemb (e.g. foo()->a = 5)"

instance TacAbleL Var where
  toTacL (Var name []) pos var =
    return $ mkMiddle $ sp2ip pos $ ICopyVar (RVar name) var

  -- This is a special case with only one index. It could be handled by the
  -- code below but it's fairly common and we would like to generate a clearer
  -- CFG without redundant registers.
  toTacL (Var name [(_, (_, ws_idx))]) pos var = do
    (r_idx, g_idx) <- toTacR (wsCapMain ws_idx) pos
    let g_set = mkMiddle $ sp2ip pos $ ICall RNull CArraySet (RVar name, r_idx, var)
    return (g_idx <*> g_set)

  -- This is a weird kind of node in the AST. It corresponds to expressions
  -- like $x[0][1][2] (`xs' here holds the indices). It seems redundant (we
  -- already have LRValInd).
  toTacL (Var name xs) pos var = do
    let idx_exprs = map (wsCapMain . snd . snd) xs
    r_arr <- RTemp <$> freshUnique
    let g_init = mkMiddle $ sp2ip pos $ ICopyVar r_arr (RVar name)
    g_gets <- forM (init idx_exprs) $ \e -> do
      (r_idx, g_idx) <- toTacR e pos
      let g_get = mkMiddle $ sp2ip pos $ ICall r_arr CArrayGet (r_arr, r_idx)
      return (g_idx <*> g_get)
    (r_idx, g_idx) <- toTacR (last idx_exprs) pos
    let g_set = mkMiddle $ sp2ip pos $ ICall RNull CArraySet (r_arr, r_idx, var)
    return $ g_init <*> (foldl (<*>) emptyGraph g_gets) <*> g_idx <*> g_set

  toTacL (VarDyn _ x) pos var = notImplemented "VarDyn ($$x etc.)"
  toTacL (VarDynExpr _ x) pos var = notImplemented "VarDynExpr (${foo()} etc)"

instance TacAbleL Ref where
  toTacL = notImplemented "references"

------------------------------------------------------------------------------
--                              Statements                                  --
------------------------------------------------------------------------------

instance CfgAble Ast where
  toCfg (Ast file toplevel sl) = do
    modify (\x -> x { gsTopLevelDecls = TLDecl [] })
    let pos = newPos file 0 0
    (_, g_toplevel) <- toTacR toplevel pos
    g_main <- toCfg sl
    g_decls <- catGraphs <$> reverse <$> fromTLD <$> gsTopLevelDecls <$> get
    -- g_decls is a graph with declarations for all top-level functions
    return (g_decls <*> g_toplevel <*> g_main)
   where
     fromTLD (TLDecl xs) = map (mkMiddle . IP Nothing . IDeclare) xs

instance CfgAble (StoredPos Stmt) where
  toCfg (StoredPos _ (StmtBlock b)) = toCfg b
  toCfg (StoredPos pos (StmtNothing stmt_end)) = snd <$> toTacR stmt_end pos

  toCfg (StoredPos pos (StmtExpr e _ stmt_end)) = do
    (_, g_e) <- toTacR e pos -- variable is discarded
    (_, g_end) <- toTacR stmt_end pos
    return (g_e <*> g_end)

  toCfg (StoredPos pos (StmtEcho args stmt_end)) = do
    (vars, gs1) <- fmap unzip $ forM args $
      \wexpr -> toTacR (wsCapMain wexpr) pos
    let gs2 = map (mkMiddle . sp2ip pos . ICall RNull CPrint) vars
    (_, g_end) <- toTacR stmt_end pos
    return $ foldl (<*>) emptyGraph (gs1 ++ gs2 ++ [g_end])

  toCfg (StoredPos pos (StmtWhile (While wexpr block _))) = do
    lstart <- freshLabel
    lbody <- freshLabel
    lend <- freshLabel
    -- setup break/continue targets before entering the loop body
    pushTargets lend lstart
    body <- toCfg block
    popTargets -- revert break/continue targets
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

  -- Code generated for `foreach' is composed of 5 basic blocks -- b_init calls
  -- CGetItr, b_cond checks whether the loop can still continue using
  -- CItrValid, b_body assigns the current value of the iterator to the
  -- variables specified by the user.  In b_cont, we move the iterator to the
  -- next item.  A `break' statement inside the body jumps to b_break;
  -- `continue' -- to b_cont.
  --
  -- b_init -> b_cond ----> b_break
  --            ^   |
  --            |   +-----> b_body
  --            |             |
  --           b_cont <-------+
  toCfg (StoredPos pos (StmtForeach (Foreach head body _))) = do
    (l_cont, l_break, l_cond, l_body) <- liftM4 (,,,) freshLabel freshLabel freshLabel freshLabel
    let (ws_obj, mws_key, ws_curr) = wsCapMain head

    (r_obj, g_obj) <- toTacR (wsCapMain ws_obj) pos
    r_itr <- RTemp <$> freshUnique
    let b_init = g_obj
             <*> (mkMiddle $ sp2ip pos $ ICall r_itr CItrGet r_obj)
             <*> mkBranch l_cond

    r_cond <- RTemp <$> freshUnique
    let b_cond = mkLabel l_cond
             <*> (mkMiddle $ sp2ip pos $ ICall r_cond CItrValid r_itr)
             <*> (mkLast $ sp2ip pos $ ICondJump r_cond l_body l_break)

    let b_cont = mkLabel l_cont
             <*> (mkMiddle $ sp2ip pos $ ICall r_itr CItrNext r_itr)
             <*> mkBranch l_cond

    r_val <- RTemp <$> freshUnique
    let g_init_curr = mkMiddle $ sp2ip pos $ ICall r_val CItrCurrent r_itr
    g_assign_curr <- toTacL (wsCapMain ws_curr) pos r_val

    (g_init_key, g_assign_key) <- case mws_key of
      Nothing -> return (emptyGraph, emptyGraph)
      Just ws_key -> do
        r_key <- RTemp <$> freshUnique
        let g_init = mkMiddle $ sp2ip pos $ ICall r_key CItrKey r_itr
        g_assign <- toTacL (wsCapMain ws_key) pos r_key
        return (g_init, g_assign)

    pushTargets l_break l_cont
    g_body <- toCfg body
    popTargets
    let b_body = mkLabel l_body
             <*> g_init_curr <*> g_assign_curr
             <*> g_init_key <*> g_assign_key
             <*> g_body
             <*> mkBranch l_cont

    return (b_init |*><*| b_cont |*><*| b_cond |*><*| b_body |*><*| (mkLabel l_break))

  -- An empty `switch' statement is a boring case but we need to handle it
  -- correctly. This is equivalent to just evaluating the switched value.
  --
  -- There is an annoying possibility of a TopLevel inside the switch, before
  -- the first case. This is to make the colon-syntax for switch more
  -- convenient to write. PHP interpreter parses it but just ignores it.
  toCfg (StoredPos pos (StmtSwitch (Switch _ w2_e _ m_tl []))) =
    toCfg (StoredPos pos (StmtExpr (wsCapMain $ wsCapMain w2_e) [] stend))
   where
     stend = maybe StmtEndSemi (StmtEndClose . wsCapMain) m_tl

  toCfg (StoredPos pos (StmtSwitch (Switch _ w2_e _ m_ws_tl cases))) = do
    (r_val, g_val) <- toTacR w2_e pos
    l_end <- freshLabel
    -- interestingly, `continue' behaves just like `break' inside PHP's switch
    pushTargets l_end l_end
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

    popTargets -- we finished generating the code inside `switch'
    -- form the final graph from blocks in `checks' and `cases'
    return $ (g_val <*> mkBranch (head l_checks))
      |*><*| (foldl1 (|*><*|) (checks ++ cases))
      |*><*| (mkLabel l_end)

  -- TODO: translate StmtEnd properly (top-levels?)
  toCfg (StoredPos pos (StmtBreak m_lvl _ stmt_end)) = loopExit gsBreakTargets pos m_lvl stmt_end
  toCfg (StoredPos pos (StmtContinue m_lvl _ stmt_end)) = loopExit gsContinueTargets pos m_lvl stmt_end

  -- It is not clear how to represent try-catch blocks so that dataflow
  -- analyses will be easy to write. Currently, the problem with control flow
  -- coming to the catch block is handled by 'special' label-like instruction
  -- ICatchException. I guess we'll see how convenient this will turn out to
  -- be.
  toCfg (StoredPos pos (StmtTry ws_block ic_catch)) = do
    lab_end <- freshLabel
    catches <- mapM (mkCatchBlock lab_end) (IC.toList1 ic_catch)
    g_block <- toCfg (wsCapMain ws_block)
    return $ (g_block <*> mkBranch lab_end)
      |*><*| (foldl (|*><*|) emptyClosedGraph catches)
      |*><*| (mkLabel lab_end)
   where
     mkCatchBlock lab_end (Catch ws_hdr _ block) = do
       let exception_type = case (wsCapMain $ fst $ wsCapMain ws_hdr) of
                                 Const [] name -> wsCapMain name
                                 _ -> error "TODO: namespaces for exceptions not supported"
       let exception_expr = snd $ wsCapMain ws_hdr
       r_exception <- RTemp <$> freshUnique
       lab_this <- freshLabel
       let g_catch = mkFirst $ sp2ip pos $
                     ICatchException lab_this r_exception exception_type
       g_assign <- case exception_expr of
                  (ExprRVal (RValLRVal lrval)) -> toTacL lrval pos r_exception
                  _ -> error "TODO: inconsistency in the AST?"
       g_main <- toCfg block
       return (g_catch <*> g_assign <*> g_main <*> (mkBranch lab_end))

  toCfg (StoredPos pos (StmtThrow ws_e _)) = do
    (r_e, g_e) <- toTacR ws_e pos
    let g_throw = mkLast $ sp2ip pos $ IThrow r_e
    lab_dead <- freshLabel
    return (g_e <*> g_throw |*><*| mkLabel lab_dead)

  -- For function definitions, we translate the function body into another
  -- block (with a special entry node IFuncEntry which handles the formal
  -- parameters). Functions at the top level are somewhat problematic, since
  -- they behave as if they have been always declared at the beginning of the
  -- file (this is a deliberate feature of the language probably designed this
  -- way to make it more user-friendly). Helper function `declare' handles
  -- that.
  toCfg (StoredPos pos (StmtFuncDef func)) = do
    (func_lab, g_body) <- createFuncGraph pos func
    g_inj <- injectBlocks g_body
    let Just name = funcName func
    g_decl <- declare (DFunction name func_lab g_body)
    return (g_decl <*> g_inj)

  toCfg (StoredPos pos (StmtClass cls)) = do
    (labs, bodies) <- unzip <$> mapM (createFuncGraph pos . snd) methods
    g_inj <- injectBlocks (foldl (|*><*|) emptyClosedGraph bodies)
    g_decl <- declare $ DClass {
      dclsName = wsCapMain (className cls),
      dclsMethods = zip4 visibilities
                         (map (maybe "" id . funcName . snd) methods)
                         labs
                         bodies,
      dclsFields = fields
    }
    return (g_decl <*> g_inj)
   where
     cstmts = case classBlock cls of
       Ast.Block ic -> [x | StoredPos _ x <- IC.toList2 ic]

     visibilities = map (mods2vis . fst) methods
     methods = [(map fst mods, func) | CStmtFuncDef mods func <- cstmts]

     fields = concat [mkFields (IC.toList1 mods) vars | CStmtVar mods vars _ <- cstmts]
     mkFields mods vars = zip (repeat (mods2vis mods)) names
      where
        -- TODO: support initializers somehow
        names = [x | VarMbVal (Var x _) _ <- map wsCapMain vars]

------------------------------------------------------------------------------
--                         Handling declarations
------------------------------------------------------------------------------

declare :: Declarable -> GMonad Cfg
declare dec = do
  tld <- gsTopLevelDecls <$> get
  case tld of
    NotTopLevel -> return (mkMiddle $ IP Nothing (IDeclare dec))
    TLDecl xs -> do
      modify $ \x -> x { gsTopLevelDecls = TLDecl (dec:xs) }
      return emptyGraph

-- Add closed blocks to the CFG. Similar to addBlocks from Hoopl but addBlocks
-- unfortunately works only on AGraphs.
--
-- FIXME: Would it be possible to implement it without redundant labels and
-- jumps?
injectBlocks :: Graph InstrPos C C -> GMonad Cfg
injectBlocks blcks = do
  rest_lab <- freshLabel
  return $ (mkBranch rest_lab)
    |*><*| blcks
    |*><*| (mkLabel rest_lab)

-- Create CFG for the function body.
createFuncGraph :: SourcePos -> Func -> GMonad (Label, Graph InstrPos C C)
createFuncGraph pos func = do
  func_lab <- freshLabel
  old_tld <- gsTopLevelDecls <$> get
  modify $ \x -> x { gsTopLevelDecls = NotTopLevel }
  body_g <- toCfg (funcBlock func)
  modify $ \x -> x { gsTopLevelDecls = old_tld }
  let Just name = funcName func
  init_g <- initGraph
  let res_g = (mkFirst $ sp2ip pos $ IFuncEntry name (map (RVar . fst) params) func_lab)
          <*> init_g
          <*> body_g
          <*> (mkLast $ sp2ip pos $ IReturn Nothing)
  return (func_lab, res_g)
 where
   -- graph with initializers created based on the default values of the
   -- function arguments in the declaration
   initGraph :: GMonad Cfg
   initGraph = fmap catGraphs $ forM params $ \(name, m_expr) ->
     case m_expr of
       Nothing -> return emptyGraph
       Just expr -> notImplemented "default functions arguments"

   -- list of all formal parameters (extracted for convenience)
   params :: [(String, Maybe Ast.Expr)]
   params = either (const []) (map (procArg . wsCapMain))
                   (wsCapMain (funcArgs func))

   procArg (FuncArg { funcArgVar = VarMbVal (Var name []) mb_wse }) = (name, fmap snd mb_wse)
   procArg _ = ("", Just $ notImplemented "crazy stuff as formal parameters")

mods2vis :: [String] -> Visibility
mods2vis ("public":_) = Public
mods2vis ("private":_) = Private
mods2vis ("protected":_) = Protected
mods2vis (_:xs) = mods2vis xs
mods2vis [] = Public

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
loopExit :: (GS -> [Label]) -> SourcePos -> Maybe (WS, Expr) -> StmtEnd -> GMonad Cfg
loopExit tfun pos m_lvl stmt_end = do
  targets <- tfun <$> get
  let lvl = case m_lvl of
              Nothing -> 1
              Just (_,e) -> e2const e
  if length targets < lvl || lvl <= 0
    then error "Invalid break/continue parameter."
    else return ()
  lab <- freshLabel
  (_, g_stmt_end) <- toTacR stmt_end pos
  return $ (mkLast $ sp2ip pos $ IJump (targets !! (lvl-1)))
    |*><*| (mkLabel lab <*> g_stmt_end)
 where
   e2const (ExprNumLit (NumLit x _)) = read x :: Int
   e2const (ExprParen wsc) = e2const (wsCapMain wsc)
   e2const (ExprPreOp PrNegate [] e) = -(e2const e)
   e2const _ = error "Break/continue parameter is not a constant expression."

pushTargets :: Label -> Label -> GMonad ()
pushTargets break continue = modify $ \gs -> gs {
    gsContinueTargets = continue:gsContinueTargets gs,
    gsBreakTargets = break:gsBreakTargets gs
  }

popTargets :: GMonad ()
popTargets = modify $ \gs -> gs {
    gsContinueTargets = tail (gsContinueTargets gs),
    gsBreakTargets = tail (gsBreakTargets gs)
  }

strLitExpr :: String -> Expr
strLitExpr str = ExprStrLit $ StrLit $ IC.Interend ('\'':str ++ "'")

-- StrLit nodes in the AST contain quoting characters at the beginning and the
-- end.  Storing this in the CFG representation isn't necessary so this
-- function is used to strip the quotes.
strLitStripQuotes :: StrLit -> StrLit
strLitStripQuotes (StrLit ic) = StrLit $ strip_last $ strip_first ic
 where
   strip_first (IC.Interend (_:xs)) = IC.Interend xs
   strip_first (IC.Intercal (_:xs) e ic') = IC.Intercal xs e ic'

   strip_last (IC.Interend xs) = IC.Interend (init xs)
   strip_last (IC.Intercal xs e ic') = IC.Intercal xs e (strip_last ic')
