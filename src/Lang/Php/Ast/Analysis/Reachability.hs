-- | This module implements a very simple reachability analysis, performed by
-- a simple walk over the AST. For each statement (or block) in the program,
-- we assign a minimal "break level", which is an indicator of the possible
-- effect this statement has on the control flow.
--
-- An instruction "break <n>" has a break level of <n> (which is 1 by default
-- if this argument is omitted). A normal instruction, which passes the control
-- to the next statement, has a break level of 0. For "throw" and "return",
-- we assign an infinite break level.
--
-- When control flow comes from different blocks (like it is the cases after an
-- if statement), we take the minimal value. Thus, a value of 0 means that this
-- part of the program *may* pass the control to the next statement but doesn't
-- need to.
--
-- When calculating the break level for a sequence of instructions, we can
-- detect instructions that follow one with break level > 0 and warn about
-- them.

module Lang.Php.Ast.Analysis.Reachability(allAnalyses) where

import Control.Monad(foldM, mapM)

import qualified Data.Intercal as IC
import Lang.Php.Ast
import Lang.Php.Ast.Traversal
import qualified Kinds

-- | Since PHP interpreter seems to expect the argument to "brake" to be a
-- 32-bit signed integer (possibly 64-bit for some systems), we keep break
-- levels as integers and represent infinity by a very large number.
type BreakLevel = Integer

infinity :: BreakLevel
infinity = 2^64

allAnalyses = [checkTL, checkFunc]

checkTL = AstAnalysis () $ \ast@(Ast _ _ st) -> do
    minBreakLevelIC st
    return ast

checkFunc = AstAnalysis () $ \func@(Func _ _ _ _ (Block st)) -> do
    minBreakLevelIC st
    return func

-- | Calculate the minimal break level for a sequence of instructions.
minBreakLevelIC :: IC.Intercal WS (StoredPos Stmt) -> TraverseState () BreakLevel
minBreakLevelIC ic = foldM f 0 (IC.toList2 ic)
 where
    f :: BreakLevel -> StoredPos Stmt -> TraverseState () BreakLevel

    f bk (StoredPos _ (StmtLabel _)) = return 0 -- even if bk > 0
    f bk (StoredPos _ (StmtNothing _)) = return bk -- don't emit any issues

    -- We don't care if StmtBreak is unreachable (since it is sometimes considered
    -- a good practice to put one at the end of 'case', even when it's not
    -- necessary.
    f bk (StoredPos _ (stmt@(StmtBreak _ _ _))) = minBreakLevel stmt

    f bk (StoredPos pos stmt) = if bk > 0 then mkIssue pos stmt >> return 0 -- TODO: break here
                                          else minBreakLevel stmt

    mkIssue pos stmt = withSourcePos pos $ emitIssue Kinds.unreachableCode [unparse stmt] ()

    msg = "It might be possible that this code will never get executed. " ++
          "This may be an indicator of a bug or some sort of redundancy."

minBreakLevelBlockOrStmt :: BlockOrStmt -> TraverseState () BreakLevel
minBreakLevelBlockOrStmt (WSCap _ (Left (StoredPos _ stmt)) _) = minBreakLevel stmt
minBreakLevelBlockOrStmt (WSCap _ (Right (Block stmts)) _) = minBreakLevelIC stmts

-- | Calculate the break level for statements. Since some statements are
-- compound (can contain whole statements blocks), this also needs to happen
-- inside the TraverseState monad.
minBreakLevel :: Stmt -> TraverseState () BreakLevel

minBreakLevel (StmtReturn _ _ _) = return infinity
minBreakLevel (StmtThrow _ _) = return infinity

minBreakLevel (StmtBreak (Just (_, ExprNumLit (NumLit x))) _ _) = return (read x)
minBreakLevel (StmtBreak _ _ _) = return 1 -- safe, this argument cannot be < 1

-- surprisingly, continue works just like break
minBreakLevel (StmtContinue x y z) = minBreakLevel (StmtBreak x y z)

minBreakLevel (StmtIf (If _ ifblocks ifelse)) = do
    let xs = map fst (map ifBlockBlock ifblocks)
    ifblock_levels <- mapM minBreakLevelBlockOrStmt xs
    ifelse_level <- case ifelse of
        Just (bos, _) -> minBreakLevelBlockOrStmt bos
        Nothing  -> return 0
    return (foldl1 min (ifelse_level:ifblock_levels))

minBreakLevel (StmtSwitch (Switch _ _ _ _ cases)) = do
    levels <- mapM minBreakLevelIC (map caseStmtList nakedCases)
    checkSwitchFallthrough (zip cases levels)
    if not hasDefault
     then return 0 -- assume that switch without 'default' can always return
     else return $ max 0 ((foldl1 min levels)-1)
 where
    hasDefault = any (either (const True) (const False) . caseExpr) nakedCases
    nakedCases = [x | (StoredPos _ x) <- cases]

minBreakLevel (StmtWhile (While _ bs _)) = handleLoop bs
minBreakLevel (StmtDoWhile (DoWhile wsbs _ _)) = handleLoop wsbs
minBreakLevel (StmtFor (For _ bs _)) = handleLoop bs
minBreakLevel (StmtForeach (Foreach _ bs _)) = handleLoop bs

minBreakLevel (StmtTry ws_block ic) = do
    xs <- mapM (minBreakLevelIC . block2ic . catchBlock) (IC.toList1 ic)
    x <- minBreakLevelIC (block2ic $ wsCapMain ws_block)
    return (foldl1 min (x:xs))
 where
    block2ic (Block ic) = ic

minBreakLevel _ = return 0

handleLoop bs = do
    bl <- minBreakLevelBlockOrStmt bs
    return (max 0 (bl-1))

-- Check whether each 'case' block has a break level > 0. If this occurs in the
-- last block, it probably is a mistake. Otherwise, it might be the desired
-- behaviour so emit a different kind of issue.
checkSwitchFallthrough :: [(StoredPos Case, BreakLevel)] -> TraverseState () ()
checkSwitchFallthrough levels = case reverse levels of
    [] -> return ()
    ((StoredPos lpos _, llev):xs) -> do
        when (llev < 1) $ withSourcePos lpos $ emitIssue Kinds.switchLastFallthrough [] ()
        forM_ xs $ \(StoredPos pos cs, lev) ->
            -- we don't warn about fallthrough from "empty" cases
            when (lev < 1 && not (isEmpty cs))
                 (withSourcePos pos $ emitIssue Kinds.switchFallthrough [] ())
 where
    isEmpty :: Case -> Bool
    isEmpty cs = case caseStmtList cs of
        IC.Interend _ -> True
        _ -> False
