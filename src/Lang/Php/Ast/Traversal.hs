{-
Copyright (c) 2015 17eyes.com

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, RankNTypes, ExistentialQuantification #-}

module Lang.Php.Ast.Traversal (
    TraverseState, getSourceLine, traverse, emitIssue, runAstAnalysis, AstAnalysis(..), withSourcePos
) where

import Data.Generics
import Data.Generics.Aliases(mkM)
import Data.Data
import qualified Control.Monad.State as MS

import Lang.Php.Ast
import Common

import qualified Issue
import Issue(Issue(Issue))

data ExtState st = MkExtState {
    esSourceFile :: FilePath,
    esSourceLine :: Int,
    esIssues :: [Issue],
    esState :: st
 }

initialExtState :: FilePath -> st -> ExtState st
initialExtState fp x = MkExtState {
                        esSourceFile = fp,
                        esSourceLine = 0,
                        esIssues = [],
                        esState = x
                    }

newtype TraverseState s a =
    MkTS { toState :: (MS.State (ExtState s) a) } deriving Monad

instance MS.MonadState s (TraverseState s) where
    get = MkTS $ esState <$> MS.get
    put x = MkTS $ MS.modify (\es -> es { esState = x })

data AstAnalysis = forall a st. Typeable a => AstAnalysis st (a -> TraverseState st a)

runAstAnalysis :: Ast -> AstAnalysis -> [Issue]
runAstAnalysis ast@(Ast fp _ _) (AstAnalysis init_state f) =
    esIssues $ snd $ MS.runState (toState $ traverse (mkM f) ast)
                                 (initialExtState fp init_state)

traverse :: GenericM (TraverseState s) -> GenericM (TraverseState s)
traverse f x = do
    maybe (return ()) update_pos (cast x)
    x' <- f x
    gmapM (traverse f) x'
 where
    update_pos :: StoredPos Stmt -> TraverseState s ()
    update_pos (StoredPos pos _) =
        MkTS $ MS.modify (\es -> es { esSourceLine = sourceLine pos })

getSourceLine :: TraverseState s Int
getSourceLine = MkTS $ MS.get >>= return . esSourceLine

-- | Constructs and emits an issue based on kind, context and payload. Source
-- name and line are taken from the 'current source position' values in the
-- monadic state (`esSourceFile' and `esSourceLine').
emitIssue :: (Issue.Kind a) -> [String] -> a -> TraverseState s ()
emitIssue kind context payload = do
    es <- MkTS $ MS.get
    let issue = Issue kind (Just $ esSourceFile es) (Just $ esSourceLine es)
                      context payload
    MkTS $ MS.put $ es { esIssues = issue:(esIssues es) }
    return ()

-- | Execute a given action in the `TraverseState' monad with a different
-- 'current source position' value. Useful in conjunction with `emitIssue'.
withSourcePos :: SourcePos -> TraverseState s a -> TraverseState s a
withSourcePos pos action = do
    es <- MkTS $ MS.get
    MkTS $ MS.modify $ \es' -> es' {
        esSourceLine = sourceLine pos,
        esSourceFile = sourceName pos
    }
    result <- action
    MkTS $ MS.modify $ \es' -> es' {
        esSourceLine = esSourceLine es,
        esSourceFile = esSourceFile es
    }
    return result
