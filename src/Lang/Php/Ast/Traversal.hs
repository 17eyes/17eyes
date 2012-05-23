{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}

module Lang.Php.Ast.Traversal (
    TraverseState, getSourceLine, traverse, emitIssue, runAstAnalysis
) where

import Data.Generics
import Data.Generics.Aliases(mkM)
import Data.Data
import qualified Control.Monad.State as MS

import Lang.Php.Ast
import Common

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

runAstAnalysis :: Typeable b => (b -> TraverseState st b) -> st -> Ast -> [Issue]
runAstAnalysis f init_state ast@(Ast fp _ _) =
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

emitIssue :: Issue -> TraverseState s ()
emitIssue issue = do
    es <- MkTS $ MS.get
    let issue' = issue {
        issueFileName = (issueFileName issue) `mplus` (Just $ esSourceFile es),
        issueLineNumber = (issueLineNumber issue) `mplus` (Just $ esSourceLine es)
    }
    MkTS $ MS.put $ es { esIssues = issue':(esIssues es) }
    return ()
