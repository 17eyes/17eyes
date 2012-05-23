{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}

module Lang.Php.Ast.Traversal (
    TraverseState, runTraverseState, getSourceLine, traverse
) where

import Data.Generics
import Data.Generics.Aliases(mkM)
import Data.Data
import qualified Control.Monad.State as MS

import Lang.Php.Ast

data ExtState st = MkExtState {
    esSourceLine :: Int,
    esState :: st
 }

initialExtState :: st -> ExtState st
initialExtState x = MkExtState {
                        esSourceLine = 0,
                        esState = x
                    }

newtype TraverseState s a =
    MkTS { toState :: (MS.State (ExtState s) a) } deriving Monad

instance MS.MonadState s (TraverseState s) where
    get = MkTS $ esState <$> MS.get
    put x = MkTS $ MS.modify (\es -> es { esState = x })

runTraverseState :: (Data a, Typeable b) => (b -> TraverseState st b) -> st -> a -> (a, st)
runTraverseState f init_state ast = second esState $
    MS.runState (toState $ traverse (mkM f) ast) (initialExtState init_state)

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
