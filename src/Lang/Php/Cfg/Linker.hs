module Lang.Php.Cfg.Linker where

import Lang.Php.Cfg.Types
import Lang.Php.Cfg.Utils
import Codebase

linkCfg :: Codebase' -> FilePath -> Cfg
linkCfg = undefined

firstSafeLabel :: (NonLocal n, CaseOC n) => Graph n e x -> Int
firstSafeLabel graph = 1 + (foldGraphNodes op graph 0)
 where
   op node x = caseOC node (const x) (const x) (opCx x) (opCx x)
   opCx x n = max x (uniqueToInt $ lblToUnique $ entryLabel n)
