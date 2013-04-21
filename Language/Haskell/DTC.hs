
-- | Main module of @DTC@.
module Language.Haskell.DTC
   ( -- * Export of @DTC@ modules
     module Language.Haskell.DTC.Parser
   , module Language.Haskell.DTC.Mod
   , module Language.Haskell.DTC.Class
   , module Language.Haskell.DTC.DataInfo
     -- * Re-export "Language.Haskell.Exts.Pretty"
   , module Language.Haskell.Exts.Pretty
     ) where

import Language.Haskell.DTC.Parser
import Language.Haskell.DTC.Mod
import Language.Haskell.DTC.Class
import Language.Haskell.DTC.DataInfo

import Language.Haskell.Exts.Pretty

