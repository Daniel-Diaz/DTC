
-- | All you need to parse a module.
module Language.Haskell.DTC.Parser
    ( -- * From "Language.Haskell.Exts.Parser"
      ParseResult (..)
    , Module
    , ParseMode (..)
    , defaultParseMode
    , parseModule
    , parseModuleWithMode
      -- * Extras
    , parseModuleWithSrc
     ) where

import Language.Haskell.Exts

-- | Parse a module from a source code file. It throws an error if parsing fails.
parseModuleWithSrc :: FilePath -> ParseMode -> IO Module
parseModuleWithSrc fp pm =
      do str <- readFile fp
         let r = parseModuleWithMode (pm { parseFilename = fp }) str
         case r of
           ParseOk p -> return p
           ParseFailed loc err -> do fail $ concat [ err
                                                   , " at: " , srcFilename loc
                                                   , ":" , show $ srcLine loc
                                                   , ":" , show $ srcColumn loc
                                                    ]

