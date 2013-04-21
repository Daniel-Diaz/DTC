
module Language.Haskell.DTC.DataInfo
     ( DataInfo (..)
     , dataInfo
     , moduleDataInfo
       ) where

import Language.Haskell.Exts
import Data.Maybe

import Language.Haskell.DTC.Mod

-- | Information about names in a data declaration.
data DataInfo =
      DataInfo { dataName  :: Name -- ^ The data type name.
               , consList  :: [ (Name , Int) ] -- ^ Ordinary constructor names, and their number of arguments.
               , rconsList :: [ (Name , [Name]) ] -- ^ Record constructor names and their field names.
                 } deriving Show

-- | Extract a 'DataInfo' from a declaration. Returns 'Nothing' if the argument is not a data declaration.
dataInfo :: Decl -> Maybe DataInfo
dataInfo (DataDecl _ _ _ name _ xs _) = Just $
    DataInfo name (mapMaybe (consName . fromConDecl) xs) (mapMaybe (rconsName . fromConDecl) xs)
      where
       fromConDecl (QualConDecl _ _ _ x) = x
       consName (ConDecl n ys) = Just (n , length ys)
       consName _ = Nothing
       rconsName (RecDecl n ys) = Just (n , map (head . fst) ys)
       rconsName _ = Nothing
dataInfo _ = Nothing

-- | Extract 'DataInfo' from all data declarations in a module.
moduleDataInfo :: Module -> [DataInfo]
moduleDataInfo (Module _ _ _ _ _ _ decls) = mapMaybe dataInfo decls
