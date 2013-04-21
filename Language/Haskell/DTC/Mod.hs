
-- | Some useful functions.
module Language.Haskell.DTC.Mod
    ( modifyHsDecls
    , unBangType
    , tyVarName
    , modifyHsName
      ) where

import Language.Haskell.Exts

-- | Lift a function over @[@'HsDecl'@]@ to a function over 'HsModule'.
modifyHsDecls :: ([Decl] -> [Decl]) -> (Module -> Module)
modifyHsDecls f (Module loc m pr w es is decls) = Module loc m pr w es is $ f decls

-- | Skip a bang in a type.
unBangType :: BangType -> Type
unBangType (BangedTy x)   = x
unBangType (UnBangedTy x) = x
unBangType (UnpackedTy x) = x

-- | Extract the 'Name' of a 'TyVarBind'.
tyVarName :: TyVarBind -> Name
tyVarName (KindedVar n _) = n
tyVarName (UnkindedVar n) = n

-- | Lift a function over 'String' to a function over 'HsName'.
modifyHsName :: (String -> String) -> (Name -> Name)
modifyHsName f (Ident x) = Ident $ f x
modifyHsName f (Symbol x) = Symbol $ f x
