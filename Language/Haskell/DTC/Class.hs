
-- | Class definition from a data declaration.
module Language.Haskell.DTC.Class
   ( -- * Functions
     dataToClassWith
   , dataToClass
     -- * Example
     -- $example
     ) where

import Data.Char
import Data.Maybe
import Language.Haskell.Exts

import Language.Haskell.DTC.Mod

----------------------------------------------------------------------------------------
{- $example
Let's see an example of how you can transform all data declarations in a module to class definitions.

Let the following module (Maybe.hs):

> module MaybeExample where
>
> data Maybe a = Just a | Nothing

It contains the data declaration of the 'Maybe' type. Now, if we write:

> import Language.Haskell.DTC
>
> main = do -- Parse the code.
>           m <- parseModuleWithSrc "Maybe.hs" defaultParseMode
>           -- Modify declarations with dataToClassWith.
>           let m' = modifyHsDecls (map (dataToClassWith "m")) m
>           -- Write the pretty-printed output.
>           writeFile "MaybeC.hs" (prettyPrint m')

It produces the following output (in MaybeC.hs):

> module MaybeExample where
>
> class Maybe m where
>
>         just :: a -> m a
>
>         fromJust :: m a -> a
>
>         nothing :: m a

-}
----------------------------------------------------------------------------------------

-- | Transform a data declaration to a class definition.
-- The 'String' argument will be the name of the type variable of the class definition.
dataToClassWith :: String -> Decl -> Decl
dataToClassWith str (DataDecl loc _ ctx x_T xs_v xs_C _)
      = let methods = concatMap (method str xs_v x_T) xs_C
        in  ClassDecl loc ctx x_T [UnkindedVar $ Ident str] [] methods
dataToClassWith _ d = d

-- | Transform a data declaration to a class definition.
-- Equivalent to @dataToClassWith \"t\"@.
dataToClass :: Decl -> Decl
dataToClass = dataToClassWith "t"

(->>) :: Type -> Type -> Type
t1 ->> t2 = TyFun t1 t2

(.>>) :: Type -> Type -> Type
t1 .>> t2 = TyApp t1 t2

hsTyTuple :: [Type] -> Type
hsTyTuple [t] = t
hsTyTuple xs  = TyTuple Boxed xs

replaceType :: Name -> String -> Type -> Type
replaceType name new (TyFun t1 t2) = replaceType name new t1 ->> replaceType name new t2
replaceType name new (TyTuple b xs)  = TyTuple Unboxed $ map (replaceType name new) xs
replaceType name new (TyApp t1 t2) = replaceType name new t1 .>> replaceType name new t2
replaceType name new (TyVar name') = if name == name' then TyVar $ Ident new
                                                      else TyVar name'
replaceType name new (TyCon qname) =
    TyCon $
     case qname of
      Qual m name' -> if name == name' then Qual m $ Ident new
                                       else Qual m name'
      UnQual name' -> if name == name' then UnQual $ Ident new
                                       else UnQual name'
      x -> x
replaceType name new (TyForall m ctx t) = TyForall m ctx $ replaceType name new t
replaceType name new (TyList t) = TyList $ replaceType name new t
replaceType name new (TyParen t) = TyParen $ replaceType name new t
replaceType name new (TyInfix t1 q t2) = TyInfix (replaceType name new t1) q (replaceType name new t2)
replaceType name new (TyKind t k) = TyKind (replaceType name new t) k

constructor :: String -> [TyVarBind] -> Name -> QualConDecl -> ClassDecl
constructor str xs_v x_T (QualConDecl loc _ _ (ConDecl name xs)) =
    constructor_ str loc name xs_v x_T xs
constructor str xs_v x_T (QualConDecl loc _ _ (RecDecl name xs)) =
    constructor_ str loc name xs_v x_T (map snd xs)

constructor_ :: String -> SrcLoc -> Name -> [TyVarBind]
                       -> Name -> [BangType] -> ClassDecl
constructor_ str loc name xs_v x_T xs =
  ClsDecl $ 
       TypeSig loc [modifyHsName (\(n:ns) -> toLower n : ns) name]
               (TyForall Nothing [] $
                 foldr (->>)
                       (foldl (.>>)
                              (TyVar $ Ident str)
                              (map (TyVar . tyVarName) xs_v) )
                       (map (replaceType x_T str . unBangType) xs)
                )

deconstructor :: String -> [TyVarBind] -> Name -> QualConDecl -> [ClassDecl]
deconstructor str xs_v x_T (QualConDecl loc _ _ (ConDecl name xs)) =
    if length xs > 0
       then [ ClsDecl $
                 TypeSig loc [modifyHsName ("from"++) name]
                      (TyForall Nothing [] $
                        foldl (.>>)
                              (TyVar $ Ident str)
                              (map (TyVar . tyVarName) xs_v)
                        ->>
                        (hsTyTuple $ map (replaceType x_T str . unBangType) xs)
                       )
             ]
       else [ ]
deconstructor str xs_v x_T (QualConDecl loc _ _ (RecDecl name xs)) =
    map (\(ys,t) -> ClsDecl $
     TypeSig loc [head ys]
             (TyForall Nothing [] $
               foldl (.>>)
                     (TyVar $ Ident str)
                     (map (TyVar . tyVarName) xs_v)
               ->>
               (replaceType x_T str $ unBangType t)
              )
         ) xs

method :: String -> [TyVarBind] -> Name -> QualConDecl -> [ClassDecl]
method str xs x_T dec = constructor str xs x_T dec : deconstructor str xs x_T dec

