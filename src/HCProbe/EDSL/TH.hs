{-# LANGUAGE TemplateHaskell #-}
module HCProbe.EDSL.TH
            (
             generatePutters
            ) where

import Control.Applicative ((<$>))
import Control.Monad.Writer
import Data.Char
import Data.List.Split
import Data.Maybe
import Language.Haskell.TH


generatePutters :: Name -> Q [Dec]
generatePutters name =
    withType name $ \cons -> fmap concat $ mapM recPut cons
  where
     recPut :: Con -> Q [Dec]
     recPut (RecC conName fields) = catMaybes <$> mapM fieldPut fields
     recPut x = error $ "HCProbe.EDSL.TH.generatePutters: Invalid constructor in type." ++ (show x)
     fieldPut (fName,_,_) = do
       let putName = "put" ++ of2hs (nameBase fName)
       mPut <- lookupValueName putName
       case mPut of
         Just _ -> return Nothing
         Nothing -> Just <$>
           (funD (mkName putName) $ [ do
             value  <- newName "value"
             clause [varP value]
               (normalB (infixE (Just [e| tell . Endo |]) [| ($) |]
                                (Just $ do
                                   record  <- newName "record"
                                   lam1E (varP record) $ recUpdE (varE record) [fieldExp fName (varE value)]
                                )
                        )
               )
               []
                                    
                                    ] )

of2hs :: String -> String
of2hs s = concat $ map upFirst $ dropOfp  $ splitOn "_" s
  where
    upFirst (a:as) = toUpper a: as
    upFirst [] = []
    dropOfp ("ofp":rest) = rest
    dropOfp l = l

-- | Boilerplate for top level splices.
--
-- The given 'Name' must be from a type constructor. Furthermore, the
-- type constructor must be either a data type or a newtype. Any other
-- value will result in an exception.
withType :: Name
         -> ([Con] -> Q a)
         -- ^ Function that generates the actual code. Will be applied
         -- to the type variable binders and constructors extracted
         -- from the given 'Name'.
         -> Q a
         -- ^ Resulting value in the 'Q'uasi monad.
withType name f = do
    info <- reify name
    case info of
      TyConI dec ->
        case dec of
          DataD    _ _ [] cons _ -> f cons
          NewtypeD _ _ [] con  _ -> f [con]
          other -> error $ "HCProbe.EDSL.TH.withType: Unsupported type: "
                          ++ show other
      _ -> error "HCProbe.EDSL.TH.withType: I need the name of a type."
