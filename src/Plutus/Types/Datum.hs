{-# LANGUAGE GADTs #-}

module Plutus.Types.Datum
    ( Typeable, Datum
    , toDatum, fromDatum
    ) where

import Data.Function (on)
import Data.Typeable

data Datum where
    Datum :: (Show a, Typeable a) => a -> Datum

instance Show Datum where
    show (Datum d) = "<<" ++ show (typeOf d) ++ ": " ++ show d ++ ">>"

instance Eq Datum where
    (==) = (==) `on` show

instance Ord Datum where
    compare = compare `on` show

toDatum :: (Show a, Typeable a) => a -> Datum
toDatum = Datum

fromDatum :: Typeable a => Datum -> Maybe a
fromDatum (Datum d) = cast d
