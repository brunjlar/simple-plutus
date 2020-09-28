{-# LANGUAGE GADTs #-}

module Plutus.Datum
    ( Datum
    , toDatum
    , fromDatum
    ) where

import Data.Dynamic
import Data.Typeable

data Datum where
    Datum :: (Show a, Typeable a) => a -> Datum

instance Show Datum where
    show (Datum d) = "<<" ++ show (typeOf d) ++ ": " ++ show d ++ ">>"

toDatum :: (Show a, Typeable a) => a -> Datum
toDatum = Datum

fromDatum :: Typeable a => Datum -> Maybe a
fromDatum (Datum d) = cast d
