{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Plutus.Hash
    ( Hash
    , hash
    ) where

import qualified Crypto.Hash        as C
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

newtype Hash = Hash (C.Digest C.MD5)
    deriving newtype (Show, Eq, Ord)

hash :: Show a => a -> Hash
hash = Hash . C.hash . T.encodeUtf8 . T.pack . show
