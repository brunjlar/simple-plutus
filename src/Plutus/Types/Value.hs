{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Plutus.Types.Value
    ( Natural
    , ScriptId
    , Token (..), _Ada, _Token
    , Value, value, tokens
    , fromToken, fromAda, tokenAmount, adaAmount
    ) where

import           Data.List       (intercalate)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Numeric.Natural (Natural)
import           Optics
import           Text.Printf     (printf, PrintfArg (..))

newtype ScriptId = ScriptId Natural
    deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral, PrintfArg)

data Token = Ada | Token ScriptId String
    deriving (Eq, Ord)

instance Show Token where
    show Ada            = "₳"
    show (Token sid tn) = printf "{%d-%s}" sid tn

newtype Value = Value (Map Token Natural)
    deriving (Eq, Ord)

instance Show Value where
    show (Value m) =
        "<" ++
        intercalate
            " + "
            [printf "%d %s" n (show t) | (t, n) <- Map.toList m] ++
        ">"

instance Semigroup Value where
    Value m <> Value n = Value $ Map.unionWith (+) m n

instance Monoid Value where
    mempty = Value Map.empty

makePrisms ''Token

value :: Token -> Lens' Value Natural
value t = lens gt st
  where
    gt :: Value -> Natural
    gt (Value m) = Map.findWithDefault 0 t m

    st :: Value -> Natural -> Value
    st (Value m) 0 = Value $ Map.delete t m
    st (Value m) n = Value $ Map.insert t n m

tokens :: Value -> [Token]
tokens (Value m) = Map.keys m

fromToken :: Token -> Natural -> Value
fromToken t n = set (value t) n mempty

fromAda :: Natural -> Value
fromAda = fromToken Ada

tokenAmount :: Token -> Value -> Natural
tokenAmount t (Value m) = Map.findWithDefault 0 t m

adaAmount :: Value -> Natural
adaAmount = tokenAmount Ada