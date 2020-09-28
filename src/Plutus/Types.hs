{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Plutus.Types
    ( module Plutus.Types.Datum
    , module Plutus.Types.Value
    , Map, Seq
    , PubKey
    , Slot
    , SlotEnd (..), _Finite, _Forever
    , SlotRange (..), srStart, srEnd
    , Address (..), _PKAddr, _ScriptAddr
    , Output (..), oAddress, oValue, oDatum
    , OutputPtr (..), opTxHash, opOutputIx
    , Input (..), iOutputPtr, iRedeemer
    , Tx (..), txInputs, txOutputs, txSignees, txSlotRange, txForge
    , Script (..)
    , ChainState (..), csUTxOs, csSlot, csScripts
    ) where

import Data.Map.Strict    (Map)
import Data.Sequence      (Seq)
import Data.String        (IsString (..))
import Optics
import Text.Printf        (PrintfArg (..))

import Plutus.Hash
import Plutus.Types.Datum
import Plutus.Types.Value

newtype PubKey = PK String
    deriving newtype (Show, Eq, Ord, IsString)

newtype Slot = Slot Natural
    deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral, PrintfArg)

data SlotEnd = Finite Slot | Forever
    deriving (Eq, Ord)

instance Show SlotEnd where
    show (Finite s) = show s
    show Forever    = "∞"

data SlotRange = SlotRange
    { _srStart :: Slot
    , _srEnd   :: SlotEnd
    } deriving (Eq, Ord)

instance Show SlotRange where
    show (SlotRange s e) =
        '[' : show s ++ "," ++ show e ++ case e of
            Finite _ -> "]"
            Forever  -> ")"

data Address = PKAddr PubKey | ScriptAddr ScriptId
    deriving (Show, Eq, Ord)

data Output = Output
    { _oAddress :: Address
    , _oValue   :: Value
    , _oDatum   :: Datum
    } deriving (Show, Eq, Ord)

data OutputPtr = OutputPtr
    { _opTxHash   :: Maybe Hash
    , _opOutputIx :: Int
    } deriving (Show, Eq, Ord)

data Input = Input
    { _iOutputPtr :: OutputPtr
    , _iRedeemer  :: Datum
    } deriving Show

data Tx = Tx
    { _txInputs    :: [Input]
    , _txOutputs   :: [Output]
    , _txSignees   :: [PubKey]
    , _txSlotRange :: SlotRange
    , _txForge     :: Value
    } deriving Show

newtype Script = Script {runScript :: Int -> [Output] -> Tx -> Either String ()}

data ChainState = ChainState
    { _csUTxOs   :: Map OutputPtr Output
    , _csSlot    :: Slot
    , _csScripts :: Seq Script
    }

makePrisms ''SlotEnd
makeLenses ''SlotRange
makePrisms ''Address
makeLenses ''Output
makeLenses ''OutputPtr
makeLenses ''Input
makeLenses ''Tx
makeLenses ''ChainState
