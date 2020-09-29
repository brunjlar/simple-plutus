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
    , ScriptM, runScript, indexS, outputsS, txS, Script
    , ChainState (..), csUTxOs, csSlot, csScripts
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map.Strict      (Map)
import Data.Sequence        (Seq)
import Data.String          (IsString (..))
import Optics
import Text.Printf          (PrintfArg (..))

import Plutus.Hash
import Plutus.Types.Datum
import Plutus.Types.Value

-- | A public key, which identifies a party on the blockchain.
newtype PubKey = PubKey String
    deriving newtype (Show, Eq, Ord, IsString)

-- | A slot.
newtype Slot = Slot Natural
    deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral, PrintfArg)

-- | Specifies the end of a 'SlotRange'. Such a range can either end at a finite 'Slot' or last forever.
data SlotEnd = Finite Slot | Forever
    deriving (Eq, Ord)

instance Show SlotEnd where
    show (Finite s) = show s
    show Forever    = "∞"

-- | A time interval.
data SlotRange = SlotRange
    { _srStart :: Slot    -- ^ begin of the time interval
    , _srEnd   :: SlotEnd -- ^ end of the time interval (can be infinite).
    } deriving (Eq, Ord)

instance Show SlotRange where
    show (SlotRange s e) =
        '[' : show s ++ "," ++ show e ++ case e of
            Finite _ -> "]"
            Forever  -> ")"

-- | An address, specifiying who is entitled to unlock the funds locked in an 'Output'.
data Address = PKAddr PubKey | ScriptAddr ScriptId
    deriving (Show, Eq, Ord)

instance IsString Address where
    fromString = PKAddr . PubKey

-- | A transaction output.
data Output = Output
    { _oAddress :: Address -- ^ the address, determining who can unlock the contained value
    , _oValue   :: Value   -- ^ the locked value
    , _oDatum   :: Datum   -- ^ additional data carrying custom state
    } deriving (Show, Eq, Ord)

-- | Uniquely identifies a specific 'Output'.
data OutputPtr = OutputPtr
    { _opTxHash   :: Maybe Hash -- ^ 'Nothing' if the 'Output' is included in the genesis block, otherwise the 'Hash' of the transaction which produced the output
    , _opOutputIx :: Int        -- ^ zero-based index of the 'Output'
    } deriving (Show, Eq, Ord)

data Input = Input
    { _iOutputPtr :: OutputPtr -- ^ identifies the 'Output' consumed by this 'Input'
    , _iRedeemer  :: Datum     -- ^ custom data
    } deriving Show

data Tx = Tx
    { _txInputs    :: [Input]    -- ^ all inputs
    , _txOutputs   :: [Output]   -- ^ all outputs
    , _txSignees   :: [PubKey]   -- ^ parties who signed this transaction
    , _txSlotRange :: SlotRange  -- ^ time interval in which this transaction must be included into the blockchain for it to be valid
    , _txForge     :: Value      -- ^ custom tokens forged by this transaction
    } deriving Show

newtype ScriptM a = ScriptM (ReaderT (Int, [Output], Tx) (Either String) a)
    deriving (Functor, Applicative, Monad, MonadError String)

runScript :: ScriptM a -> Int -> [Output] -> Tx -> Either String a
runScript (ScriptM m) i outputs tx = runReaderT m (i, outputs, tx)

indexS :: ScriptM Int
indexS = ScriptM $ asks $ \(i, _, _) -> i

outputsS :: ScriptM [Output]
outputsS = ScriptM $ asks $ \(_, outputs, _) -> outputs

txS :: ScriptM Tx
txS = ScriptM $ asks $ \(_, _, tx) -> tx

-- | A validation script which will be executed when a transaction tries to spend
-- an 'Output' locked at the script address for the script.
type Script = ScriptM ()

instance Show (ScriptM a) where
    show _ = "<<SCRIPT>>"

-- | Determines the state of the blockchain.
data ChainState = ChainState
    { _csUTxOs   :: Map OutputPtr Output -- ^ unspent 'Output'\'s
    , _csSlot    :: Slot                 -- ^ current time
    , _csScripts :: Seq Script           -- ^ uploaded 'Script'\'s
    } deriving Show

makePrisms ''SlotEnd
makeLenses ''SlotRange
makePrisms ''Address
makeLenses ''Output
makeLenses ''OutputPtr
makeLenses ''Input
makeLenses ''Tx
makeLenses ''ChainState
