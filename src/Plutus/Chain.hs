{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Plutus.Chain
    ( MonadError (..)
    , unit, genesis
    , ChainError (..)
    , ChainM
    , runChainM
    , tick, time
    , uploadScript, lookupScript
    , addUTxO, useUTxO
    , outputsAt
    ) where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.State
import qualified Data.Map.Strict        as Map
import qualified Data.Sequence          as Seq
import           Optics
import           Optics.State.Operators

import           Plutus.Types

unit :: Datum
unit = toDatum ()

genesis :: Int -> OutputPtr
genesis = OutputPtr Nothing

genesisState :: [(PubKey, Natural)] -> ChainState
genesisState xs = ChainState
    { _csUTxOs   = Map.fromList [ (OutputPtr Nothing i, Output (PKAddr pk) (fromAda n) unit)
                                | (i, (pk, n)) <- zip [0..] xs
                                ]
    , _csSlot    = 0
    , _csScripts = Seq.Empty
    }

data ChainError =
      NonExistingOutput OutputPtr
    | MissingSignature PubKey
    | ValueMismatch Value Value
    | ValidationError String
    | SlotError Slot SlotRange
    | InvalidScriptId ScriptId
    | IllegalForging ScriptId
    | IllegalAdaForging
    | NoInput
    | CustomError String
    deriving Show

newtype ChainM a = ChainM (StateT ChainState (Either ChainError) a)
    deriving (Functor, Applicative, Monad, MonadError ChainError)

runChainM :: [(PubKey, Natural)] -> ChainM a -> Either ChainError (a, ChainState)
runChainM xs (ChainM m) = runStateT m $ genesisState xs

tick :: Slot -> ChainM ()
tick slot = ChainM $ csSlot %= max slot

time :: ChainM Slot
time = ChainM $ use csSlot

uploadScript :: Script -> ChainM ScriptId
uploadScript script = ChainM $ do
    csScripts %= (Seq.|> script)
    fromIntegral . pred . length <$> use csScripts

lookupScript :: ScriptId -> ChainM Script
lookupScript sid = ChainM $ do
    scripts <- use csScripts
    case scripts Seq.!? fromIntegral sid of
        Nothing     -> throwError $ InvalidScriptId sid
        Just script -> return script

addUTxO :: OutputPtr -> Output -> ChainM ()
addUTxO ptr out = ChainM $ csUTxOs % at ptr .= Just out

useUTxO :: OutputPtr -> ChainM Output
useUTxO ptr = ChainM $ do
    m <- use $ csUTxOs % at ptr
    case m of
        Nothing  -> throwError $ NonExistingOutput ptr
        Just out -> csUTxOs % at ptr .= Nothing >> return out

outputsAt :: Address -> ChainM [(OutputPtr, Output)]
outputsAt addr = ChainM $ do
    m <- use csUTxOs
    return $ Map.toList $ Map.filter (\output -> output ^. oAddress == addr) m
