{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Plutus.Validation
    ( inRange
    , addTx
    ) where

import Control.Monad
import Optics

import Plutus.Chain
import Plutus.Hash
import Plutus.Types

inRange :: Slot -> SlotRange -> Bool
inRange s (SlotRange start end) =
    s >= start &&
    case end of
        Forever    -> True
        (Finite t) -> s <= t

addTx :: Tx -> ChainM Hash
addTx tx = do

    -- ensure the transaction's slot range is valid
    checkSlot

    let inputs = tx ^. txInputs
    when (null inputs) $   -- Each transaction needs at least one input
        throwError NoInput -- (so no two transactions have the same hash).

    -- make sure all inputs point to unspent outputs
    consumedOutputs <- mapM useUTxO $ toListOf (each % iOutputPtr) inputs

    -- validate each input and collect all input values
    inVal <- mconcat <$> mapM (validateInput consumedOutputs) [0 .. length inputs - 1]

    -- add the newly produced outputs to the UTxO-set and collect their values
    outVal <- processOutputs

    -- ensure that the sum of all input values and the newly forged values
    -- equals the sum of all output values
    let forge = tx ^. txForge
    when (inVal <> forge /= outVal) $
        throwError $ ValueMismatch inVal outVal

    -- check that no ada was forged
    unless (adaAmount forge == 0) $
        throwError IllegalAdaForging

    -- check that a corresponding monetary policy output was consumed for each forged token
    let forgeScripts = toListOf (to tokens % each % _Token % _1) forge
    forM_ forgeScripts $ \sid ->
        unless (anyOf (each % oAddress % _ScriptAddr) (== sid) consumedOutputs) $
            throwError $ IllegalForging sid

    return $ hash tx

  where
    checkSlot :: ChainM ()
    checkSlot = do
        currentSlot <- time
        let sr = tx ^. txSlotRange
        unless (currentSlot `inRange` sr) $
            throwError $ SlotError currentSlot sr

    validateInput :: [Output] -> Int -> ChainM Value
    validateInput outputs i = do
        let output = outputs !! i
        case output ^. oAddress of
            PKAddr pk ->
                unless (pk `elem` (tx ^. txSignees)) $
                    throwError $ MissingSignature pk
            ScriptAddr sid -> do
                script <- lookupScript sid
                case runScript script i outputs tx of
                    Left err -> throwError $ ValidationError err
                    Right () -> return ()
        return $ output ^. oValue

    processOutputs :: ChainM Value
    processOutputs = do
        let tid             = Just $ hash tx
            producedOutputs = tx ^. txOutputs
        values <- forM [0 .. length producedOutputs - 1] $ \i -> do
            let output = producedOutputs !! i
            addUTxO (OutputPtr tid i) output
            return $ output ^. oValue
        return $ mconcat values
