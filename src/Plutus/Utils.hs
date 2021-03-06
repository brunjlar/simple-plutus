{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plutus.Utils
    ( evalChainM, execChainM
    , dumpChainState, runChainM'
    , optr
    , always
    , viewS, assertS, fromDatumS
    , ownOutput, ownDatum, ownValue
    , ownInput, ownRedeemer, ownScriptId
    ) where

import           Control.Monad
import           Data.Proxy      (Proxy (..))
import qualified Data.Map.Strict as Map
import           Data.Typeable   (typeRep)
import           Optics
import           Text.Printf     (printf)

import           Plutus.Chain
import           Plutus.Hash
import           Plutus.Types

evalChainM :: [(PubKey, Natural)] -> ChainM a -> Either ChainError a
evalChainM xs m = fst <$> runChainM xs m

execChainM :: [(PubKey, Natural)] -> ChainM a -> Either ChainError ChainState
execChainM xs m = snd <$> runChainM xs m

dumpChainState :: ChainState -> IO ()
dumpChainState cs = do
    printf "time   : %d\n" (cs ^. csSlot)
    printf "outputs:\n\n"
    printf "  TxId                              Ix           Address                        Value                          Datum\n\n"
    forM_ (cs ^. csUTxOs % to Map.toList) $ \(OutputPtr h i, Output a v d) ->
        printf "  %-33s [%2d]   |->   %-30s %-30s %s\n" (maybe "Genesis" show h) i (show a) (show v) (show d)

runChainM' :: Show a => [(PubKey, Natural)] -> ChainM a -> IO ()
runChainM' xs m = case runChainM xs m of
    Left err      -> printf "ERROR: %s\n" (show err)
    Right (x, cs) -> do
        printf "RESULT : %s\n" (show x)
        dumpChainState cs

optr :: Hash -> Int -> OutputPtr
optr h = OutputPtr (Just h)

always :: SlotRange
always = SlotRange 0 Forever

assertS :: Bool -> String -> Script
assertS False err = throwError err
assertS True  _   = return ()

viewS :: Is k An_AffineFold => Optic' k is s a -> s -> ScriptM a
viewS o s = case preview o s of
    Nothing -> throwError "no element in focus"
    Just a  -> return a

fromDatumS :: forall a. Typeable a => Datum -> ScriptM a
fromDatumS d = case fromDatum d of
    Nothing -> throwError $ "expected " ++ show d ++ " to wrap value of type " ++ show (typeRep (Proxy :: Proxy a))
    Just a  -> return a

ownOutput :: ScriptM Output
ownOutput = do
    i       <- indexS
    outputs <- outputsS
    viewS (ix i) outputs

ownDatum :: Typeable a => ScriptM a
ownDatum = do
    output <- ownOutput
    fromDatumS $ output ^. oDatum

ownValue :: ScriptM Value
ownValue = do
    output <- ownOutput
    return $ output ^. oValue

ownInput :: ScriptM Input
ownInput = do
    i  <- indexS
    tx <- txS
    viewS (txInputs % ix i) tx

ownRedeemer :: Typeable a => ScriptM a
ownRedeemer = do
    input <- ownInput
    fromDatumS $ input ^. iRedeemer

ownScriptId :: ScriptM ScriptId
ownScriptId = do
    output <- ownOutput
    viewS (oAddress % _ScriptAddr) output
