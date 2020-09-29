{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plutus.Utils
    ( evalChainM, execChainM
    , dumpChainState, runChainM'
    , optr
    , always
    , viewS, assertS
    , ownDatum, ownRedeemer, ownScriptId, ownValue
    ) where

import           Control.Monad
import qualified Data.Map.Strict as Map
import           Data.Typeable   (typeOf)
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
    forM_ (cs ^. csUTxOs % to Map.toList) $ \(OutputPtr h i, Output a v d) -> do
        printf "  %-33s [%2d]   |->   %-30s %-30s %s\n" (maybe "Genesis" show h) i (show a) (show v) (show d)

runChainM' :: Show a => [(PubKey, Natural)] -> ChainM a -> IO ()
runChainM' xs m = case runChainM xs m of
    Left err      -> printf "ERROR: %s\n" (show err)
    Right (x, cs) -> do
        printf "RESULT : %s\n" (show x)
        dumpChainState cs

optr :: Hash -> Int -> OutputPtr
optr h i = OutputPtr (Just h) i

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
    Nothing -> throwError $ "expected " ++ show d ++ " to wrap value of type " ++ (show $ typeOf (undefined :: a))
    Just a  -> return a

ownDatum :: Typeable a => ScriptM a
ownDatum = do
    i       <- indexS
    outputs <- outputsS
    d       <- viewS (ix i % oDatum) outputs
    fromDatumS d

ownRedeemer :: Typeable a => ScriptM a
ownRedeemer = do
    i  <- indexS
    tx <- txS
    d  <- viewS (txInputs % ix i % iRedeemer) tx
    fromDatumS d

ownScriptId :: ScriptM ScriptId
ownScriptId = do
    i       <- indexS
    outputs <- outputsS
    viewS (ix i % oAddress % _ScriptAddr) outputs

ownValue :: ScriptM Value
ownValue = do
    i       <- indexS
    outputs <- outputsS
    viewS (ix i % oValue) outputs
