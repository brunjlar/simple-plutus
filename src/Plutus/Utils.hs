module Plutus.Utils
    ( evalChainM, execChainM
    , dumpChainState, runChainM'
    , optr
    , always
    , validationError
    , datum, redeemer, ownScriptId
    ) where

import           Control.Monad
import qualified Data.Map.Strict as Map
import           Optics
import           Text.Printf     (printf)

import           Plutus.Chain
import           Plutus.Hash
import           Plutus.Types

evalChainM :: ChainM a -> [(PubKey, Natural)] -> Either ChainError a
evalChainM m xs = fst <$> runChainM m xs

execChainM :: ChainM a -> [(PubKey, Natural)] -> Either ChainError ChainState
execChainM m xs = snd <$> runChainM m xs

dumpChainState :: ChainState -> IO ()
dumpChainState cs = do
    printf "time   : %d\n" (cs ^. csSlot)
    printf "outputs:\n\n"
    printf "  TxId                              Ix           Address                        Value                          Datum\n\n"
    forM_ (cs ^. csUTxOs % to Map.toList) $ \(OutputPtr h i, Output a v d) -> do
        printf "  %-33s [%2d]   |->   %-30s %-30s %s\n" (maybe "Genesis" show h) i (show a) (show v) (show d)

runChainM' :: Show a => ChainM a -> [(PubKey, Natural)] -> IO ()
runChainM' m xs = case runChainM m xs of
    Left err      -> printf "ERROR: %s\n" (show err)
    Right (x, cs) -> do
        printf "RESULT : %s\n" (show x)
        dumpChainState cs

optr :: Hash -> Int -> OutputPtr
optr h i = OutputPtr (Just h) i

always :: SlotRange
always = SlotRange 0 Forever

validationError :: String -> Either String a
validationError = Left

datum :: Typeable a => Int -> [Output] -> Either String a
datum i outputs = case preview (ix i % oDatum) outputs >>= fromDatum of
    Nothing -> validationError "wrong datum type"
    Just a  -> return a

redeemer :: Typeable a => Int -> Tx -> Either String a
redeemer i tx = case preview (txInputs % ix i % iRedeemer) tx >>= fromDatum of
    Nothing -> validationError "wrong redeemer type"
    Just a  -> return a

ownScriptId :: Int -> [Output] -> Either String ScriptId
ownScriptId i outputs = case preview (ix i % oAddress % _ScriptAddr) outputs of
    Nothing  -> validationError "index out of range of wrong address type"
    Just sid -> return sid
