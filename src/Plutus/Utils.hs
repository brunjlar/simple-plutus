module Plutus.Utils
    ( evalChainM
    , optr
    , always
    , validationError
    , redeemer
    ) where

import Optics

import Plutus.Chain
import Plutus.Hash
import Plutus.Types

evalChainM :: ChainM a -> [(PubKey, Natural)] -> Either ChainError a
evalChainM m xs = fst <$> runChainM m xs

optr :: Hash -> Int -> OutputPtr
optr h i = OutputPtr (Just h) i

always :: SlotRange
always = SlotRange 0 Forever

validationError :: String -> Either String a
validationError = Left

redeemer :: Typeable a => Int -> Tx -> Either String a
redeemer i tx = case preview (txInputs % ix i % iRedeemer) tx >>= fromDatum of
    Nothing -> validationError "wrong redeemer type"
    Just a  -> return a