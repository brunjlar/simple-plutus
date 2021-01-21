{-# LANGUAGE OverloadedStrings #-}

module Examples.GuessingGame where

import Plutus

-- | Validation script for a guessing game. Funds can be unlocked by guessing the secret.
-- The guess must be put into the redeemer.
guessingScript :: String -- ^ the secret to guess
               -> Script
guessingScript secret = do
    guess <- ownRedeemer
    assertS (guess == secret) "incorrect guess"

-- |
-- >>> guessingExample "Java"
-- ERROR: ValidationError "incorrect guess"
--
-- >>> guessingExample "Haskell"
-- RESULT : ()
-- time   : 0
-- outputs:
-- <BLANKLINE>
--   TxId                              Ix           Address                        Value                          Datum
-- <BLANKLINE>
--   a7af03d34254e12b6821ef95df11b2cf  [ 0]   |->   PKAddr "Bob"                   <100 ₳>                        <<(): ()>>
--
guessingExample :: String -> IO ()
guessingExample guess = runChainM' [("Alice", 100)] $ do
    sid  <- uploadScript $ guessingScript "Haskell"
    tid1 <- addTx $ Tx
        { _txInputs    = [Input (genesis 0) unit]
        , _txOutputs   = [Output (ScriptAddr sid) (fromAda 100) unit]
        , _txSignees   = ["Alice"]
        , _txSlotRange = always
        , _txForge     = mempty
        }
    void $ addTx $ Tx
        { _txInputs    = [Input (optr tid1 0) (toDatum guess)]
        , _txOutputs   = [Output "Bob" (fromAda 100) unit]
        , _txSignees   = []
        , _txSlotRange = always
        , _txForge     = mempty
        }
