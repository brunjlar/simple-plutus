{-# LANGUAGE OverloadedStrings #-}

module Examples.NoPlutus where

import Plutus

-- | A simple example of the two transactions from the slides not involving Plutus at all.
--
-- >>> simpleExample
-- RESULT : ()
-- time   : 0
-- outputs:
-- <BLANKLINE>
--   TxId                              Ix           Address                        Value                          Datum
-- <BLANKLINE>
--   988c014c5beff9b07711f86a296040b3  [ 0]   |->   PKAddr "Alice"                 <5 ₳>                          <<(): ()>>
--   988c014c5beff9b07711f86a296040b3  [ 1]   |->   PKAddr "Bob"                   <35 ₳>                         <<(): ()>>
--   988c014c5beff9b07711f86a296040b3  [ 2]   |->   PKAddr "Charlie"               <110 ₳>                        <<(): ()>>
--
simpleExample :: IO ()
simpleExample = runChainM' [("Alice", 100), ("Bob", 50)] $ do
    tid1 <- addTx Tx
        { _txInputs = [Input (genesis 0) unit]
        , _txOutputs = [ Output (PKAddr "Alice") (fromAda 60) unit
                       , Output (PKAddr "Bob") (fromAda 40) unit]
        , _txSignees = ["Alice"]
        , _txSlotRange = always
        , _txForge = mempty
        }
    void $ addTx Tx
        { _txInputs = [ Input (optr tid1 0) unit
                      , Input (optr tid1 1) unit
                      , Input (genesis 1) unit
                      ]
        , _txOutputs = [ Output (PKAddr "Alice") (fromAda 5) unit
                       , Output (PKAddr "Bob") (fromAda 35) unit
                       , Output (PKAddr "Charlie") (fromAda 110) unit
                       ]
        , _txSignees = ["Alice", "Bob"]
        , _txSlotRange = always
        , _txForge = mempty
        }
