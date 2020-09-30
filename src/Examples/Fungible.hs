{-# LANGUAGE OverloadedStrings  #-}

module Examples.Fungible where

import Plutus

-- | Monetary policy for a simple fungible token that can only minted once.
fungibleScript :: OutputPtr -- ^ unique output to consume during minting
               -> Natural   -- ^ total supply to be minted
               -> String    -- ^ token name
               -> Script
fungibleScript ptr supply name = do

    -- make sure the specified output is consumed
    tx <- txS
    assertS (anyOf (txInputs % each % iOutputPtr) (== ptr) tx) $
        "output " ++ show ptr ++ " not consumed"

    -- make sure the right amount is minted
    sid <- ownScriptId
    let actualSupply = tx ^. txForge % to (tokenAmount $ Token sid name)
    assertS (actualSupply == supply) $
        "actually minted token amount " ++ show actualSupply ++ " disagrees with expected amount " ++ show supply

-- |
-- >>> fungibleExample 1000000 "TOKEN"
-- RESULT : ()
-- time   : 0
-- outputs:
-- <BLANKLINE>
--   TxId                              Ix           Address                        Value                          Datum
-- <BLANKLINE>
--   80cdeeae6c10a6f2b57e7cbeeab2e501  [ 0]   |->   PKAddr "Alice"                 <1000000 {0-TOKEN}>            <<(): ()>>
--   c19b8929f590513d271d842d252436cf  [ 0]   |->   PKAddr "Alice"                 <1000 ₳>                       <<(): ()>>
--
fungibleExample :: Natural -> String -> IO ()
fungibleExample supply tn = runChainM' [("Alice", 1000)] $ do
    -- create an output that can serve as unique output for the monetary policy
    tid1 <- addTx $ Tx
        { _txInputs    = [Input (genesis 0) unit]
        , _txOutputs   = [ Output "Alice" (fromAda $ 1000) unit
                         , Output "Alice" mempty unit
                         ]
        , _txSignees   = ["Alice"]
        , _txSlotRange = always
        , _txForge     = mempty
        }

    -- create an output at the monetary policy address
    sid <- uploadScript $ fungibleScript (optr tid1 1) supply tn
    let token = Token sid tn
    tid2 <- addTx $ Tx
        { _txInputs    = [Input (optr tid1 0) unit]
        , _txOutputs   = [ Output "Alice" (fromAda $ 1000) unit
                         , Output (ScriptAddr sid) mempty unit
                         ]
        , _txSignees   = ["Alice"]
        , _txSlotRange = always
        , _txForge     = mempty
        }

    -- mint the token
    void $ addTx $ Tx
        { _txInputs    = [ Input (optr tid2 1) unit
                         , Input (optr tid1 1) unit
                         ]
        , _txOutputs   = [Output "Alice" (fromToken token supply) unit]
        , _txSignees   = ["Alice"]
        , _txSlotRange = always
        , _txForge     = fromToken token supply
        }
