{-# LANGUAGE OverloadedStrings #-}

module Examples.Crowd where

import Plutus

-- | Validation script for a crowd-sourcing campaign.
crowdScript :: PubKey  -- ^ campaign owner
            -> Natural -- ^ campaign target
            -> Slot    -- ^ collection deadline; once reached, the owner can collect
            -> Slot    -- ^ refund deadline; once reached, contributors can get a refund
            -> Script
crowdScript owner target collect refund = do
    tx <- txS
    let start = tx ^. txSlotRange % srStart
    case (start >= refund, start >= collect) of

        -- refund
        (True, _) -> do
                        d <- ownDatum
                        assertS (d `elem` tx ^. txSignees) "contributor signature missing"

        -- collect
        (_, True) -> do
                        outputs <- outputsS
                        sid     <- ownScriptId
                        let v = sumOf (each % filtered (\o -> o ^. oAddress == ScriptAddr sid) % oValue % to adaAmount) outputs
                        assertS (v >= target) "campaign target not reached"
                        assertS (owner `elem` tx ^. txSignees) "owener signature missing"

        _         -> throwError "deadline not reached"

-- | Starts an example crowd sourcing campaign owned by Alice, which targets 1500 ada.
startExampleCampaign :: Natural
                     -> Natural
                     -> ChainM (Hash, Hash)
startExampleCampaign bob charlie = do
    sid <- uploadScript $ crowdScript "Alice" 1500 10 20
    tid1 <- addTx $ Tx
        { _txInputs    = [Input (genesis 1) unit]
        , _txOutputs   = [ Output (ScriptAddr sid) (fromAda bob) (toDatum ("Bob" :: PubKey))
                         , Output "Bob" (fromAda $ 1000 - bob) unit
                         ]
        , _txSignees   = ["Bob"]
        , _txSlotRange = SlotRange 0 $ Finite 9
        , _txForge     = mempty
        }
    tid2 <- addTx $ Tx
        { _txInputs    = [Input (genesis 2) unit]
        , _txOutputs   = [ Output (ScriptAddr sid) (fromAda charlie) (toDatum ("Charlie" :: PubKey))
                         , Output "Charlie" (fromAda $ 1000 - charlie) unit
                         ]
        , _txSignees   = ["Charlie"]
        , _txSlotRange = SlotRange 0 $ Finite 9
        , _txForge     = mempty
        }
    return (tid1, tid2)

-- | Example run of a successful campaign, where Bob gives 700 ada and Charlie gives 800 ada.
--
-- >>> successfulCampaignExample
-- RESULT : ()
-- time   : 10
-- outputs:
-- <BLANKLINE>
--   TxId                              Ix           Address                        Value                          Datum
-- <BLANKLINE>
--   Genesis                           [ 0]   |->   PKAddr "Alice"                 <1000 ₳>                       <<(): ()>>
--   043e18e096737dbfc2d30ef1d17950eb  [ 1]   |->   PKAddr "Charlie"               <200 ₳>                        <<(): ()>>
--   5304d9170f6074e731c9fbf3cc29c963  [ 0]   |->   PKAddr "Alice"                 <1500 ₳>                       <<(): ()>>
--   72792dbd24aff7bf6c0307a4b73506fa  [ 1]   |->   PKAddr "Bob"                   <300 ₳>                        <<(): ()>>
--
successfulCampaignExample :: IO ()
successfulCampaignExample = runChainM' [("Alice", 1000), ("Bob", 1000), ("Charlie", 1000)] $ do
    (tid1, tid2)  <- startExampleCampaign 700 800
    tick 10
    void $ addTx $ Tx
        { _txInputs    = [ Input (optr tid1 0) unit
                         , Input (optr tid2 0) unit
                         ]
        , _txOutputs   = [ Output "Alice" (fromAda 1500) unit
                         ]
        , _txSignees   = ["Alice"]
        , _txSlotRange = SlotRange 10 $ Finite 19
        , _txForge     = mempty
        }

-- | Example run of a failed campaign, where Bob and Charlie both give 700 ada.
--
-- >>> failedCampaignExample
-- RESULT : ()
-- time   : 20
-- outputs:
-- <BLANKLINE>
--   TxId                              Ix           Address                        Value                          Datum
-- <BLANKLINE>
--   Genesis                           [ 0]   |->   PKAddr "Alice"                 <1000 ₳>                       <<(): ()>>
--   131637e2ba575e9f8e6041508989608e  [ 1]   |->   PKAddr "Charlie"               <300 ₳>                        <<(): ()>>
--   298c66e50c418c0b6b567c9e33d27b46  [ 0]   |->   PKAddr "Charlie"               <700 ₳>                        <<(): ()>>
--   72792dbd24aff7bf6c0307a4b73506fa  [ 1]   |->   PKAddr "Bob"                   <300 ₳>                        <<(): ()>>
--   adad0a3f4f320a6ce5239d4df8003734  [ 0]   |->   PKAddr "Bob"                   <700 ₳>                        <<(): ()>>
--
failedCampaignExample :: IO ()
failedCampaignExample = runChainM' [("Alice", 1000), ("Bob", 1000), ("Charlie", 1000)] $ do
    (tid1, tid2)  <- startExampleCampaign 700 700
    tick 20
    void $ addTx $ Tx
        { _txInputs    = [Input (optr tid1 0) unit]
        , _txOutputs   = [Output "Bob" (fromAda 700) unit]
        , _txSignees   = ["Bob"]
        , _txSlotRange = SlotRange 20 Forever
        , _txForge     = mempty
        }
    void $ addTx $ Tx
        { _txInputs    = [Input (optr tid2 0) unit]
        , _txOutputs   = [Output "Charlie" (fromAda 700) unit]
        , _txSignees   = ["Charlie"]
        , _txSlotRange = SlotRange 20 Forever
        , _txForge     = mempty
        }
