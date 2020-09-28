{-# LANGUAGE OverloadedStrings #-}

module Examples.Crowd where

import Plutus

crowdScript :: PubKey -> Natural -> Slot -> Slot -> Script
crowdScript owner target collect refund = Script $ \i outputs tx -> do
    let start = tx ^. txSlotRange % srStart
    case (start >= refund, start >= collect) of

        -- refund
        (True, _) -> do
                        d <- datum i outputs
                        unless (d `elem` tx ^. txSignees) $
                            validationError "contributor signature missing"

        -- collect
        (_, True) -> do
                        let v = sumOf (each % oValue % to adaAmount) outputs
                        unless (v >= target) $
                            validationError "campaign target not reached"

                        unless (owner `elem` tx ^. txSignees) $
                            validationError "owener signature missing"

        _         -> validationError "deadline not reached"

startExampleCampaign :: Natural -> Natural -> ChainM (Hash, Hash)
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

successfulCampaignExample :: IO ()
successfulCampaignExample = flip runChainM' [("Alice", 1000), ("Bob", 1000), ("Charlie", 1000)] $ do
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

failedCampaignExample :: IO ()
failedCampaignExample = flip runChainM' [("Alice", 1000), ("Bob", 1000), ("Charlie", 1000)] $ do
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
