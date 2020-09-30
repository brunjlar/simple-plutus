{-# LANGUAGE OverloadedStrings #-}

module Examples.Counter where

import Plutus

-- | A simple state machine implementing a counter.
counter :: StateMachine Natural ()
counter = StateMachine
    { initialState = 0
    , transit      = \n () -> Just $ n + 1
    , isFinal      = const False
    }

-- | Submits a transaction that ticks the counter.
count :: ScriptId -> Token -> ChainM ()
count sid t = do

    -- find the current state machine output
    (ptr, output) <- do
        x <- stateMachineOutput sid t
        case x of
            Nothing -> throwError $ CustomError "state machine not deployed"
            Just y  -> return y

    n <- case fromDatum $ output ^. oDatum of
            Nothing -> throwError $ CustomError "unexpected datum"
            Just m  -> return m

    void $ addTx Tx
        { _txInputs    = [Input ptr unit]
        , _txOutputs   = [Output (ScriptAddr sid) (output ^. oValue) $ toDatum $ succ (n :: Natural)]
        , _txSignees   = []
        , _txSlotRange = always
        , _txForge     = mempty
        }

-- |
-- >>> counterExample 42
-- RESULT : ()
-- time   : 0
-- outputs:
-- <BLANKLINE>
--   TxId                              Ix           Address                        Value                          Datum
-- <BLANKLINE>
--   5fde673b32e9bc704b0f292c35a626a9  [ 1]   |->   PKAddr "Alice"                 <1000 ₳>                       <<(): ()>>
--   7a99d42816801954804fab1060a4feb9  [ 0]   |->   ScriptAddr 1                   <1 {0-STATEMACHINE}>           <<Natural: 42>>
--
counterExample :: Int -> IO ()
counterExample n = runChainM' [("Alice", 1000)] $ do
    (sid, t) <- deployStateMachine counter trivialCont "Alice"
    replicateM_ n $ count sid t
