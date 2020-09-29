{-# LANGUAGE OverloadedStrings #-}

module Examples.Counter where

import Plutus

counter :: StateMachine Natural ()
counter = StateMachine
    { initialState = 0
    , transit      = \n () -> Just $ n + 1
    , isFinal      = const False
    }

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

counterExample :: IO ()
counterExample = runChainM' [("Alice", 1000)] $ do
    (sid, t) <- deployStateMachine counter trivialCont "Alice"
    replicateM_ 42 $ count sid t
