{-# LANGUAGE ScopedTypeVariables #-}

module Plutus.StateMachine
    ( StateMachine (..)
    , trivialCont
    , deployStateMachine
    , stateMachineOutput
    ) where

import Control.Monad
import Optics

import Plutus.Chain
import Plutus.Types
import Plutus.Utils
import Plutus.Validation

data StateMachine s t = StateMachine
    { initialState :: s
    , transit      :: s -> t -> Maybe s
    , isFinal      :: s -> Bool
    }

stateMachineScript :: forall s t. (Eq s, Typeable s, Typeable t)
                   => StateMachine s t
                   -> Token
                   -> (s -> t -> Maybe (s, Output) -> Script)
                   -> Script
stateMachineScript sm token cont = do

    v <- ownValue
    assertS (tokenAmount token v == 1) $ "unique token is not present in the old output"

    state         <- ownDatum
    transition    <- ownRedeemer
    expectedState <- case transit sm state transition of
                        Nothing -> throwError "transition not allowed in this state"
                        Just s  -> return s

    sid <- ownScriptId
    tx  <- txS
    if isFinal sm expectedState
        then do
            assertS (null $ relevantOutputs sid tx) $ "no output at the script address expected in final state"
            cont state transition Nothing
        else do
            output        <- case relevantOutputs sid tx of
                                [o] -> return o
                                _   -> throwError "expected exactly one output at the script address for the state machine"
            assertS (tokenAmount token (output ^. oValue) == 1) $
                "unique token is not present in the new output"

            actualState <- fromDatumS $ output ^. oDatum
            assertS (actualState == expectedState) $
                "actual state of the output does not agree with the expected state"

            cont state transition $ Just (expectedState, output)

  where
    relevantOutputs :: ScriptId -> Tx -> [Output]
    relevantOutputs sid tx =
        [ o
        | o <- tx ^. txOutputs
        , o ^. oAddress == ScriptAddr sid
        ]

trivialCont :: s -> t -> Maybe (s, Output) -> Script
trivialCont _s _t _mso = return ()

-- | Monetary policy for a unique token.
uniqueTokenScript :: String    -- ^ token name
                  -> OutputPtr -- ^ pointer to an output that must be consumed during forging
                  -> Script
uniqueTokenScript tn ptr = do
    sid <- ownScriptId
    tx  <- txS
    let token = Token sid tn
    assertS (tokenAmount token (tx ^. txForge) == 1) $
        "unique token must be forged with amount one"

    assertS (anyOf (txInputs % each) (\input -> input ^. iOutputPtr == ptr) tx) $
        "required output not consumed by forging transaction"

deployStateMachine :: (Show s, Eq s, Typeable s, Typeable t)
                   => StateMachine s t
                   -> (s -> t -> Maybe (s, Output) -> Script) -- ^ continuation script
                   -> PubKey                                  -- ^ "owner" of the state machine
                   -> ChainM (ScriptId, Token)                -- ^ returns the state machine script id and the unique token
deployStateMachine sm cont owner = do

    -- find an output belonging to the owner
    (ptr, output) <- do
        xs <- outputsAt $ PKAddr owner
        case xs of
            []    -> throwError $ CustomError "owner owns no output"
            x : _ -> return x
    let v = output ^. oValue

    -- create output that will be required to be consumed during forging of the unique token
    tid1 <- addTx Tx
        { _txInputs    = [Input ptr unit]
        , _txSignees   = [owner]
        , _txOutputs   = [ Output (PKAddr owner) mempty unit
                         , Output (PKAddr owner) v unit
                         ]
        , _txSlotRange = always
        , _txForge     = mempty
        }

    let tn = "STATEMACHINE"
    uniqueTokenSid <- uploadScript $ uniqueTokenScript tn $ optr tid1 0
    let token = Token uniqueTokenSid tn -- the unique token

    -- create output at the address of the monetary policy of the unique token
    tid2 <- addTx Tx
        { _txInputs    = [Input (optr tid1 1) unit]
        , _txSignees   = [owner]
        , _txOutputs   = [ Output (ScriptAddr uniqueTokenSid) mempty unit
                         , Output (PKAddr owner) v unit
                         ]
        , _txSlotRange = always
        , _txForge     = mempty
        }

    -- forge the unique token
    let tv = fromToken token 1
    tid3 <- addTx Tx
        { _txInputs    = [ Input (optr tid1 0) unit
                         , Input (optr tid2 0) unit
                         ]
        , _txSignees   = [owner]
        , _txOutputs   = [Output (PKAddr owner) tv unit]
        , _txSlotRange = always
        , _txForge     = tv
        }

    -- deploy
    stateMachineSid <- uploadScript $ stateMachineScript sm token cont
    void $ addTx Tx
        { _txInputs    = [Input (optr tid3 0) unit]
        , _txSignees   = [owner]
        , _txOutputs   = [Output (ScriptAddr stateMachineSid) tv $ toDatum $ initialState sm]
        , _txSlotRange = always
        , _txForge     = mempty
        }

    return (stateMachineSid, token)

stateMachineOutput :: ScriptId -> Token -> ChainM (Maybe (OutputPtr, Output))
stateMachineOutput sid t = do
    xs <- outputsAt $ ScriptAddr sid
    return $ case filter (\(_, o) -> o ^. oValue % value t == 1) xs of
        [x] -> Just x
        _   -> Nothing
