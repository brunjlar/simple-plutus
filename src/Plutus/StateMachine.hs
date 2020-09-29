{-# LANGUAGE ScopedTypeVariables #-}

module Plutus.StateMachine
    (
    ) where

{-
import Control.Monad

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
stateMachineScript sm token cont = Script $ \i outputs tx -> do

    assert (tokenAmount token value == 1) $
        validationError "unique token is not present in the old output"

    state         <- case fromDatum datum of
                        Nothing -> throwError "datum has the wrong type"
                        Just s  -> return (s :: s)
    transition    <- case fromDynamic $ getRedeemer index tx of
                        Nothing -> throwError "redeemer has the wrong type"
                        Just t  -> return (t :: t)
    expectedState <- case transit sm state transition of
                        Nothing -> throwError "transition not allowed in this state"
                        Just s  -> return s

    if isFinal sm expectedState
        then do
            unless (null $ relevantOutputs sid tx) $
                throwError "no output at the script address expected in final state"
            toEither $ runScript (cont state transition Nothing) sid value datum index outputs tx
        else do
            output        <- case relevantOutputs sid tx of
                                [o] -> return o
                                _   -> throwError "expected exactly one output at the script address for the state machine"
            unless (tokenAmount token (output ^. oValue) == 1) $
                throwError "unique token is not present in the new output"

            actualState   <- case fromDynamic $ output ^. oDatum of
                                Nothing -> throwError "output datum has the wrong type"
                                Just s  -> return (s :: s)
            unless (actualState == expectedState) $
                throwError "actual state of the output does not agree with the expected state"

            toEither $ runScript (cont state transition (Just (expectedState, output))) sid value datum index outputs tx

  where
    relevantOutputs :: ScriptId -> Tx -> [Output]
    relevantOutputs sid tx = [o | o <- tx ^. txOutputs, o ^. oAddress == ScriptAddress sid]

trivialCont :: s -> t -> Maybe (s, Output) -> Script
trivialCont _s _t _mso = Script $ \_sid _value _datum _index _outputs _tx -> Validated

-- | monetary policy of our unique token
uniqueTokenScript :: String      -- ^ token name
                  -> (TxId, Int) -- ^ "pointer" to an output that must be consumed during forging
                  -> Script
uniqueTokenScript tn (tid, i) = Script $ \sid _value _datum _index _outputs tx -> fromEither $ do
    let token = Token sid tn
    unless (tokenAmount token (tx ^. txForge) == 1) $
        throwError "unique token must be forged with amount one"
    unless (any (\input -> input ^. iTxId == tid &&
                           input ^. iIx   == i) $ tx ^. txInputs) $
        throwError "required output not consumed by forging transaction"

deployStateMachine :: (Eq s, Typeable s, Typeable t)
                   => StateMachine s t
                   -> (s -> t -> Maybe (s, Output) -> Script) -- ^ continuation script
                   -> PubKey                                  -- ^ "owner" of the state machine
                   -> ChainM (ScriptId, TxId, Token)
deployStateMachine sm cont owner = do

    -- create output that will be required to be consumed during forging of the unique token
    tid1 <- freshTxId
    addTx Tx
        { _txId        = tid1
        , _txInputs    = []
        , _txSignees   = [owner]
        , _txOutputs   = [ Output (PKAddress owner) mempty unit ]
        , _txSlotRange = SlotRange 0 Forever
        , _txForge     = mempty
        }

    uniqueTokenSid <- uploadScript $ uniqueTokenScript "STATEMACHINE" (tid1, 0)
    let token = Token uniqueTokenSid "STATEMACHINE"

    -- create output at the address of the monetary policy of our unique token
    tid2 <- freshTxId
    addTx Tx
        { _txId        = tid2
        , _txInputs    = []
        , _txSignees   = [owner]
        , _txOutputs   = [ Output (ScriptAddress uniqueTokenSid) mempty unit ]
        , _txSlotRange = SlotRange 0 Forever
        , _txForge     = mempty
        }

    -- forge the unique token
    tid3 <- freshTxId
    addTx Tx
        { _txId        = tid3
        , _txInputs    = [ Input tid1 0 unit
                         , Input tid2 0 unit
                         ]
        , _txSignees   = [owner]
        , _txOutputs   = [ Output (PKAddress owner) (fromToken token 1) unit ]
        , _txSlotRange = SlotRange 0 Forever
        , _txForge     = fromToken token 1
        }

    stateMachineSid <- uploadScript $ stateMachineScript sm token cont
    tid4 <- freshTxId
    addTx Tx
        { _txId        = tid4
        , _txInputs    = [Input tid3 0 unit]
        , _txSignees   = [owner]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress stateMachineSid
                            , _oValue   = fromToken token 1
                            , _oDatum   = toDyn $ initialState sm
                            }
                         ]
        , _txSlotRange = SlotRange 0 Forever
        , _txForge     = mempty
        }

    return (stateMachineSid, tid4, token)
-}
