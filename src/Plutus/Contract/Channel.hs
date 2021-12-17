{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.Contract.Channel where

import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                      as Haskell

import           Data.Text                    (Text)
import           Ledger                       (Datum (..), TxId, getCardanoTxId)
import qualified Ledger
import qualified Ledger.Constraints           as Constraints
import           Ledger.Contexts              (ScriptContext (..))
import           Ledger.Crypto                (PubKey (..))
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              (AsContractError, Contract,
                                               mkTxConstraints,
                                               submitUnbalancedTx, throwError,
                                               utxosAt)
import qualified Plutus.Contract.BalanceProof as BP
import qualified Plutus.Contract.Typed.Tx     as Typed
import qualified Plutus.V1.Ledger.Ada         as Ada

-- Channel Script
data ChannelParams =
    ChannelParams
        { party1 :: PubKey
        , party2 :: PubKey
        }

PlutusTx.unstableMakeIsData ''ChannelParams
PlutusTx.makeLift ''ChannelParams

data Action = CloseChannel BP.BalanceProofCheck

data Channel
instance Scripts.ValidatorTypes Channel where
    type instance RedeemerType Channel = Action
    type instance DatumType Channel = ()

PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

{-# INLINABLE validate #-}
validate :: ChannelParams -> () -> Action -> ScriptContext -> Bool
validate ChannelParams{..} () (CloseChannel BP.BalanceProofCheck{..}) ctx =
            traceIfFalse "invalid party 1 signature" (validateSignature party1 bpcSignature1)
            && traceIfFalse "invalid party 2 signature" (validateSignature party2 bpcSignature2)
        where
          validateSignature party sig = isJust $ BP.verifyBalanceProof ctx party sig

typedValidator :: ChannelParams -> Scripts.TypedValidator Channel
typedValidator = Scripts.mkTypedValidatorParam @Channel
        $$(PlutusTx.compile [|| validate ||])
        $$(PlutusTx.compile [|| wrap ||])
    where wrap = Scripts.wrapValidator

-- Channel Endpoints
openChannel ::
    forall w s e.
    ( AsContractError e
    )
    => ChannelParams
    -> Ledger.Ada
    -> Contract w s e TxId
openChannel cp adaAmount = do
    let channelScript = typedValidator cp
    let constraints = Constraints.mustPayToTheScript () $ Ada.toValue adaAmount
    let lookups = Constraints.typedValidatorLookups channelScript
    utx <- mkTxConstraints lookups constraints
    getCardanoTxId <$> submitUnbalancedTx (Constraints.adjustUnbalancedTx utx)

closeChannel ::
    forall w s.
    ChannelParams
    -> BP.BalanceProofCheck
    -> Contract w s Text TxId
closeChannel cp sigs = do
    let channelScript = typedValidator cp
    unspentOutputs <- utxosAt (Scripts.validatorAddress channelScript)
    let constraints = Typed.collectFromScript unspentOutputs (CloseChannel sigs)
                       <> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData sigs)
    let lookups = Constraints.typedValidatorLookups channelScript
                    Haskell.<> Constraints.unspentOutputs unspentOutputs
    if Constraints.modifiesUtxoSet constraints
    then do
        utx <- mkTxConstraints lookups constraints
        getCardanoTxId <$> submitUnbalancedTx (Constraints.adjustUnbalancedTx utx)
    else throwError "Close channel failed"