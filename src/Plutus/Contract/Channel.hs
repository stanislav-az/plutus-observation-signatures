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

import           Data.Aeson                   (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema          as OpenApi
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           GHC.Generics                 (Generic)
import           Ledger                       (Datum (..), TxId, getCardanoTxId)
import qualified Ledger
import qualified Ledger.Constraints           as Constraints
import           Ledger.Contexts              (ScriptContext (..))
import           Ledger.Crypto                (PubKey (..))
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              (AsContractError, Contract,
                                               logError, logInfo,
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
    deriving (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

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
          validateSignature party sig = isJust $ BP.verifyBalanceProofOnChain ctx party sig

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
    -> Contract w s e ()
openChannel cp adaAmount = do
    logInfo @Text $ "Starting Channel With: " Haskell.<> show cp
    let channelScript = typedValidator cp
    let constraints = Constraints.mustPayToTheScript () $ Ada.toValue adaAmount
    let lookups = Constraints.typedValidatorLookups channelScript
    utx <- mkTxConstraints lookups constraints
    txId <- getCardanoTxId <$> submitUnbalancedTx (Constraints.adjustUnbalancedTx utx)
    logInfo @Text $ "Opened Channel: " Haskell.<> show txId

closeChannel ::
    forall w s.
    ChannelParams
    -> BP.BalanceProofCheck
    -> Contract w s Text ()
closeChannel cp@ChannelParams{..} sigs@BP.BalanceProofCheck{..} = do
    logInfo @Text $ "Closing Channel With: " Haskell.<> show cp
    bp1 <- maybe (throw "Off-chain party 1 signature invalid") pure $ BP.verifyBalanceProofOffChain party1 bpcSignature1
    bp2 <- maybe (throw "Off-chain party 2 signature invalid") pure $ BP.verifyBalanceProofOffChain party2 bpcSignature2
    unless (bp1 Haskell.== bp2) $ throw "Balance proofs are not equal"
    
    let channelScript = typedValidator cp
    unspentOutputs <- utxosAt (Scripts.validatorAddress channelScript)
    let constraints = Typed.collectFromScript unspentOutputs (CloseChannel sigs)
                       <> Constraints.mustIncludeDatum (Datum $ PlutusTx.toBuiltinData bp2)
    let lookups = Constraints.typedValidatorLookups channelScript
                    Haskell.<> Constraints.unspentOutputs unspentOutputs
    txId <- if Constraints.modifiesUtxoSet constraints
            then do
                utx <- mkTxConstraints lookups constraints
                getCardanoTxId <$> submitUnbalancedTx (Constraints.adjustUnbalancedTx utx)
            else throw "Close channel failed"
    logInfo @Text $ "Closed Channel: " Haskell.<> show txId
    where
        throw e = logError e >> throwError e

show :: Haskell.Show a => a -> Text
show = Text.pack . Haskell.show
