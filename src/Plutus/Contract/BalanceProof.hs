{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

-- Interface to Plutus.Contract.Oracle
module Plutus.Contract.BalanceProof where

import           GHC.Generics           (Generic)
import           Ledger.Contexts        (ScriptContext (..))
import           Ledger.Crypto          (PubKey (..))
import qualified Ledger.Crypto          as Crypto
import qualified Plutus.Contract.Oracle as Oracle
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                as Haskell

data BalanceProof = BalanceProof
  { bpBalance1 :: Integer
  , bpBalance2 :: Integer
  , bpOrder    :: Integer
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''BalanceProof

PlutusTx.makeIsDataIndexed ''BalanceProof [('BalanceProof, 0)]

data BalanceProofCheck = BalanceProofCheck
  { bpcSignature1 :: Oracle.SignedMessage BalanceProof
  , bpcSignature2 :: Oracle.SignedMessage BalanceProof
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''BalanceProofCheck

PlutusTx.makeIsDataIndexed ''BalanceProofCheck [('BalanceProofCheck, 0)]

signBalanceProof ::
     BalanceProof -> Crypto.PrivateKey -> Oracle.SignedMessage BalanceProof
signBalanceProof = Oracle.signMessage

{-# INLINABLE verifyBalanceProofOnChain #-}
verifyBalanceProofOnChain :: ScriptContext
    -> PubKey
    -> Oracle.SignedMessage BalanceProof
    -> Maybe BalanceProof
verifyBalanceProofOnChain ctx pk msg = case Oracle.verifySignedMessageOnChain ctx pk msg of
  Right bp -> Just bp
  Left err -> Nothing

verifyBalanceProofOffChain ::
    PubKey
    -> Oracle.SignedMessage BalanceProof
    -> Maybe BalanceProof
verifyBalanceProofOffChain pk msg = case Oracle.verifySignedMessageOffChain pk msg of
  Right bp -> Just bp
  Left err -> Nothing
