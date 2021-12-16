{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Plutus.Contract.BalanceProof where

import GHC.Generics (Generic)
import qualified Ledger.Crypto as Crypto
import qualified Plutus.Contract.Oracle as Oracle
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

data BalanceProof = BalanceProof
  { bpBalance1 :: Integer
  , bpBalance2 :: Integer
  , bpOrder :: Integer
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
