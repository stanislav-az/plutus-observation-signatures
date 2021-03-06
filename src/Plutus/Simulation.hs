{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeApplications   #-}

module Plutus.Simulation   where

import           Data.Aeson                          (FromJSON, Result (..),
                                                      ToJSON, encode, fromJSON)
import           GHC.Generics                        (Generic)
import qualified Plutus.Contract.BalanceProof        as BP
import qualified Plutus.Contract.Channel             as Chan
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..),
                                                      type (.\\))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (Simulation,
                                                      SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
-- import qualified Data.OpenApi.Schema  as OpenApi
import           Control.Monad                       (forM, forM_, void, when)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Default                        (Default (def))
import qualified Data.OpenApi.Schema                 as OpenApi
import           Data.Text                           (Text)
import qualified Ledger
import qualified Ledger.CardanoWallet                as CW
import           Plutus.PAB.Effects.Contract.Builtin (Builtin,
                                                      BuiltinHandler (..),
                                                      HasDefinitions (..),
                                                      SomeBuiltin (..))
import qualified Plutus.PAB.Webserver.Server         as PAB
import qualified Plutus.V1.Ledger.Ada                as Ada
import           Prettyprinter                       (Pretty (..), viaShow)
import qualified Wallet.Emulator.Types               as Wallet
import qualified Wallet.Emulator.Wallet              as Wallet

data ChannelContracts =
      OpenChannel Chan.ChannelParams Ledger.Ada
    | CloseChannel Chan.ChannelParams BP.BalanceProofCheck
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty ChannelContracts where
    pretty = viaShow

instance HasDefinitions ChannelContracts where
    getDefinitions = []
    getSchema = \case
        OpenChannel _ _  -> Builtin.endpointsToSchemas @Builtin.EmptySchema
        CloseChannel _ _ -> Builtin.endpointsToSchemas @Builtin.EmptySchema
    getContract = \case
        OpenChannel cp a  -> SomeBuiltin $ Chan.openChannel @() @Builtin.EmptySchema @Text cp a
        CloseChannel cp bpc -> SomeBuiltin $ Chan.closeChannel @() @Builtin.EmptySchema cp bpc

handlers :: SimulatorEffectHandlers (Builtin ChannelContracts)
handlers =
    Simulator.mkSimulatorHandlers def def
    $ interpret (contractHandler (Builtin.handleBuiltin @ChannelContracts))

channelSimulation :: Simulation (Builtin ChannelContracts) ()
channelSimulation = do
    let party1Wallet = Wallet.knownWallet 1
    let party1PubKey = Wallet.walletPubKey party1Wallet
    let party1PrivKey =  knownWalletPrivKey 1


    let party2Wallet = Wallet.knownWallet 2
    let party2PubKey = Wallet.walletPubKey party2Wallet
    let party2PrivKey =  knownWalletPrivKey 2

    let channelParams = Chan.ChannelParams
                            { Chan.party1 = party1PubKey
                            , Chan.party2 = party2PubKey
                            }
    let channelAda = Ada.lovelaceOf $ oneAdaInLovelace * 50
    cidOpenChan  <- Simulator.activateContract party1Wallet $ OpenChannel channelParams channelAda
    _        <- Simulator.waitUntilFinished cidOpenChan
    _        <- Simulator.waitNSlots 10

    let balanceProof = BP.BalanceProof
                        { BP.bpBalance1 = oneAdaInLovelace * 6
                        , BP.bpBalance2 = oneAdaInLovelace * 15
                        , BP.bpOrder    = 5
                        }
    let balanceProofCheck = BP.BalanceProofCheck
            { BP.bpcSignature1 = BP.signBalanceProof balanceProof party1PrivKey
            , BP.bpcSignature2 = BP.signBalanceProof balanceProof party2PrivKey
            }
    cidCloseChan  <- Simulator.activateContract party1Wallet $ CloseChannel channelParams balanceProofCheck
    _        <- Simulator.waitUntilFinished cidCloseChan
    _        <- Simulator.waitNSlots 5

    pure ()

runChannelSimulation :: IO ()
runChannelSimulation = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin ChannelContracts) "Starting PAB webserver. Press enter to exit."
    shutdown <- PAB.startServerDebug

    _  <- channelSimulation

    _ <- liftIO getLine
    shutdown

knownWalletPrivKey :: Integer -> Ledger.PrivateKey
knownWalletPrivKey =  Wallet.ownPrivateKey . Wallet.fromMockWallet . CW.fromWalletNumber . CW.WalletNumber

oneAdaInLovelace :: Integer
oneAdaInLovelace = 1000000
