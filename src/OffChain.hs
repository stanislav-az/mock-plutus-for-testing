{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module OffChain where

import Control.Lens (view)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Monoid (Last (Last))
import GHC.Generics (Generic)
import Ledger
  ( Address,
    AssetClass,
    PaymentPubKeyHash,
    Value,
    getCardanoTxId,
    pubKeyHashAddress,
  )
import qualified Ledger.Constraints as Constraints
import Ledger.Tx (ciTxOutValue)
import qualified Ledger.Typed.Scripts as Scripts
import OnChain
import Plutus.Contract
  ( AsContractError,
    Contract,
    ContractError,
    Endpoint,
    Promise,
    awaitTxConfirmed,
    endpoint,
    ownFirstPaymentPubKeyHash,
    select,
    submitTxConstraintsWith,
    tell,
    utxosAt,
    type (.\/),
  )

type AppSchema =
  Endpoint "fundsAt" PaymentPubKeyHash
    .\/ Endpoint "ownFirstPaymentPubKeyHash" ()
    .\/ Endpoint "mintAsset" Integer

endpoints :: Promise (Last AppResponse) AppSchema ContractError ()
endpoints =
  ( endpoint @"fundsAt" (fundsAt >=> tellAppResponse FundsAt)
      `select` endpoint @"ownFirstPaymentPubKeyHash" (const ownFirstPaymentPubKeyHash >=> tellAppResponse OwnFirstPaymentPubKeyHash)
      `select` endpoint @"mintAsset" (mintAsset >=> tellAppResponse MintAsset)
  )
    <> endpoints

data AppResponse
  = OwnFirstPaymentPubKeyHash PaymentPubKeyHash
  | FundsAt Value
  | MintAsset AssetClass
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

tellAppResponse :: forall a. (a -> AppResponse) -> a -> Contract (Last AppResponse) AppSchema ContractError ()
tellAppResponse wrapper = tell . Last . Just . wrapper

-- | Gets all UTxOs belonging to the user and concats them into one Value
fundsAt :: PaymentPubKeyHash -> Contract (Last AppResponse) AppSchema ContractError Value
fundsAt pkh = utxosValue (const True) $ pubKeyHashAddress pkh Nothing

utxosValue :: (AsContractError e) => (Value -> Bool) -> Address -> Contract w s e Value
utxosValue p address = do
  os <- Map.elems <$> utxosAt address
  pure $ mconcat [val | o <- os, let val = view ciTxOutValue o, p val]

mintAsset ::
  Integer ->
  Contract (Last AppResponse) AppSchema ContractError AssetClass
mintAsset amount = do
  pkh <- ownFirstPaymentPubKeyHash
  let mintingPolicy = testTokenMintingPolicy pkh
  let asset = testTokenAsset pkh
  let lookups = Constraints.plutusV1MintingPolicy mintingPolicy
  let mintTx = Constraints.mustMintValue (testTokenValue pkh amount)
  tx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
  _ <- awaitTxConfirmed (getCardanoTxId tx)
  pure asset
