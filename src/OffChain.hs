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
import Data.Foldable (Foldable (fold))
import qualified Data.Map as Map
import Data.Monoid (Last (Last))
import GHC.Generics (Generic)
import Ledger
  ( AssetClass,
    ChainIndexTxOut,
    PaymentPubKeyHash,
    Value,
    pubKeyHashAddress,
  )
import qualified Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import Ledger.Tx (ciTxOutValue)
import qualified Ledger.Typed.Scripts as Scripts
import OnChain
  ( testTokenAsset,
    testTokenMintingPolicy,
    testTokenValue,
  )
import Plutus.Contract
  ( Contract,
    ContractError,
    Endpoint,
    Promise,
    endpoint,
    ownFirstPaymentPubKeyHash,
    ownUtxos,
    select,
    submitTxConstraintsWith,
    tell,
    utxosAt,
    type (.\/),
  )

type AppSchema =
  Endpoint "fundsAt" PaymentPubKeyHash
    .\/ Endpoint "ownFunds" ()
    .\/ Endpoint "ownFirstPaymentPubKeyHash" ()
    .\/ Endpoint "mintAsset" Integer
    .\/ Endpoint "splitWalletValueBy" Integer

endpoints :: Promise (Last AppResponse) AppSchema ContractError ()
endpoints =
  ( endpoint @"fundsAt" (fundsAt >=> tellAppResponse FundsAt)
      `select` endpoint @"ownFirstPaymentPubKeyHash" (const ownFirstPaymentPubKeyHash >=> tellAppResponse OwnFirstPaymentPubKeyHash)
      `select` endpoint @"mintAsset" (mintAsset >=> tellAppResponse MintAsset)
      `select` endpoint @"ownFunds" (const ownFunds >=> tellAppResponse OwnFunds)
      `select` endpoint @"splitWalletValueBy" splitWalletValueBy
  )
    <> endpoints

data AppResponse
  = OwnFirstPaymentPubKeyHash PaymentPubKeyHash
  | FundsAt Value
  | OwnFunds Value
  | MintAsset AssetClass
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

tellAppResponse :: forall a. (a -> AppResponse) -> a -> Contract (Last AppResponse) AppSchema ContractError ()
tellAppResponse wrapper = tell . Last . Just . wrapper

-- | Gets all UTxOs belonging to the user and concats them into one Value
fundsAt :: PaymentPubKeyHash -> Contract (Last AppResponse) AppSchema ContractError Value
fundsAt pkh = do
  os <- utxosAt $ pubKeyHashAddress pkh Nothing
  pure $ utxosValue (const True) os

ownFunds :: Contract (Last AppResponse) AppSchema ContractError Value
ownFunds = do
  utxosValue (const True) <$> ownUtxos

utxosValue :: (Value -> Bool) -> Map.Map k ChainIndexTxOut -> Value
utxosValue p os =
  mconcat [val | o <- Map.elems os, let val = view ciTxOutValue o, p val]

mintAsset ::
  Integer ->
  Contract (Last AppResponse) AppSchema ContractError AssetClass
mintAsset amount = do
  pkh <- ownFirstPaymentPubKeyHash
  let mintingPolicy = testTokenMintingPolicy pkh
  let asset = testTokenAsset pkh
  let lookups = Constraints.plutusV1MintingPolicy mintingPolicy
  let mintTx =
        Constraints.mustMintValue (testTokenValue pkh amount)
          <> Constraints.mustBeSignedBy pkh
  _ <- submitTxConstraintsWith @Scripts.Any lookups mintTx
  pure asset

splitWalletValueBy ::
  Integer ->
  Contract (Last AppResponse) AppSchema ContractError ()
splitWalletValueBy n = do
  val <- ownFunds
  pkh <- ownFirstPaymentPubKeyHash
  let adaAmount = Ada.getLovelace $ Ada.fromValue val
  let splitAmount = adaAmount `div` n
  let outputs = replicate (fromIntegral n - 1) $ Constraints.mustPayToPubKey pkh (Ada.lovelaceValueOf splitAmount)
  let tx =
        fold outputs
          <> Constraints.mustBeSignedBy pkh
  _ <- submitTxConstraintsWith @Scripts.Any mempty tx
  pure ()
