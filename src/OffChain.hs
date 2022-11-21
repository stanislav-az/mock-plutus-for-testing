{-# LANGUAGE DataKinds #-}
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
import qualified Data.Map as Map
import Data.Monoid (Last (Last))
import Ledger
  ( Address,
    PaymentPubKeyHash,
    Value,
    pubKeyHashAddress,
  )
import Ledger.Tx (ciTxOutValue)
import Plutus.Contract
  ( AsContractError,
    Contract,
    ContractError,
    Endpoint,
    Promise,
    endpoint,
    ownFirstPaymentPubKeyHash,
    select,
    tell,
    utxosAt,
    type (.\/),
  )

type AppSchema =
  Endpoint "fundsAt" PaymentPubKeyHash
    .\/ Endpoint "ownFirstPaymentPubKeyHash" ()

endpoints :: Promise (Last AppResponse) AppSchema ContractError ()
endpoints =
  ( endpoint @"fundsAt" (fundsAt >=> tellAppResponse FundsAt)
      `select` endpoint @"ownFirstPaymentPubKeyHash" (const ownFirstPaymentPubKeyHash >=> tellAppResponse OwnFirstPaymentPubKeyHash)
  )
    <> endpoints

data AppResponse
  = OwnFirstPaymentPubKeyHash PaymentPubKeyHash
  | FundsAt Value

tellAppResponse :: forall a. (a -> AppResponse) -> a -> Contract (Last AppResponse) AppSchema ContractError ()
tellAppResponse wrapper = tell . Last . Just . wrapper

-- | Gets all UTxOs belonging to the user and concats them into one Value
fundsAt :: PaymentPubKeyHash -> Contract (Last AppResponse) AppSchema ContractError Value
fundsAt pkh = utxosValue (const True) $ pubKeyHashAddress pkh Nothing

utxosValue :: (AsContractError e) => (Value -> Bool) -> Address -> Contract w s e Value
utxosValue p address = do
  os <- Map.elems <$> utxosAt address
  pure $ mconcat [val | o <- os, let val = view ciTxOutValue o, p val]
