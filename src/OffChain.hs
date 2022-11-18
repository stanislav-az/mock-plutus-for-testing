{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module OffChain where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Void                (Void)
import           Ledger                   (PaymentPubKeyHash, Value,
                                           pubKeyHashAddress)
import           Plutus.Contract          (AsContractError, Contract, Promise,
                                           ownFirstPaymentPubKeyHash, select,
                                           throwError)
import           Control.Lens         (view, (^?))
import           Control.Monad
import qualified Data.Map             as Map
import           Data.Maybe           (catMaybes)
import           Ledger               (Address, TxOutRef, Value)
import           Ledger.Tx            (ChainIndexTxOut, ciTxOutScriptDatum,
                                       ciTxOutValue)
import           Plutus.Contract
import           Plutus.V1.Ledger.Api (Datum (getDatum))
import qualified PlutusTx
import Data.Monoid

type AppSchema =
    Endpoint "fundsAt" PaymentPubKeyHash
    .\/ Endpoint "ownFirstPaymentPubKeyHash" ()

endpoints :: Promise (Last AppResponse) AppSchema ContractError ()
endpoints =
    (endpoint @"fundsAt" (fundsAt >=> tellAppResponse FundsAt)
    `select` endpoint @"ownFirstPaymentPubKeyHash" ((const ownFirstPaymentPubKeyHash) >=> tellAppResponse OwnFirstPaymentPubKeyHash))
    <> endpoints

data AppResponse
    = OwnFirstPaymentPubKeyHash PaymentPubKeyHash
    | FundsAt Value

tellAppResponse :: forall a. (a -> AppResponse) -> a -> Contract (Last AppResponse) AppSchema ContractError ()
tellAppResponse wrapper = tell . Last . Just . wrapper

-- | Gets all UTxOs belonging to the user and concats them into one Value
fundsAt :: PaymentPubKeyHash  -> Contract (Last AppResponse) AppSchema ContractError Value
fundsAt pkh = utxosValue (const True) $ pubKeyHashAddress pkh Nothing

utxosValue :: (AsContractError e) => (Value -> Bool) -> Address -> Contract w s e Value
utxosValue p address = do
    os  <- Map.elems <$> utxosAt address
    pure $ mconcat [val | o <- os, let val = view ciTxOutValue o, p val]
