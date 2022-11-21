{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OnChain where

import Ledger
  ( CurrencySymbol,
    MintingPolicy,
    PaymentPubKeyHash,
    PubKeyHash,
    TokenName,
    Value,
    mkMintingPolicyScript,
    unPaymentPubKeyHash,
  )
import qualified Ledger.Scripts as Scripts
import Ledger.Value (AssetClass (AssetClass), assetClassValue)
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Api as Ledger
  ( scriptContextTxInfo,
    txInfoSignatories,
  )
import Plutus.V1.Ledger.Contexts
  ( ScriptContext,
    TxInfo,
  )
import qualified PlutusTx
import PlutusTx.Prelude

{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: (PaymentPubKeyHash, Integer) -> () -> ScriptContext -> Bool
mkMintingPolicy (pkh, _) _ ctx =
  traceIfFalse "Not signed by the correct PubKeyHash" $
    signer == unPaymentPubKeyHash pkh
  where
    info :: TxInfo
    !info = Ledger.scriptContextTxInfo ctx

    signer :: PubKeyHash
    [signer] = Ledger.txInfoSignatories info

tokenMintingPolicy :: PaymentPubKeyHash -> Integer -> MintingPolicy
tokenMintingPolicy pkh n =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy . mkMintingPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode (pkh, n)

tokenCurrencySymbol :: PaymentPubKeyHash -> Integer -> CurrencySymbol
tokenCurrencySymbol pkh n = Scripts.scriptCurrencySymbol $ tokenMintingPolicy pkh n

-- Change to make new token name as needed
testTokenName :: TokenName
testTokenName = "TT"

-- Change to make new currency symbol as needed
testParam :: Integer
testParam = 42

testTokenAsset :: PaymentPubKeyHash -> AssetClass
testTokenAsset pkh = AssetClass (tokenCurrencySymbol pkh testParam, testTokenName)

testTokenValue :: PaymentPubKeyHash -> Integer -> Value
testTokenValue pkh = assetClassValue (testTokenAsset pkh)

testTokenMintingPolicy :: PaymentPubKeyHash -> MintingPolicy
testTokenMintingPolicy pkh = tokenMintingPolicy pkh testParam
