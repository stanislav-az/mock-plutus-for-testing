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

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger (CurrencySymbol, PaymentPubKeyHash (..), PubKeyHash, TokenName, Value)
import Ledger.Value (AssetClass (AssetClass), assetClassValue)
import qualified Plutus.Script.Utils.V1.Scripts as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Api as Ledger
import qualified Plutus.V1.Ledger.Contexts as V1
import qualified PlutusTx
import PlutusTx.Prelude

{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: (PaymentPubKeyHash, Integer) -> () -> V1.ScriptContext -> Bool
mkMintingPolicy (pkh, _) _ ctx =
  traceIfFalse "Not signed by the correct PubKeyHash" $
    signer == unPaymentPubKeyHash pkh
  where
    info :: V1.TxInfo
    !info = Ledger.scriptContextTxInfo ctx

    signer :: PubKeyHash
    [signer] = Ledger.txInfoSignatories info

tokenMintingPolicy :: PaymentPubKeyHash -> Integer -> Scripts.MintingPolicy
tokenMintingPolicy pkh n =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy . mkMintingPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode (pkh, n)

tokenCurrencySymbol :: PaymentPubKeyHash -> Integer -> CurrencySymbol
tokenCurrencySymbol pkh n = Scripts.scriptCurrencySymbol $ tokenMintingPolicy pkh n

---------------------------------------------- ::Test token:: ----------------------------------------------
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

testTokenMintingPolicy :: PaymentPubKeyHash -> Scripts.MintingPolicy
testTokenMintingPolicy pkh = tokenMintingPolicy pkh testParam

---------------------------------------------- ::Script serialization:: ----------------------------------------------
plutusScript :: PaymentPubKeyHash -> Integer -> Ledger.Script
plutusScript pkh n =
  Ledger.unMintingPolicyScript (tokenMintingPolicy pkh n)

validator :: PaymentPubKeyHash -> Integer -> Scripts.Validator
validator pkh n = Ledger.Validator $ plutusScript pkh n

scriptAsCbor :: PaymentPubKeyHash -> Integer -> LB.ByteString
scriptAsCbor pkh n = serialise $ validator pkh n

cardanoApiMintingScript :: PaymentPubKeyHash -> Integer -> PlutusScript PlutusScriptV1
cardanoApiMintingScript pkh n = PlutusScriptSerialised . SBS.toShort . LB.toStrict $ scriptAsCbor pkh n

scriptShortBs :: PaymentPubKeyHash -> Integer -> SBS.ShortByteString
scriptShortBs pkh n = SBS.toShort . LB.toStrict $ scriptAsCbor pkh n
