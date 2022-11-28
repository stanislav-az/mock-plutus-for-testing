{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api (Error (displayError), writeFileTextEnvelope)
import Ledger
  ( PaymentPubKeyHash (PaymentPubKeyHash),
    PubKeyHash (PubKeyHash),
  )
import OnChain (cardanoApiMintingScript)
import Options
  ( MintContractOpts
      ( MintContractOpts,
        number,
        pubKeyHash,
        scriptPath
      ),
    options,
  )
import Options.Applicative (execParser)
import Plutus.V2.Ledger.Api (toBuiltin)
import Prelude

main :: IO ()
main = do
  MintContractOpts {..} <- execParser options
  let pkh = PaymentPubKeyHash . PubKeyHash . toBuiltin $ pubKeyHash
  result <- writeFileTextEnvelope scriptPath Nothing $ cardanoApiMintingScript pkh number
  case result of
    Left err -> putStrLn $ displayError err
    Right () -> pure ()
