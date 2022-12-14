{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api (Error (displayError), writeFileTextEnvelope)
import Ledger
  ( PaymentPubKeyHash (PaymentPubKeyHash),
    PubKeyHash (PubKeyHash),
  )
import OnChain (cardanoApiMintingScript, scriptShortBs)
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
import Plutus.V1.Ledger.ProtocolVersions (vasilPV)
import Plutus.V2.Ledger.Api (toBuiltin)
import qualified Plutus.V2.Ledger.Api as Plutus
import qualified PlutusCore as Plutus
import Prelude
import qualified Data.Aeson as J

main :: IO ()
main = do
  MintContractOpts {..} <- execParser options
  pkhParsed <- either error (pure . PubKeyHash) $ J.eitherDecodeStrict pubKeyHash
  let pkh = PaymentPubKeyHash pkhParsed
  costParams <- maybe (error "defaultCostModelParams failed") pure Plutus.defaultCostModelParams
  evalContext <- either (error . show) pure $ Plutus.mkEvaluationContext costParams
  let scriptParams = [Plutus.toData (pkh, number)]
  let (logout, e) = Plutus.evaluateScriptCounting vasilPV Plutus.Verbose evalContext (scriptShortBs pkh number) scriptParams
  putStrLn "Log output: " >> print logout
  case e of
    Left evalErr -> putStrLn "Eval Error: " >> print evalErr
    Right exbudget -> putStrLn "Ex Budget: " >> print exbudget
  result <- writeFileTextEnvelope scriptPath Nothing $ cardanoApiMintingScript pkh number
  case result of
    Left err -> putStrLn $ displayError err
    Right () -> putStrLn $ "Script written to " <> scriptPath
