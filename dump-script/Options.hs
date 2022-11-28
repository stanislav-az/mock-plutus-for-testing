{-# LANGUAGE DerivingStrategies #-}

module Options where

import Data.ByteString (ByteString)
import Options.Applicative

data MintContractOpts = MintContractOpts
  { pubKeyHash :: ByteString,
    number :: Integer,
    scriptPath :: FilePath
  }
  deriving stock (Show, Eq)

mintContractOpts :: Parser MintContractOpts
mintContractOpts =
  MintContractOpts
    <$> strOption
      ( long "pub-key-hash"
          <> metavar "PUB_KEY_HASH"
          <> help "Public key hash of token owner"
      )
    <*> option
      auto
      ( long "number"
          <> short 'n'
          <> help "Parametrize script to get a unique currency symbol"
          <> showDefault
          <> value 42
          <> metavar "INT"
      )
    <*> strOption
      ( long "path"
          <> help "Path where the script is written"
          <> showDefault
          <> value "dump-script/mintingScript.plutus"
          <> metavar "SCRIPT_PATH"
      )

options :: ParserInfo MintContractOpts
options =
  info
    (mintContractOpts <**> helper)
    ( fullDesc
        <> progDesc "Dump script byte code to a file"
        <> header "dump-script - write plutus script to a file"
    )
