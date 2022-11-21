{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module PAB where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema as OpenApi
import GHC.Generics (Generic)
import qualified OffChain as O
import Plutus.PAB.Effects.Contract.Builtin
  ( HasDefinitions (..),
    SomeBuiltin (..),
  )
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Prettyprinter (Pretty (..), viaShow)

data AppContracts
  = AppContracts
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty AppContracts where
  pretty = viaShow

instance HasDefinitions AppContracts where
  getDefinitions = [AppContracts]
  getSchema = \case
    AppContracts -> Builtin.endpointsToSchemas @O.AppSchema
  getContract = \case
    AppContracts -> SomeBuiltin O.endpoints
