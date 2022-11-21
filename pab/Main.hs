{-# LANGUAGE TypeApplications #-}

import           PAB                 (AppContracts (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                      (runWith)

main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @AppContracts)
