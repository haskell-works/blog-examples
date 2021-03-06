{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.SumBitVectors where

import App.Commands.Types                (SumBitVectorsOptions (SumBitVectorsOptions))
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.Generics.Product.Any
import Data.Word
import GHC.Int
import GHC.Prim
import GHC.Stats
import GHC.Word                          hiding (ltWord)
import HaskellWorks.Data.Bits.BitWise
import Options.Applicative
import System.Posix.Process
import System.Process

import qualified Data.ByteString.Lazy                 as BS
import qualified Data.Vector.Storable                 as DVS
import qualified Data.Vector.Storable.Mutable         as DVSM
import qualified HaskellWorks.Data.Vector.Storable    as DVS
import qualified Ops.SumBitVectors.Word64.Branchiest  as BRANCHIEST
import qualified Ops.SumBitVectors.Word64.Branchless2 as BRANCHLESS2
import qualified Ops.SumBitVectors.Word64.Branchier   as BRANCHIER
import qualified Ops.SumBitVectors.Word64.Branchless  as BRANCHLESS
import qualified Ops.SumBitVectors.Word64.Branchy     as BRANCHY
import qualified System.Environment                   as IO
import qualified System.IO                            as IO

runSumBitVectors :: SumBitVectorsOptions -> IO ()
runSumBitVectors opts = do
  let filePaths = opts ^. the @"filePaths"

  vs <- forM filePaths DVS.mmap
  let !sv = case opts ^. the @"branchiness" of
        "branchless" -> BRANCHLESS.sumBitVectors  vs
        "branchless2"-> BRANCHLESS2.sumBitVectors vs
        "branchy"    -> BRANCHY.sumBitVectors     vs
        "branchier"  -> BRANCHIER.sumBitVectors   vs
        "branchiest" -> BRANCHIEST.sumBitVectors  vs

  IO.putStrLn $ "Vector length: " <> show (DVS.length sv) <> ", Branchless: " <> show (opts ^. the @"branchiness")

  return ()

optsSumBitVectors :: Parser SumBitVectorsOptions
optsSumBitVectors = SumBitVectorsOptions
  <$> many
      ( strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input JSON file"
        <>  metavar "STRING"
        )
      )
  <*> strOption
      ( long "branchiness"
      <>  short 'b'
      <>  help "Branchiness of addition (branchless|branchy|branchful)"
      )

cmdSumBitVectors :: Mod CommandFields (IO ())
cmdSumBitVectors = command "sum-bit-vectors" $ flip info idm $ runSumBitVectors <$> optsSumBitVectors
