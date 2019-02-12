{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE MagicHash        #-}
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

import qualified HaskellWorks.Data.Vector.Storable as DVS
import qualified Data.ByteString.Lazy         as BS
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM
import qualified System.Environment           as IO
import qualified System.IO                    as IO

ltWord :: Word64 -> Word64 -> Word64
ltWord (W64# a#) (W64# b#) = fromIntegral (I64# (ltWord# a# b#))
{-# INLINE ltWord #-}

branchlessSumCarry :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
branchlessSumCarry a b carry = (total, newCarry)
  where preTotal  = a + b
        total     = preTotal + carry
        newCarry  = total `ltWord` (a .|. b .|. carry)

branchfulSumCarry :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
branchfulSumCarry a b carry = (total, newCarry)
  where preTotal  = a + b
        total     = preTotal + carry
        newCarry  = if total < a .|. b .|. carry then 1 else 0
        
branchlessSumVector :: DVS.Vector Word64 -> DVS.Vector Word64 -> Word64 -> (Word64, DVS.Vector Word64)
branchlessSumVector u v carry = DVS.createT $ do
  w <- DVSM.new len
  go w 0 0
  return (undefined, w)
  where len = min (DVS.length u) (DVS.length v)
        go :: DVSM.MVector s Word64 -> Int -> Word64 -> ST s Word64
        go w i c = if i < len
          then do
            let (t, nc) = branchlessSumCarry (DVS.unsafeIndex u i) (DVS.unsafeIndex v i) c
            DVSM.unsafeWrite w i t
            go w (i + 1) nc
          else return c

branchfulSumVector :: DVS.Vector Word64 -> DVS.Vector Word64 -> Word64 -> (Word64, DVS.Vector Word64)
branchfulSumVector u v carry = DVS.createT $ do
  w <- DVSM.new len
  go w 0 0
  return (undefined, w)
  where len = min (DVS.length u) (DVS.length v)
        go :: DVSM.MVector s Word64 -> Int -> Word64 -> ST s Word64
        go w i c = if i < len
          then do
            let (t, nc) = branchfulSumCarry (DVS.unsafeIndex u i) (DVS.unsafeIndex v i) c
            DVSM.unsafeWrite w i t
            go w (i + 1) nc
          else return c
          
branchlessSumBitVectors :: [DVS.Vector Word64] -> DVS.Vector Word64
branchlessSumBitVectors []        = DVS.empty
branchlessSumBitVectors [v]       = v
branchlessSumBitVectors (v:w:vs)  = branchlessSumBitVectors (snd (branchlessSumVector v w 0):vs)

branchfulSumBitVectors :: [DVS.Vector Word64] -> DVS.Vector Word64
branchfulSumBitVectors []        = DVS.empty
branchfulSumBitVectors [v]       = v
branchfulSumBitVectors (v:w:vs)  = branchfulSumBitVectors (snd (branchfulSumVector v w 0):vs)

runSumBitVectors :: SumBitVectorsOptions -> IO ()
runSumBitVectors opts = do
  let filePaths = opts ^. the @"filePaths"

  vs <- forM filePaths DVS.mmap
  let !sv = if opts ^. the @"branchless"
        then branchlessSumBitVectors vs
        else branchfulSumBitVectors  vs

  IO.putStrLn $ "Vector length: " <> show (DVS.length sv) <> ", Branchless: " <> show (opts ^. the @"branchless")

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
  <*> switch
      ( long "branchless"
      <>  short 'b'
      <>  help "Enable branchless addition"
      )

cmdSumBitVectors :: Mod CommandFields (IO ())
cmdSumBitVectors = command "sum-bit-vectors" $ flip info idm $ runSumBitVectors <$> optsSumBitVectors
