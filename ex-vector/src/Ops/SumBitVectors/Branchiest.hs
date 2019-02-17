{-# LANGUAGE DataKinds #-}

module Ops.SumBitVectors.Branchiest where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.Generics.Product.Any
import Data.Word
import GHC.Int
import GHC.Prim
import GHC.Stats
import GHC.Word                       hiding (ltWord)
import HaskellWorks.Data.Bits.BitWise
import Options.Applicative
import System.Posix.Process
import System.Process

import qualified Data.ByteString.Lazy              as BS
import qualified Data.Vector.Storable              as DVS
import qualified Data.Vector.Storable.Mutable      as DVSM
import qualified HaskellWorks.Data.Vector.Storable as DVS
import qualified System.Environment                as IO
import qualified System.IO                         as IO

sumCarry :: Word64 -> Word64 -> Bool -> (Word64, Bool)
sumCarry a b carry = (total, newCarry)
  where preTotal  = a + b
        total     = if carry then preTotal + 1 else preTotal
        newCarry  = total < a || total < b || (carry && total < 1)

sumVector :: DVS.Vector Word64 -> DVS.Vector Word64 -> Word64 -> (Word64, DVS.Vector Word64)
sumVector u v carry = DVS.createT $ do
  w <- DVSM.new len
  go w 0 False
  return (undefined, w)
  where len = min (DVS.length u) (DVS.length v)
        go :: DVSM.MVector s Word64 -> Int -> Bool -> ST s Word64
        go w i c = if i < len
          then do
            let (t, nc) = sumCarry (DVS.unsafeIndex u i) (DVS.unsafeIndex v i) c
            DVSM.unsafeWrite w i t
            go w (i + 1) nc
          else return 1

sumBitVectors :: [DVS.Vector Word64] -> DVS.Vector Word64
sumBitVectors []       = DVS.empty
sumBitVectors [v]      = v
sumBitVectors (v:w:vs) = sumBitVectors (snd (sumVector v w 0):vs)
