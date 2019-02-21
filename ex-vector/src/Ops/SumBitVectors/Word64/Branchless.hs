{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

module Ops.SumBitVectors.Word64.Branchless where

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

ltWord :: Word64 -> Word64 -> Word64
ltWord (W64# a#) (W64# b#) = fromIntegral (I64# (ltWord# a# b#))
{-# INLINE ltWord #-}

add :: Word64 -> Word64 -> (Word64, Word64)
add a b = (total, newCarry)
  where total     = a + b
        newCarry  = total `ltWord` (a .|. b)

addCarry :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
addCarry a b c = (t, carry0 + carry1)
  where (s, carry0) = add a b
        (t, carry1) = add s c

sumVector :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
sumVector u v = DVS.create $ do
  w <- DVSM.new len
  go w 0 0
  return w
  where len = min (DVS.length u) (DVS.length v)
        go :: DVSM.MVector s Word64 -> Int -> Word64 -> ST s Word64
        go w i c = if i < len
          then do
            let (t, nc) = addCarry (DVS.unsafeIndex u i) (DVS.unsafeIndex v i) c
            DVSM.unsafeWrite w i t
            go w (i + 1) nc
          else return c

sumBitVectors :: [DVS.Vector Word64] -> DVS.Vector Word64
sumBitVectors []       = DVS.empty
sumBitVectors [v]      = v
sumBitVectors (v:w:vs) = sumBitVectors (sumVector v w:vs)
