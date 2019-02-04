{-# LANGUAGE MagicHash #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.Word
import GHC.Int
import GHC.Prim
import GHC.Stats
import GHC.Word                       hiding (ltWord)
import HaskellWorks.Data.Bits.BitWise
import System.Posix.Process
import System.Process

import qualified Data.ByteString.Lazy         as BS
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM
import qualified System.Environment           as IO

ltWord :: Word64 -> Word64 -> Word64
ltWord (W64# a#) (W64# b#) = fromIntegral (I64# (ltWord# a# b#))
{-# INLINE ltWord #-}

sumCarry :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
sumCarry a b carry = (total, newCarry)
  where preTotal  = a + b
        total     = preTotal + carry
        newCarry  = total `ltWord` (a .|. b .|. carry)

sumVector :: DVS.Vector Word64 -> DVS.Vector Word64 -> Word64 -> (Word64, DVS.Vector Word64)
sumVector u v carry = DVS.createT $ do
  w <- DVSM.new len
  go w 0 0
  return (undefined, w)
  where len = min (DVS.length u) (DVS.length v)
        go :: DVSM.MVector s Word64 -> Int -> Word64 -> ST s Word64
        go w i c = if i < len
          then do
            let (t, nc) = sumCarry (DVS.unsafeIndex u i) (DVS.unsafeIndex v i) c
            DVSM.unsafeWrite w i t
            go w (i + 1) nc
          else return c

main :: IO ()
main = do
  return ()
