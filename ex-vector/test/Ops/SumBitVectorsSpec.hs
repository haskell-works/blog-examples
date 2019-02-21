{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Ops.SumBitVectorsSpec (spec) where

import Control.Monad
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

import qualified Ops.SumBitVectors.Word8.Branchless as A

import qualified Ops.SumBitVectors.Word8.Branchy as B

import qualified Ops.SumBitVectors.Word8.Branchier as C

import qualified Ops.SumBitVectors.Word8.Branchiest as D

import qualified Data.Vector.Storable as DVS

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.ValueSpec" $ do
  it "stub" $ requireTest $ do
    aw :: Word32 <- forAll $ G.word32 R.constantBounded
    bw :: Word32 <- forAll $ G.word32 R.constantBounded
    cw <- forAll $ pure $ aw + bw

    let av :: DVS.Vector Word8 = DVS.fromList $ fromIntegral <$> [aw, aw .>. 8, aw .>. 16, aw .>. 24]
    let bv :: DVS.Vector Word8 = DVS.fromList $ fromIntegral <$> [bw, bw .>. 8, bw .>. 16, bw .>. 24]
    let cv :: DVS.Vector Word8 = DVS.fromList $ fromIntegral <$> [cw, cw .>. 8, cw .>. 16, cw .>. 24]

    A.sumVector av bv === cv
    B.sumVector av bv === cv
    C.sumVector av bv === cv
    D.sumVector av bv === cv
