{-# LANGUAGE DeriveGeneric #-}

module App.Commands.Types
  ( SumBitVectorsOptions(..)
  ) where

import Data.Semigroup ((<>))
import GHC.Generics

newtype SumBitVectorsOptions = SumBitVectorsOptions
  { filePaths :: [FilePath]
  } deriving (Eq, Show, Generic)
