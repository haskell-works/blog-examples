{-# LANGUAGE DeriveGeneric #-}

module App.Commands.Types
  ( SumBitVectorsOptions(..)
  ) where

import Data.Semigroup ((<>))
import GHC.Generics

data SumBitVectorsOptions = SumBitVectorsOptions
  { filePaths   :: [FilePath]
  , branchiness :: String
  } deriving (Eq, Show, Generic)
