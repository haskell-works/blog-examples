{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Data.Aeson
import GHC.Stats
import System.Posix.Process
import System.Process

import qualified Data.ByteString.Lazy as BS
import qualified System.Environment   as IO

main :: IO ()
main = do
  pid <- getProcessID
  (filename:_) <- IO.getArgs
  bs <- BS.readFile filename
  let !maybeJson = decode bs :: Maybe Value

  system $ "ps aux | grep " <> show pid <> " | grep -v grep"

  forM_ maybeJson $ \_ ->
    putStrLn "Done"
