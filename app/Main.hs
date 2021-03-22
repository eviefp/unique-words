{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main
  ( main
  ) where

import Prelude

import qualified ConduitBS
import qualified ConduitStream
import qualified Megaparsec
import qualified PipesText
import qualified StreamingBytestring
import qualified StreamingStrings
import qualified StreamlyT

main :: IO ()
main = StreamlyT.run
