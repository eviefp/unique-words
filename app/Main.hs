{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main
  ( main
  ) where

import Prelude

import qualified Megaparsec
import qualified StreamingBytestring
import qualified StreamingStrings


main :: IO ()
main = StreamingBytestring.run
