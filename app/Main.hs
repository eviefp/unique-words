{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main
  ( main,
  )
where

import ConduitBS qualified
import ConduitStream qualified
import Megaparsec qualified
import PipesText qualified
import Simple qualified
import StreamingBytestring qualified
import StreamingStrings qualified
import StreamlyT qualified
import Prelude

main :: IO ()
main = Simple.run
