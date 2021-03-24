{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main
  ( main,
  )
where

import Prelude

import ConduitBS qualified
import ConduitAccursed qualified
import ConduitStream qualified
import Megaparsec qualified
import PipesText qualified
import Boarders qualified
import StreamingBytestring qualified
import StreamingStrings qualified
import StreamlyT qualified
import Pointers qualified

main :: IO ()
main = Pointers.run
