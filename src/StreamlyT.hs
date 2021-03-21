module StreamlyT
  ( run
  ) where

import Prelude

import qualified Data.Text.IO     as TIO
import qualified Streamly.Prelude as S

test :: IO ()
test =
  S.drain
    $ S.repeatM TIO.getLine

run :: IO ()
run = pure ()
