module PipesText
  ( run
  ) where

import Prelude

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BS
import qualified Data.HashMap.Strict     as HM
import qualified Pipes.Prelude           as P
import qualified Pipes.Text              as PT
import qualified Pipes.Text.IO           as PTI

import           Control.Lens  (view)
import           Data.Function (on)
import           Data.List     (sortBy)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Pipes


type Map = HM.HashMap Text Int

test :: IO Map
test =
  P.fold (HM.unionWith (+)) mempty id
    $ PT.concats (view PT.words PTI.stdin)
    >-> P.map (`HM.singleton` 1)


run :: IO ()
run = test >>= printMap
  where
    printMap :: Map -> IO ()
    printMap =
      BS.putStr
      . B.toLazyByteString
      . foldMap (\(k, v) ->
           mconcat
             [ B.string8 $ T.unpack k
             , B.string8 " "
             , B.string8 $ show v
             , B.string8 "\n"
             ])
      . sortBy (flip compare `on` snd)
      . HM.toList
