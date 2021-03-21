module ConduitStream
  ( run
  ) where

import Prelude

import qualified Conduit             as C
import qualified Data.Conduit.List   as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

import Conduit       ((.|))
import Data.Char     (isPunctuation, toLower)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List     (sortBy)
import Data.Text     (Text)


type Map = HM.HashMap Text Int

test :: IO Map
test =
  C.runConduitRes
     $ C.stdinC
    .| C.decodeUtf8C
    -- .| C.filterCE (not . isPunctuation)
    -- .| C.omapCE toLower
    .| CL.concatMap T.words
    .| C.mapC (T.toLower . T.filter (not . isPunctuation))
    .| C.foldlC (\hm k -> HM.insertWith (+) k 1 hm) mempty

run :: IO ()
run = test >>= printMap
  where
    printMap :: Map -> IO ()
    printMap =
      traverse_ (\(w, f) -> TIO.putStrLn $ w <> " " <> T.pack (show f))
      . sortBy (flip compare `on` snd)
      . HM.toList
