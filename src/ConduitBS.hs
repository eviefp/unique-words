module ConduitBS
  ( run
  ) where

import Prelude

import qualified Conduit                 as C
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as Bld
import qualified Data.ByteString.Char8   as BC
import qualified Data.Conduit.List       as CL
import qualified Data.HashMap.Strict     as HM
import qualified Data.Word8              as W8
import qualified System.IO               as SIO

import Conduit               ((.|))
import Data.ByteString.Char8 (ByteString)
import Data.Function         (on)
import Data.Int              (Int32)
import Data.List             (sortBy)


type Map = HM.HashMap ByteString Int32

test :: IO Map
test =
  C.runConduitRes
     $ C.stdinC
    .| CL.concatMap BC.words
    .| C.mapC ((`HM.singleton` 1) . B.map W8.toLower)
    .| C.foldlC (HM.unionWith (+)) mempty

run :: IO ()
run =
  SIO.hSetBuffering SIO.stdout (SIO.BlockBuffering Nothing)
    >> test >>= printMap
  where
    printMap :: Map -> IO ()
    printMap =
      Bld.hPutBuilder SIO.stdout
      . foldMap (\(bs, i) -> Bld.byteString bs <> " " <> Bld.int32BE i)
      . sortBy (flip compare `on` snd)
      . HM.toList
