module ConduitAccursed
  ( run
  ) where

import Prelude

import qualified Conduit                  as C
import qualified Data.ByteString          as B
import qualified Data.ByteString.Builder  as Bld
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Internal as BSI
import qualified Data.Conduit.List        as CL
import qualified Data.HashMap.Internal    as HMI
import qualified Data.HashMap.Strict      as HM
import qualified Data.Primitive.PVar      as PV
import qualified System.IO                as SIO

import Conduit               ((.|))
import Data.Bits             (setBit)
import Data.ByteString.Char8 (ByteString)
import Data.Function         (on)
import Data.List             (sortBy)


type Map = HM.HashMap ByteString (PV.PVar Int PV.RW)

test :: IO Map
test =
  C.runConduitRes
     $ C.stdinC
    .| CL.concatMap BC.words
    .| C.mapC (B.map (`setBit` 5))
    .| C.foldlC addToTable mempty

addToTable :: Map -> ByteString -> Map
addToTable !hm !bs = BSI.accursedUnutterablePerformIO $
  case HM.lookup bs hm of
    Nothing -> do
      ref <- PV.newPVar 1
      pure (HMI.insertNewKey (HMI.hash bs) bs ref hm)
    Just !ref -> do
      PV.modifyPVar_ ref (+ 1)
      pure hm
{-# inline addToTable #-}

run :: IO ()
run =
  SIO.hSetBuffering SIO.stdout (SIO.BlockBuffering Nothing)
    >> test >>= traverse PV.readPVar >>= printMap
  where
    printMap :: HM.HashMap ByteString Int -> IO ()
    printMap =
      Bld.hPutBuilder SIO.stdout
      . foldMap (\(bs, i) -> Bld.byteString bs <> " " <> Bld.intDec i)
      . sortBy (flip compare `on` snd)
      . HM.toList

