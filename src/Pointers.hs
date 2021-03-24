module Pointers
  ( run
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Primitive.PVar as PV
import           Prelude

import qualified Control.Monad.Extra      as E
import           Data.Bool                (bool)
import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Internal as B
import           Data.Foldable            (foldl')
import qualified Data.HashMap.Internal    as HMI
import           Data.Hashable            (hash)
import           Data.Int                 (Int32)
import           Data.List                (sortOn)
import           System.IO                (isEOF)

type Map = HM.HashMap ByteString (PV.PVar Int32 PV.RW)

parseLine :: Map -> ByteString -> Map
parseLine im = foldl' go im . BS.words
  where
    go :: Map -> ByteString -> Map
    go !m !bs = B.accursedUnutterablePerformIO $
      case HM.lookup bs m of
        Nothing -> do
          ref <- PV.newPVar 1
          pure $ HMI.insertNewKey (fromIntegral $ hash bs) bs ref m
        Just !ref -> do
          PV.modifyPVar_ ref (+ 1)
          pure m
{-# INLINE parseLine #-}

run :: IO ()
run = do
  E.loopM go HM.empty >>= printTable
    where
    go :: Map -> IO (Either Map Map)
    go m = isEOF >>= bool (Left . parseLine m <$> BS.getLine) (pure $ Right m)
    {-# INLINE go #-}

printTable :: Map -> IO ()
printTable !ht = do
  traverse PV.readPVar ht >>=
    BS.putStr
    . BS.unlines
    . fmap (\ ~(k, v) -> BS.concat [k, ": ", BS.pack (show v)])
    . sortOn snd
    . HM.toList
{-# INLINE printTable #-}
