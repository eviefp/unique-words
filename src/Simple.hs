{-# language BangPatterns      #-}
{-# language OverloadedStrings #-}
module Simple
  ( run
  ) where

import Prelude

-- pvar
import Data.Primitive.PVar

--bytestring
import qualified Data.ByteString          as ByteString
import           Data.ByteString.Char8    as Char8 (pack, unlines, words)
import           Data.ByteString.Internal (ByteString (..),
                                           accursedUnutterablePerformIO)

-- base
import Data.Bits
import Data.Foldable (foldl')
import Data.List     (sortOn)
import GHC.Word      (Word8)
import System.IO     (stdin)

--unordered-containers
import qualified Data.HashMap.Internal as Internal
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap

-- hashable
import Data.Hashable

type HashTable = HashMap ByteString (PVar Int RW)

run :: IO ()
run = do
  bs <- ByteString.hGetContents stdin
  let htM = buildTable bs
  ht <- traverse readPVar htM
  printTable ht


printTable :: HashMap ByteString Int -> IO ()
printTable !ht = do
  ByteString.putStr $
     Char8.unlines
   . fmap (\ ~(k, v) -> ByteString.concat [k, ": ", Char8.pack (show v)])
   . sortOn snd
   . HashMap.toList
   $ ht

buildTable :: ByteString -> HashTable
buildTable =
    foldl' addToTable HashMap.empty
  . fmap (ByteString.map toAsciiLower)
  . Char8.words

addToTable :: HashTable -> ByteString -> HashTable
addToTable !hm !bs = accursedUnutterablePerformIO $
  case HashMap.lookup bs hm of
    Nothing -> do
      ref <- newPVar 1
      pure (Internal.insertNewKey (fromIntegral (hash bs)) bs ref hm)
    Just !ref -> do
      modifyPVar_ ref (+ 1)
      pure hm

toAsciiLower :: Word8 -> Word8
toAsciiLower w = setBit w 5
