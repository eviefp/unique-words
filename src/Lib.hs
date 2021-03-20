{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# language PartialTypeSignatures #-}
module Lib
  where


import qualified Data.ByteString.Builder    as B
import           Data.ByteString.Lazy
    (ByteString)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.HashMap.Strict        as HM
import           Prelude
import qualified Streaming.ByteString.Char8 as SB
import qualified Streaming.Prelude          as S

type Map = HM.HashMap ByteString Int

test :: IO Map
test =
  S.fold_ (HM.unionWith (+)) mempty id
    . S.map go
    . S.mapped SB.toLazy
    $ SB.words SB.stdin

{-# INLINE go #-}
go :: ByteString -> HM.HashMap ByteString Int
go bs = HM.singleton bs 1


{-# INLINE gogo #-}
gogo :: S.Stream (S.Of ByteString) IO x -> IO (S.Of Map x)
gogo = S.fold (\x s -> (s, 1):x) [] (HM.fromListWith (+))


run :: IO ()
run = do
  m <- test
  printMap m
  where
    printMap :: Map -> IO ()
    printMap =
      BS.putStr
      . B.toLazyByteString
      . HM.foldMapWithKey
        (\k _ ->
           mconcat
             [ B.lazyByteString k
             , B.byteString " "
             , B.byteString "1"
             , B.byteString "\n"
             ])
