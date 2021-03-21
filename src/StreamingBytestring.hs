module StreamingBytestring
  ( run
  ) where


import qualified Data.ByteString.Builder    as B
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.Char                  as C
import           Data.Function              (on)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (sortBy)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Prelude
import qualified Streaming.ByteString.Char8 as SB
import qualified Streaming.Prelude          as S

type Map = HM.HashMap ByteString Int

test :: IO Map
test =
  S.fold_ (HM.unionWith (+)) mempty id
    . S.map (`HM.singleton` 1)
    . S.mapped SB.toLazy
    . SB.words
    . SB.map C.toLower
    $ SB.stdin

run :: IO ()
run = do
  m <- test
  printMap m
  where
    printMap :: Map -> IO ()
    printMap =
      BS.putStr
      . B.toLazyByteString
      . foldMap (\(k, v) ->
           mconcat
             [ B.lazyByteString k
             , B.byteString " "
             , B.byteString . TE.encodeUtf8 . T.pack $ show v
             , B.byteString "\n"
             ])
      . sortBy (compare `on` snd)
      . HM.toList
