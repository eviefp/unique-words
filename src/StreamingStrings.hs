module StreamingStrings
  ( run
  ) where


import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BS
import           Data.Function           (on)
import qualified Data.HashMap.Strict     as HM
import           Data.List               (sortBy)
import           Prelude
import qualified Streaming.Prelude       as S

type Map = HM.HashMap String Int

test :: IO Map
test =
  S.fold_ (HM.unionWith (+)) mempty id
    . S.map (`HM.singleton` 1)
    . (`S.for` (S.each . words))
    $ S.stdinLn

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
             [ B.string8 k
             , B.string8 " "
             , B.string8 $ show v
             , B.string8 "\n"
             ])
      . sortBy (flip compare `on` snd)
      . HM.toList
