module Megaparsec
  ( run
  ) where

import Prelude

import qualified Data.HashMap.Strict as HM
import qualified Text.Megaparsec     as MP

import qualified Data.Char    as C
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as LIO
import           Data.Void    (Void)


type Map = HM.HashMap Text Int

type Parser = MP.Parsec Void Text


parse :: Parser Map
parse = HM.fromListWith (+) <$> MP.many wordParser

wordParser :: Parser (Text, Int)
wordParser = do
  word <- MP.takeWhile1P Nothing (not . C.isSpace)
  _ <- MP.takeWhileP Nothing C.isSpace
  pure (T.toLower word, 1)

run :: IO ()
run =
  LIO.getContents
    >>= either mempty printMap <$> MP.runParser parse "input"
  where
    printMap :: Map -> IO ()
    printMap =
      LIO.putStr
      . HM.foldMapWithKey
        (\k _ ->
           mconcat
             [ k
             , " 1\n"
             ])
