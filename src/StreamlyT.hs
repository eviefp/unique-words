{-# OPTIONS_GHC -Wno-orphans #-}
module StreamlyT
  ( run
  ) where

import Prelude

import qualified Data.ByteString.Builder              as Bld
import qualified Data.ByteString.Char8                as BS
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Word8                           as W8
import qualified Foreign.Ptr                          as P
import qualified Streamly.FileSystem.Handle           as SFS
import qualified Streamly.Internal.Memory.Array.Types as Arr
import qualified Streamly.Internal.Memory.ArrayStream as SA
import qualified Streamly.Prelude                     as S
import qualified System.IO                            as SIO

import Data.Function                        (on, (&))
import Data.Hashable                        (Hashable (hashWithSalt),
                                             hashPtrWithSalt)
import Data.Int                             (Int32)
import Data.List                            (sortBy)
import Foreign                              (Storable)
import Foreign.ForeignPtr                   (withForeignPtr)
import GHC.IO                               (unsafePerformIO)
import Streamly.Internal.Memory.Array.Types (Array (Array))

type Map = HM.HashMap (Array W8.Word8) Int32

instance (Storable a, Hashable a) => Hashable (Array a) where
  hashWithSalt slt arr@(Array start _ _ ) =
    unsafePerformIO
      $ withForeignPtr start \ptr ->
          hashPtrWithSalt (P.castPtr ptr) (Arr.length arr) slt

test :: IO Map
test =
  S.foldr (HM.unionWith (+)) mempty
    $ SA.splitOn W8._space (S.unfold SFS.readChunks SIO.stdin)
    & S.map (`HM.singleton` 1)

run :: IO ()
run =
  SIO.hSetBuffering SIO.stdout (SIO.BlockBuffering Nothing)
    >> test >>= printMap
  where
    printMap :: Map -> IO ()
    printMap =
      Bld.hPutBuilder SIO.stdout
      . foldMap (\(bs, i) -> Bld.byteString (BS.pack $ show bs) <> " " <> Bld.byteString (BS.pack $ show i) <> "\n")
      . sortBy (flip compare `on` snd)
      . HM.toList
