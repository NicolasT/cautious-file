{-# LANGUAGE ForeignFunctionInterface #-}
module System.Posix.ByteLevel (fdWrite, fdWriteB, writeAllB, writeAllL) where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import qualified Data.ByteString as Strict
import Data.ByteString.Lazy (ByteString, toChunks)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Function (fix)
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.C.String (CString, CStringLen)
import Foreign.C.Types (CInt, CSize)
import System.Posix.Types (ByteCount, Fd(..))

#include <unistd.h>

foreign import ccall "write"
       c_write :: CInt -> CString -> CSize -> IO CSize

fdWrite :: Fd -> CStringLen -> IO ByteCount
fdWrite (Fd fd) (cs, l) =  throwErrnoIfMinus1Retry "write" . c_write fd cs $ fromIntegral l

fdWriteB :: Fd -> Strict.ByteString -> IO Int
fdWriteB fd bs = fromIntegral <$> unsafeUseAsCStringLen bs (fdWrite fd)

-- | Write the entire contents of the strict bytestring. Assumes blocking mode.
writeAllB :: Fd -> Strict.ByteString -> IO ()
writeAllB fd = fix $ \me s -> unless (Strict.null s) $ do
    count <- fdWriteB fd s
    me $ Strict.drop (fromIntegral count) s

-- | Write the entire contents of the lazy bytestring. Assumes blocking mode.
writeAllL :: Fd -> ByteString -> IO ()
writeAllL fd = mapM_ (writeAllB fd) . toChunks