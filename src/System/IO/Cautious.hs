{-# LANGUAGE CPP #-}
-- | It is recommended to write
--
-- import Prelude hiding (writeFile)
--
-- when importing this module.
module System.IO.Cautious
  ( writeFile
  , writeFileL
  , writeFileWithBackup
  , writeFileWithBackupL
  ) where

import Prelude hiding (writeFile)

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import System.Directory (canonicalizePath, renameFile)
import System.FilePath (splitFileName)
import System.IO (openTempFile)
#ifdef _POSIX
import System.Posix.ByteLevel (writeAllL)
import System.Posix.Fsync (fsync)
import System.Posix.IO (closeFd, handleToFd)
#else
import Data.ByteString.Lazy (hPut)
import System.IO (hClose)
#endif

writeFile :: FilePath -> String -> IO ()
writeFile = writeFileWithBackup $ return ()

writeFileL :: FilePath -> ByteString -> IO ()
writeFileL = writeFileWithBackupL $ return ()

-- | Backs up the old version of the file with "backup". "backup" must not fail if there is no
-- old version of the file.
writeFileWithBackup :: IO () -> FilePath -> String -> IO ()
writeFileWithBackup backup fp = writeFileWithBackupL backup fp . pack

-- | Backs up the old version of the file with "backup". "backup" must not fail if there is no
-- old version of the file.
writeFileWithBackupL :: IO () -> FilePath -> ByteString -> IO ()
writeFileWithBackupL backup fp bs = do
    cfp <- canonicalizePath fp
    (tempFP, handle) <- uncurry openTempFile $ splitFileName cfp
#ifdef _POSIX
    fd <- handleToFd handle
    writeAllL fd bs
    fsync fd
    closeFd fd
#else
    hPut handle bs
    hClose handle
#endif
    backup
    renameFile tempFP cfp
