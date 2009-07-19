{-# LANGUAGE CPP #-}
-- | It is recommended to write
--
-- import Prelude hiding (writeFile)
--
-- when importing this module.
module System.IO.Cautious
  ( writeFile
  , writeFileWithBackup
  ) where

import Prelude hiding (writeFile)

import System.Directory (renameFile)
import System.FilePath (splitFileName)
import System.IO (openTempFile)
#ifdef _POSIX
import Control.Monad (when)
import Data.Function (fix)
import Data.List (genericDrop)
import System.Posix.IO (closeFd, FdOption (SynchronousWrites), fdWrite, handleToFd, setFdOption)
import System.Posix.Types (Fd)

-- | Don't bother to split into two writes if the string to write is shorter than this
splitLimit :: Int
splitLimit = 65536

-- | Write the entire contents of a string to a file descriptor. Assumes blocking mode.
writeAll :: Fd -> String -> IO ()
writeAll fd = fix $ \me s -> when (not $ null s) $ do
    count <- fdWrite fd s
    me $ genericDrop count s
#else
import System.IO (hPutStr, hClose)
#endif

writeFile :: FilePath -> String -> IO ()
writeFile = writeFileWithBackup $ return ()

-- | Backs up the old version of the file with "backup". "backup" must not fail if there is no
-- old version of the file.
writeFileWithBackup :: IO () -> FilePath -> String -> IO ()
writeFileWithBackup backup fp text = do
    (tempFP, handle) <- uncurry openTempFile $ splitFileName fp
#ifdef _POSIX
    fd <- handleToFd handle
    let writeSync = (setFdOption fd SynchronousWrites True >>) . writeAll fd
    if null $ drop splitLimit text
      then writeSync text
      else writeAll fd (init text) >> writeSync [last text]
    closeFd fd
#else
    hPutStr handle text
    hClose handle
#endif
    backup
    renameFile tempFP fp
