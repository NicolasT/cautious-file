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
import qualified Prelude

import System.Directory (renameFile)
#ifdef _POSIX
import System.IO (hFlush, hPutStr)
import System.Posix.Files (ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.IO (closeFd, createFile, FdOption (SynchronousWrites), fdToHandle, setFdOption)

-- Don't bother to split into two writes if the string to write is shorter than this
splitLimit :: Int
splitLimit = 65536
#endif

writeFile :: FilePath -> String -> IO ()
writeFile = writeFileWithBackup (const $ return ())

-- | Backs up the old version of the file with "backup". "backup" must not fail if there is no
-- old version of the file.
writeFileWithBackup :: (FilePath -> IO ()) -> FilePath -> String -> IO ()
writeFileWithBackup backup fp text = do
     let tempFP = fp ++ ".tmp"
#ifdef _POSIX
     fd <- createFile tempFP $ unionFileModes ownerReadMode ownerWriteMode
     handle <- fdToHandle fd
     let writeSync = (setFdOption fd SynchronousWrites True >>) . hPutStr handle
     if length text < splitLimit
       then writeSync text
       else hPutStr handle (init text) >> writeSync [last text]
     hFlush handle
     closeFd fd
#else
     Prelude.writeFile tempFP text
#endif
     backup fp
     renameFile tempFP fp
