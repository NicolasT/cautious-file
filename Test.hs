{-# LANGUAGE CPP #-}
module Main where

import System.IO.Cautious (writeFile)

import Prelude hiding (writeFile)
import Control.Monad (replicateM, zipWithM_, unless)
import System.Directory (removeFile)
import System.Environment (getArgs)
#ifdef _POSIX
import System.Posix.Files (createSymbolicLink, readSymbolicLink)
#endif
import System.Random (mkStdGen, randomRIO, setStdGen)

-- Remember that because we are using zipWithM_ below, the lengths of testLengths and outputFilePaths need to be the same!
testLengths :: [Int]
testLengths = [ 0      -- Extreme value
              , 1      -- Extreme value
              , 65530  -- Normal value below split limit
              , 65536  -- Split limit
              , 128000 -- Normal value above split limit
              ]

outputFilePaths :: [FilePath]
outputFilePaths = ["foo", "foo.txt", "./bar.txt", ".dotfile", "./.dot.dot"]

defaultSeed :: Int
defaultSeed = 1234

stdTestCase :: Int -> FilePath -> IO ()
stdTestCase dataLen outputFile = do
    testData <- replicateM dataLen $ randomRIO ('\0', '\127')
    writeFile outputFile testData
    testData' <- readFile outputFile
    unless (testData == testData') . fail $ "stdTestCase: failed with " ++ show dataLen ++ " " ++ show outputFile
    removeFile outputFile

#ifdef _POSIX
symlinkTestCase :: IO ()
symlinkTestCase = do
    let linkTarget = "link-dest"
        linkSrc    = "link"
        testData   = "hi"
    writeFile linkTarget ""
    createSymbolicLink linkTarget linkSrc
    writeFile linkSrc testData
    newTarget <- readSymbolicLink linkSrc
    testData' <- readFile linkTarget
    unless (testData == testData') $ fail "symlinkTestCase failed: data read back not the same as data written out!"
    unless (newTarget == linkTarget) $ fail "symlinkTestCase failed: symlink clobbered"
    mapM_ removeFile [linkSrc, linkTarget]
#endif

main :: IO ()
main = do
    args <- getArgs
    setStdGen . mkStdGen $ if null args then defaultSeed else read (head args)
    zipWithM_ stdTestCase testLengths outputFilePaths
#ifdef _POSIX
    symlinkTestCase
#else
    putStrLn "Warning: POSIX tests not run! If you are on a POSIX platform, then to run them type 'runhaskell -D_POSIX Test.hs'"
#endif
    putStrLn "All cautious-file tests completed successfully!"