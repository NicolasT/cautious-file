module Main where

import System.IO.Cautious (writeFile)

import Prelude hiding (writeFile)
import Control.Monad (replicateM, zipWithM_)
import System.Directory (removeFile)
import System.Environment (getArgs)
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

testCase :: Int -> FilePath -> IO ()
testCase dataLen outputFile = do
    testData <- replicateM dataLen $ randomRIO ('\0', '\255')
    writeFile outputFile testData
    testData' <- readFile outputFile
    True <- return $ testData == testData'
    removeFile outputFile

main :: IO ()
main = do
    args <- getArgs
    setStdGen . mkStdGen $ if null args then defaultSeed else read (head args)
    zipWithM_ testCase testLengths outputFilePaths
    putStrLn "All cautious-file tests completed successfully!"