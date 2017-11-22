module Main where

import           System.Environment        (getArgs)

import Kale

main :: IO ()
main = do
    kaleArgs <- getArgs
    case kaleArgs of
        src : _ : dest : _ -> do
            tasks <- findTasks src
            writeTaskModule dest (mkTaskModule src tasks)
        _ -> do
            putStrLn usage
            print kaleArgs
