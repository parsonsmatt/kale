module Lib.PostTask where

data Args = Args Int String
    deriving Show

task :: Args -> IO ()
task = print
