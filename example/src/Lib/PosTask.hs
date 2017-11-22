module Lib.PosTask where

data Args = Args Int String

task :: Args -> IO ()
task (Args i s) = do
    print i
    putStrLn s
