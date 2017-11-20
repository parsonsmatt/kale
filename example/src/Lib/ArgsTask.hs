module Lib.ArgsTask where

data Args = Args { name :: String, age :: Int }

task :: Args -> IO ()
task args = do
    putStrLn $ "Your name is: " ++ name args
    print (age args)
