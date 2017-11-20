module Lib.ArgsTask where

data Args = Args { name :: String, age :: Int }
    deriving (Eq, Show, Read)

task :: Args -> IO ()
task args = do
    putStrLn $ "Your name is: " ++ name args
    print (age args)
