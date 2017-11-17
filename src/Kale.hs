{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Kale where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.String
import           Data.Traversable          (for)
import           System.Directory          (doesDirectoryExist, doesFileExist,
                                            getDirectoryContents)
import           System.Environment        (getArgs)
import           System.Exit
import           System.FilePath
import           System.IO

instance IsString ShowS where
  fromString = showString

data Task = Task
    { taskModule :: String
    -- ^ The name of the task module.
    , taskArgs   :: Maybe String
    -- ^ The arguments to provide to the task.
    , taskName   :: String
    -- ^ The name of the task.
    }
    deriving (Eq, Show)

-- | 'runKale' is the entry point of the Kale task discovery tool.
runKale :: IO ()
runKale = do
    kaleArgs <- getArgs
    case kaleArgs of
        src : _ : dest : _ -> do
            tasks <- findTasks src
            writeFile dest (mkTaskModule src tasks)
        _ -> do
            putStrLn usage
            print kaleArgs

mkTaskModule :: FilePath -> [Task] -> String
mkTaskModule src tasks = unlines
  [ "{-# LINE 1 " ++ show src ++ " #-}"
  , "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
  , "{-# LANGUAGE DeriveGeneric #-}"
  , "{-# LANGUAGE DeriveAnyClass #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , ""
  , "module " ++ pathToModule src ++ " where"
  , ""
  , "import Kale.Discover"
  , importList tasks
  , ""
  , mkCommandSum tasks
  , ""
  , driver tasks
  ]

driver :: [Task] -> String
driver [] = ""
driver tasks = unlines $
    [ "kaleMain :: IO ()"
    , "kaleMain = do"
    ] ++ indent 2
    [ "cmd <- getRecord \"kale-discovery\""
    , "case (cmd :: Command) of"
    ] ++ indent 4 (map mkCaseOf tasks)

mkCommandSum :: [Task] -> String
mkCommandSum [] = ""
mkCommandSum tasks = "data Command = "
    ++ intercalate "|" (map taskToSum tasks)
    ++ " deriving (Eq, Show, Read, Generic, ParseRecord)"

taskToSum :: Task -> String
taskToSum = taskName

mkCaseOf :: Task -> String
mkCaseOf task = concat
    [taskName task, " -> ", taskModule task, ".task"]

indent :: Int -> [String] -> [String]
indent n = map (replicate n ' ' ++)

usage :: String
usage = "Kale doesn't take any arguments."

findTasks :: FilePath -> IO [Task]
findTasks src = do
  let (dir, file) = splitFileName src
  files <- filter (/= file) <$> getFilesRecursive dir
  catMaybes <$> traverse (fileToTask dir) files

fileToTask :: FilePath -> FilePath -> IO (Maybe Task)
fileToTask dir file = runMaybeT $
    case reverse $ splitDirectories file of
        [] ->
            empty
        x:xs ->  do
            name <- MaybeT . pure . stripSuffixes $ x
            guard (isValidModuleName name && all isValidModuleName xs)
            let fileName = dir </> file
            moduleContents <- liftIO $ readFile fileName
            pure (mkTask moduleContents name (intercalate "." (reverse (name : xs))))
  where
    stripSuffixes x =
        stripSuffix "Task.hs" x <|> stripSuffix "Task.lhs" x
    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix suffix str = reverse <$> stripPrefix (reverse suffix) (reverse str)

mkTask :: String -> String -> String -> Task
mkTask fileContent name mod_ = Task
    { taskModule = mod_
    , taskArgs   = findArgs fileContent
    , taskName   = casify name
    }

casify :: String -> String
casify str = intercalate "_" $ groupBy (\a b -> isUpper a && isLower b) str


findArgs :: String -> Maybe String
findArgs file =  Nothing

decs :: String -> [String]
decs = reverse . fmap (collapseSpace . concat . reverse) . foldl' go [] . lines
  where
    go acc line@(c:_)
        | isSpace c || c == '}' =
            consFirst line acc
        | otherwise =
            [line] : acc
    go acc [] = acc
    consFirst a []     = [[a]]
    consFirst a (x:xs) = (a : x) : xs

collapseSpace :: String -> String
collapseSpace = unwords . words

-- See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
isValidModuleName :: String -> Bool
isValidModuleName []     = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive baseDir = sort <$> go []
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      c <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
      dirs <- filterM (doesDirectoryExist . (baseDir </>)) c >>= mapM go
      files <- filterM (doesFileExist . (baseDir </>)) c
      return (files ++ concat dirs)

-- | Derive module name from specified path.
pathToModule :: FilePath -> String
pathToModule f = toUpper m:ms
  where
    fileName = last $ splitDirectories f
    m:ms = takeWhile (/='.') fileName


-- | Generate imports for a list of specs.
importList :: [Task] -> String
importList = unlines . map f
  where
    f :: Task -> String
    f task = "import qualified " ++ taskModule task ++ "Task"
