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

-- | The 'Task' datatype represents a user-define task.
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

-- | Generates the Haskell source code for a task module.
mkTaskModule :: FilePath -- ^ The path to the module
             -> [Task]   -- ^ The list of Tasks from which to generate module code.
             -> String
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

-- | Generates Haskell source code for a module "driver".
driver :: [Task] -- ^ The list of 'Task's from which to generate module code.
       -> String
driver [] = ""
driver tasks = unlines $
    [ "kaleMain :: IO ()"
    , "kaleMain = do"
    ] ++ indent 2
    [ "cmd <- getRecord \"kale-discovery\""
    , "case (cmd :: Command) of"
    ] ++ indent 4 (map mkCaseOf tasks)

-- | Generates Haskell source code for a command data type specific to this module.
mkCommandSum :: [Task] -- ^ The list of 'Task's from which to create commands.
             -> String
mkCommandSum [] = ""
mkCommandSum tasks = "data Command = "
    ++ intercalate "|" (map taskToSum tasks)
    ++ " deriving (Eq, Show, Read, Generic, ParseRecord)"

-- | Creates the String representation of a command sum type for the given 'Task'.
taskToSum :: Task -- ^ The 'Task'
          -> String
taskToSum = taskName

-- | Create the String represntation of the case expression for the given 'Task'.
mkCaseOf :: Task -- ^ The 'Task'
         -> String
mkCaseOf task = concat
    [taskName task, " -> ", taskModule task, "Task.task"]

-- | Indent the given Strings by the given number of spaces.
indent :: Int      -- ^ The number of spaces to indent.
       -> [String] -- ^ The Strings to indent.
       -> [String]
indent n = map (replicate n ' ' ++)

-- | Usage string.
usage :: String
usage = "Kale doesn't take any arguments."

-- | Returns the 'Task's found at the given FilePath.
findTasks :: FilePath  -- ^ The path at which to search for 'Task's.
          -> IO [Task]
findTasks src = do
  let (dir, file) = splitFileName src
  files <- filter (/= file) <$> getFilesRecursive dir
  catMaybes <$> traverse (fileToTask dir) files

-- | Creates a 'Task' from the file at a relative path in a given directory.
fileToTask :: FilePath      -- ^ A directory.
           -> FilePath      -- ^ A path relative to the above directory.
           -> IO (Maybe Task)
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
    stripSuffixes :: String -> Maybe String
    stripSuffixes x =
        stripSuffix "Task.hs" x <|> stripSuffix "Task.lhs" x
    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix suffix str =
        reverse <$> stripPrefix (reverse suffix) (reverse str)

-- | Creates a 'Task' from file contents and metadata.
mkTask :: String -- ^ Contents of a task file.
       -> String -- ^ The name of the 'Task'.
       -> String -- ^ The name of the task module.
       -> Task
mkTask fileContent name mod_ = Task
    { taskModule = mod_
    , taskArgs   = findArgs fileContent
    , taskName   = casify name
    }

-- | Convert a String in camel case to snake case.
casify :: String -> String
casify str = intercalate "_" $ groupBy (\a b -> isUpper a && isLower b) str

-- | Parse task module arguments from the task module file contents.
findArgs :: String   -- ^ Task module file contents.
         -> Maybe String
findArgs file =  Nothing

-- | Splits a string by declarations.
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

-- | Collapses multiple spaces to a single space.
collapseSpace :: String -> String
collapseSpace = unwords . words

-- | Returns True if the given string is a valid task module name.
-- See `Cabal.Distribution.ModuleName` (http://git.io/bj34)
isValidModuleName :: String -> Bool
isValidModuleName []     = False
isValidModuleName (c:cs) = isUpper c && all isValidModuleChar cs

-- | Returns True if the given Char is a valid taks module character.
isValidModuleChar :: Char -> Bool
isValidModuleChar c = isAlphaNum c || c == '_' || c == '\''

-- | Returns a list of relative paths to all files in the given directory.
getFilesRecursive :: FilePath      -- ^ The directory to search.
                  -> IO [FilePath]
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
