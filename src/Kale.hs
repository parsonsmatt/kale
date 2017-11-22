{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Kale where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Char                 (isAlphaNum, isLower, isSpace,
                                            isUpper, toUpper)
import           Data.List                 (foldl', groupBy, intercalate, sort,
                                            stripPrefix, isPrefixOf, find)
import           Data.Maybe                (catMaybes)
import           System.Directory          (doesDirectoryExist, doesFileExist,
                                            getDirectoryContents)
import           System.Environment        (getArgs)
import           System.FilePath

data TaskArgs = NoArgs | PositionalArgs String | RecordArgs String deriving (Eq, Show)
newtype TaskModule = TaskModule { unTaskModule :: String } deriving (Eq, Show)
newtype TaskName = TaskName { unTaskName :: String } deriving (Eq, Show)

-- | The 'Task' datatype represents a user-define task.
data Task = Task
    { taskModule :: TaskModule
    -- ^ The name of the task module.
    , taskArgs   :: TaskArgs
    -- ^ The arguments to provide to the task.
    , taskName   :: TaskName
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
            writeTaskModule dest (mkTaskModule src tasks)
        _ -> do
            putStrLn usage
            print kaleArgs

newtype TaskModuleContents = TaskModuleContents { unTaskModuleContents :: String }

-- | Write the given task module contents to the specified file.
writeTaskModule :: FilePath           -- ^ The file to write to.
                -> TaskModuleContents -- ^ Content of the task module.
                -> IO ()
writeTaskModule dest taskModuleContents = writeFile dest (unTaskModuleContents taskModuleContents)

-- | Generates the Haskell source code for a task module.
mkTaskModule :: FilePath           -- ^ The path to the module
             -> [Task]             -- ^ The list of Tasks from which to generate module code.
             -> TaskModuleContents
mkTaskModule src tasks = TaskModuleContents $ unlines
  [ "{-# LINE 1 " ++ show src ++ " #-}"
  , "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
  , "{-# LANGUAGE DeriveGeneric #-}"
  , "{-# LANGUAGE DeriveAnyClass #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE RecordWildCards #-}"
  , ""
  , "module " ++ pathToModule src ++ " where"
  , ""
  , "import Kale.Discover"
  , unImportList $ importList tasks
  , ""
  , unCommandSumType $ mkCommandSum tasks
  , ""
  , unDriver $ driver tasks
  ]

newtype Driver = Driver { unDriver :: String }

-- | Generates Haskell source code for a module "driver".
driver :: [Task] -- ^ The list of 'Task's from which to generate module code.
       -> Driver
driver [] = Driver ""
driver tasks = Driver $ unlines $
    [ "kaleMain :: IO ()"
    , "kaleMain = do"
    ] ++ indent 2
    [ "cmd <- getRecord \"kale-discovery\""
    , "case (cmd :: Command) of"
    ] ++ indent 4 (map mkCaseOf tasks)

newtype CommandSumType = CommandSumType { unCommandSumType :: String }

-- | Generates Haskell source code for a command data type specific to this module.
mkCommandSum :: [Task] -- ^ The list of 'Task's from which to create commands.
             -> CommandSumType
mkCommandSum [] = CommandSumType ""
mkCommandSum tasks = CommandSumType $ "data Command = "
    ++ intercalate " | " (map (unTaskName . taskToSum) tasks)
    ++ " deriving (Eq, Show, Read, Generic, ParseRecord)"

-- | Create a 'TaskName' from the given 'Task'.
taskToSum :: Task -- ^ The 'Task'
          -> TaskName
taskToSum task = TaskName $ (unTaskName . taskName $ task) ++ case taskArgs task of
    NoArgs -> ""
    PositionalArgs args -> stripArgs args
    RecordArgs args -> stripArgs args

-- | Strips all but the record fields from a data type.
--
-- >>> stripArgs "data Args = Args { foo :: Int }"
-- " { foo :: Int }"
stripArgs :: String -> String
stripArgs =
    (' ' :)
    . (++ "}")
    . dropWhile (/= '{')
    . takeWhile (/= '}')

-- | Create the String represntation of the case expression for the given 'Task'.
mkCaseOf :: Task -- ^ The 'Task'
         -> String
mkCaseOf task = concat
    [ unTaskName . taskName $ task
    , case taskArgs task of
        NoArgs -> ""
        _ -> "{..}"
    --, maybe "" (const "{..}") (taskArgs task)
    , " -> "
    , unTaskModule . taskModule $ task
    , "Task.task"
    , case taskArgs task of
        NoArgs ->
            ""
        _ -> concat
            [ " "
            , unTaskModule . taskModule $ task
            , "Task.Args {..}"
            ]
    ]

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
            moduleContents <- liftIO $ FileContent <$> readFile fileName
            pure (mkTask moduleContents (mkTaskName name) (TaskModule $ intercalate "." (reverse (name : xs))))
  where
    stripSuffixes :: String -> Maybe String
    stripSuffixes x =
        stripSuffix "Task.hs" x <|> stripSuffix "Task.lhs" x
    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix suffix str =
        reverse <$> stripPrefix (reverse suffix) (reverse str)

newtype FileContent = FileContent { unFileContent :: String }

-- | Creates a 'Task' from file contents and metadata.
mkTask :: FileContent -- ^ Contents of a task file.
       -> TaskName    -- ^ The name of the 'Task'.
       -> TaskModule  -- ^ The name of the task module.
       -> Task
mkTask fileContent name mod_ = Task
    { taskModule = mod_
    , taskArgs   = mkTaskArgs fileContent
    , taskName   = name
    }

-- | Convert a String in camel case to snake case.
casify :: String -> String
casify str = intercalate "_" $ groupBy (\a b -> isUpper a && isLower b) str

-- | Create 'TaskArgs' from the given task module contents.
mkTaskArgs :: FileContent -> TaskArgs
mkTaskArgs fileContent = case findArgs fileContent of
  Nothing -> NoArgs
  Just args ->
    if '{' `elem` args
    then PositionalArgs args
    else RecordArgs args

-- | Create a 'TaskName' from the given string.
mkTaskName :: String -> TaskName
mkTaskName = TaskName . casify

-- | Parse task module arguments from the task module file contents.
findArgs :: FileContent -- ^ Task module file contents.
         -> Maybe String
findArgs = find ("data Args" `isPrefixOf`) . decs . unFileContent

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

newtype ImportList = ImportList { unImportList :: String } deriving (Show)

-- | Generate imports for a list of specs.
importList :: [Task] -> ImportList
importList = ImportList . unlines . map f
  where
    f :: Task -> String
    f task = "import qualified " ++ unTaskModule (taskModule task) ++ "Task"
