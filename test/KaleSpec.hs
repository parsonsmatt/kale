module KaleSpec where

import Test.Hspec

import Kale

spec :: Spec
spec = do
    describe "driver" $ do
        it "is empty on empty tasks" $
            unDriver (driver []) `shouldBe` ""
        it "has command sum" $
            unDriver (driver [Task (TaskModule "Foo.Bar") Nothing (TaskName "Bar")])
                `shouldBe` unlines
                    [ "kaleMain :: IO ()"
                    , "kaleMain = do"
                    , "  cmd <- getRecord \"kale-discovery\""
                    , "  case (cmd :: Command) of"
                    , "    Bar -> Foo.BarTask.task"
                    ]

    describe "casify" $ do
        it "simple case works" $
            casify "HelloWorld" `shouldBe` "Hello_World"
        it "acts strange with many caps letters" $
            casify "HTTPWorker" `shouldBe` "H_T_T_P_Worker"
        -- the inputs to casify will always be module names, so the below should
        -- not happen
        it "acts strange with initial lowercase" $
            casify "lowHigh" `shouldBe` "l_o_w_High"

    describe "decs" $ do
        it "roughly parses declarations" $ do
            decs decs1
                `shouldBe`
                    [ "data Foo = Bar"
                    ]
            decs decs0
                `shouldBe`
                    [ "module Foo where"
                    , "import Asdf"
                    , "data Foo = Bar | Baz"
                    ]
        it "can parse record syntax" $
            decs decs2
                `shouldBe`
                    [ "module Wat where"
                    , "data Foo = Foo { fooName :: String , fooAge :: Int }"
                    ]

        it "can parse weird records" $
            decs decs3
                `shouldBe`
                    [ "data Foo = Foo { fooName :: String}"
                    ]

    describe "mkCommandSum" $ do
        it "is empty on empty task lists" $
            unCommandSumType (mkCommandSum []) `shouldBe` ""
        it "lists tasktnames properly" $
            unCommandSumType (mkCommandSum [task0, task1])
                `shouldBe` concat
                    [ "data Command = Name { foo :: Int }"
                    , " | Other_Name deriving (Eq, "
                    , "Show, Read, Generic, ParseRecord)"
                    ]

    describe "mkCaseOf" $ do
        it "works with args" $ do
            mkCaseOf task0
                `shouldBe`
                    "Name{..} -> Module.FooTask.task Module.FooTask.Args {..}"
        it "works without args" $ do
            mkCaseOf task1
                `shouldBe`
                    "Other_Name -> Module1.FooTask.task"

    describe "mkTask" $ do
        it "is Nothing for taskArgs when fileContent is empty" $
            mkTask "" "OtherName" "Module1.Foo" `shouldBe` task1

    describe "pathToModule" $ do
        it "parses directories and file extensions" $
            pathToModule "foo/bar.baz" == "Bar" `shouldBe` True

    describe "importList" $ do
        it "pulls out taskModules and splits into newlines" $
            unImportList (importList [task0, task1]) `shouldBe`
                unlines [
                        "import qualified Module.FooTask",
                        "import qualified Module1.FooTask"
                        ]

    describe "findArgs" $ do
        it "correctly finds arguments" $ do
            findArgs args0
                `shouldBe`
                    Just "data Args = Args { fooId :: Int , barId :: Int }"

args0 :: String
args0 = unlines
    [ "module ASdf where"
    , ""
    , "data Args"
    , "  = Args"
    , "  { fooId :: Int"
    , "  , barId :: Int"
    , "  }"
    ]

decs0 :: String
decs0 = unlines
    [ "module Foo where"
    , ""
    , "import Asdf"
    , ""
    , "data Foo"
    , "  = Bar"
    , "  | Baz"
    ]

decs1 :: String
decs1 = unlines
    [ "data Foo"
    , " = Bar"
    ]

decs2 :: String
decs2 = unlines
    [ "module Wat where"
    , "data Foo = Foo"
    , "  { fooName :: String"
    , "  , fooAge  :: Int"
    , "  }"
    ]

decs3 :: String
decs3 = unlines
    [ "data Foo = Foo {"
    , " fooName :: String"
    , "}"
    ]

decs4 :: String
decs4 = unlines
    [ "module Wat where"
    , "data Args = Args"
    , "  { fooName :: String"
    , "  , fooAge  :: Int"
    , "  }"
    ]


task0 :: Task
task0 = Task (TaskModule "Module.Foo") (TaskArgs <$> Just "data Args = Args { foo :: Int }") (TaskName "Name")

task1 :: Task
task1 = Task (TaskModule "Module1.Foo") (TaskArgs <$> Nothing) (TaskName "Other_Name")
