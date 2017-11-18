module KaleSpec where

import Test.Hspec

import Kale

spec :: Spec
spec = do
    describe "driver" $ do
        it "is empty on empty tasks" $
            driver [] `shouldBe` ""
        it "has command sum" $
            driver [Task "Foo.Bar" Nothing "Bar"]
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
            mkCommandSum [] `shouldBe` ""
        it "lists tasktnames properly" $
            mkCommandSum [task0, task1] 
                `shouldBe`
                    unwords ["data Command = Name|Other_Name deriving (Eq,",
                        "Show, Read, Generic, ParseRecord)"]

    describe "mkTask" $ do
        it "is Nothing for taskArgs when fileContent is empty" $
            mkTask "" "OtherName" "Module1.foo" `shouldBe` task1

    describe "pathToModule" $ do
        it "parses directories and file extensions" $
            pathToModule "foo/bar.baz" == "Bar" `shouldBe` True

    describe "importList" $ do
        it "pulls out taskModules and splits into newlines" $
            importList [task0, task1] `shouldBe`
                unlines [
                        "import qualified Module.fooTask",
                        "import qualified Module1.fooTask"
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

task0 :: Task
task0 = Task "Module.foo" (Just "args0 args1") "Name"

task1 :: Task
task1 = Task "Module1.foo" Nothing "Other_Name"
