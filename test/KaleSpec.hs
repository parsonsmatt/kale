module KaleSpec where

import Test.Hspec

import Kale

spec :: Spec
spec = do
    describe "driver" $ do
        it "is empty on empty tasks" $
            (unDriver $ driver []) `shouldBe` ""
        it "has command sum" $
            (unDriver $ driver [Task (TaskModule "Foo.BarTask") Nothing (TaskName "Bar")])
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
