{-# LINE 1 "./example/src/Lib" #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Kale.Discover
import qualified Lib.BarTask
import qualified Lib.FooTask


data Command = Bar|Foo deriving (Eq, Show, Read, Generic, ParseRecord)

kaleMain :: IO ()
kaleMain = do
  cmd <- getRecord "kale-discovery"
  case (cmd :: Command) of
    Bar -> Lib.BarTask.task
    Foo -> Lib.FooTask.task

