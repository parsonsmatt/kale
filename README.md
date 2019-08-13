# kale

[![Build Status](https://travis-ci.org/parsonsmatt/kale.svg?branch=master)](https://travis-ci.org/parsonsmatt/kale)
[![Coverage Status](https://coveralls.io/repos/github/parsonsmatt/kale/badge.svg?branch=master)](https://coveralls.io/github/parsonsmatt/kale?branch=master)

A tool for creating command line interfaces, Kale automates the process by using the idea of a `Task`.

## An Example

A `Task` is an abstraction over the command-line arguments and the corresponding actions to be taken. These arguments are to be specified using the type, `Args`. Kale expects these Tasks to be named with the suffix 'Task' like 'FooTask.hs'. Kale auto-discovers them for you and generates the basic boiler plate like so:
If the user defines Tasks like "Lib.FooTask.hs" and "Lib.BarTask.hs",

```
module Lib.FooTask where

data Args = Args { i am command line arguments for the program }

task :: Args -> IO ()
task args = ...
```
Kale generates the following:

```
module Task where

import qualified Lib.FooTask as FooTask
import qualified Lib.BarTask as BarTask
-- ...

data Command = Foo { fooArgs ... } | Bar { barArgs ... } | ...
  deriving (Generic, ParseRecord)

main :: IO ()
main = do
  cmd <- getRecord "the program"
  case cmd of
    Foo  {..} -> FooTask.task FooTask.Args {..}
    Bar {..} -> BarTask.task BarTask.Args {..}
    ...
```

## Beginners Welcome!

This is a great project to contribute to if you'd like experience working on a real-world practical Haskell codebase.
There are a number of issues in the issue tracker, each of which should have enough information to get you started.
If you need more information or would like advice on implementing, feel free to post a comment and I'll be happy to help out.

## Developing

To hack on `kale`, you'll need the [`stack`](https://docs.haskellstack.org/en/stable/README/) build tool.
If you'd prefer to use another tool, please make an issue and file a pull request.

The `example` directory contains an example project.
This acts as both documentation and an integration test.
While hacking, you can run `make test` to run the unit tests and build the example project.

You can run `make lint` to run `hlint` over the project.

For a super fast reload and test runner, use `make ghcid`.
