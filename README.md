# kale

[![Build Status](https://travis-ci.org/parsonsmatt/kale.svg?branch=master)](https://travis-ci.org/parsonsmatt/kale)
[![Coverage Status](https://coveralls.io/repos/github/parsonsmatt/kale/badge.svg?branch=master)](https://coveralls.io/github/parsonsmatt/kale?branch=master)

A tool for creating command line interfaces.

## Beginner's Welcome!

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

## Usage

There are several steps that are required in order to take advantage of kale's features.

1. Create a project.

    You can do this manually or by using `stack` like so:

        $ stack new my-cool-task

    However you create your project these instructions assume you have a directory
    structure similar to that described
    [here](https://wiki.haskell.org/Structure_of_a_Haskell_project#Directory_Structure).

2. Add kale as a dependency.

    Add an entry for `kale` to the `extra-deps` section of your `stack.yaml`
    file:

	    extra-deps
          - git: git@github.com:parsonsmatt/kale.git
            commit: <current-commit-hash>

    Replace `<current-commit-hash>` with the most recent git hash of the `kale`
    project.

	Then, add `kale` to the `build-depends` section of your cabal file.

3. Enabling the pre-processor.

    `kale` utilizes the pre-processing step of the compiler to generate the
    necessary boilerplate. First, create the top-level module that will
    namespace all of the tasks you want to do. For this example, we'll create
    `src/Lib.hs`. This file will contain only a single line, the GHC pragma to
    use the kale-discover preprocessor:

	    {-# OPTIONS_GHC -F -pgmF kale-discover #-}

4. Adding Tasks

    `kale` searches for modules namespaced under the one you put the annotation
    in. So `src/Lib.hs` is going to look for modules with a name matching
    `Lib.*Task`. Creating a new task module will add a new command to the
    executable.

	The module name will be converted into kebab-case for the command
    line. Assuming an executable name of `tasks`, then `Lib.FooTask` will be
    invoked like `tasks foo`, and `Lib.RunJobTask` will be run as `tasks
    run-job`. You must define the actual task logic in a function `task :: IO
    ()`.

5. Adding Arguments

    By default, `kale` expects `task :: IO ()`. If you want to provide command
    line arguments, then you will need to define a data type called `Args` with
    a single constructor `Args`. If `Args` is a record, then it'll be parsed as
    keyword arguments. If `Args` is an ordinary product, then they are
    interpreted as positional arguments.

    The function task will then have the type `task :: Args -> IO ()`.

6. Call `kaleMain`.

    Create the file `src/Main.hs` if it does not already exist and ensure that
    it has only the following contents:

	    module Main where

        import Lib

        main :: IO ()
        main = Lib.kaleMain

Once that is complete you can build the project:

    $ stack build

Then, try out your new executable:

    $ ./stack-work/<path-to-binary> <zero-or-more-arguments>
