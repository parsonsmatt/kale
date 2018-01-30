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

	Then, add `kale` to the `build-depends` section of you cabal file.

3. Create one or more tasks.

    In the `Lib` directory create one module for each task that you need.  At a
    minimum each task module must define a top-level definition named `task`.
    If your task will not accept any arguments, then it's type should be `IO
    ()`.  If your task takes does take arguments, the type should be `Args -> IO
    ()` and you must define the `Args` data type.  The value(s) of the `Args`
    data type are up to you.

4. Enable the pre-processor.

    `kale` utilizes the pre-processing step of the compilation process to generate
    the necessary boilerplate.  To enable the pre-processor create the file
    `src/Lib.hs` if it does not already exist and ensure that it has only the
    following contents:

	    {-# OPTIONS_GHC -F -pgmF kale-discover #-}

5. Call `kaleMain`.

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
