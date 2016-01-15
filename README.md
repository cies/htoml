htoml
=====

[![Build Status](https://travis-ci.org/cies/htoml.svg?branch=master)](https://travis-ci.org/cies/htoml)
[![Latest version on Hackage](https://img.shields.io/hackage/v/htoml.svg)](https://hackage.haskell.org/package/html)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/htoml.svg)](https://hackage.haskell.org/package/html)

[![htoml on Stackage LTS 2](http://stackage.org/package/htoml/badge/lts-2)](http://stackage.org/lts-2/package/htoml)
[![htoml on Stackage LTS 3](http://stackage.org/package/htoml/badge/lts-3)](http://stackage.org/lts-3/package/htoml)
[![htoml on Stackage Nightly](http://stackage.org/package/htoml/badge/nightly)](http://stackage.org/nightly/package/htoml)


A [TOML](https://github.com/mojombo/toml) parser library in
[Haskell](http://haskell-lang.org).

TOML is the obvious, minimal configuration language by
[Tom Preston-Werner](https://github.com/mojombo).
It is an alternative to the [XML](http://www.w3.org/TR/REC-xml/),
[YAML](http://www.yaml.org/spec/1.2/spec.html) and
[INI](http://en.wikipedia.org/wiki/INI_file) formats mainly for the purpose of
configuration files. Many will find that XML and YAML are too heavy for
the purpose of configuration files prupose while INI is underspecified.
TOML is to configuration files, like what Markdown is for rich-text.

This library aims to be compatible with the latest version of the
[TOML spec](https://github.com/mojombo/toml), currently that is
[v0.4.0](https://github.com/toml-lang/toml/releases/tag/v0.4.0).

The documentation for this package may (or may not) be found on
[Hackage](https://hackage.haskell.org/package/htoml).


### Quick start

Installing `htoml` is easy. Either by using
[Stack](http://haskellstack.org) (recommended):

    stack install htoml

Or using Cabal:

    cabal install htoml

In order to make your project depend on it you can add it as a
dependency in your project's `.cabal` file, and since it is not
yet on [Stackage](https://www.stackage.org/) you will also have
to add it to the `extra-deps` section of your `stack.yaml` file
when using Stack.

To quickly show some features of `htoml` we use Stack to start a
GHCi-based REPL. It picks up configuration from the `.ghci` file
in the root of the repository.

    git clone https://github.com/cies/htoml.git
    cd htoml
    stack init
    stack --install-ghc ghci

We can now start exploring `htoml` from a GHCi REPL.

    > txt <- readFile "benchmarks/example.toml"
    > let r = parseTomlDoc "" txt
    > r
    Right (fromList [("database",NTable (fromList [("enabled",NTValue (VBoolean True) [...]

    > let Right toml = r
    > toJSON toml
    Object (fromList [("database",Object (fromList [("enabled",Bool True) [...]

    > let Left error = parseTomlDoc "" "== invalid toml =="
    > e
    (line 1, column 1):
    unexpected '='
    expecting "#", "[" or end of input

Notice that some outputs are truncated, indicated by `[...]`.


### How to pull data from a TOML file after parsing it

Once you have sucessfully parsed a TOML file you most likely want to pull
some piecces of data out of the resulting data structure.

To do so you have two main options. The first is to use pattern matching.
For example let's consider the following `parseResult`:

```haskell
Right (fromList [("server",NTable (fromList [("enabled",NTValue (VBoolean True))] ) )] )
```

Which could be pattern matched with:

```haskell
case parseResult of
  Left  _ -> "Could not parse file"
  Right m -> case m ! "server" of
    NTable mm -> case mm ! "enabled" of
      VBoolean b -> "Server is " ++ (if b then "enabled" else "disabled")
      _ -> "Could not parse server status (Boolean)"
    _ -> "TOML file does not contain the 'server' key"
```

The second main option is to use the `toJSON` function to transform the data
to an [Aeson](https://hackage.haskell.org/package/aeson) data structure,
after which you can use your Aeson toolbelt to tackle the problem. Since
TOML is intended to be a close cousin of JSON this is a very practical
approach.

Other ways to pull data from a parsed TOML document will most likely
exist; maybe the `lens` library can give great results in some cases.
But I have no experience with them.


### Compatibility

Currently we are testing against several versions of GHC with
[Travis CI](https://travis-ci.org/cies/htoml) as defined in the `env` section of our
[`.travis.yml`](https://github.com/cies/htoml/blob/master/.travis.yml).
`lts-2` implies GHC 7.8.4, `lts-3` implies GHC 7.10.2, and `nightly` is
build with a regularly updated version of GHC.


### Version contraints of `htoml`'s dependencies

If you encounter any problems because `htoml`'s dependecies are
constrained either too much or too little, please
[file a issue](https://github.com/cies/htoml/issues) for that.

I will try to have `htoml` included in [Stackage](http://stackage.org)
as soon as it is reviewed by the community. Stackage provides a very
attractive solution to most (dependency) version conflicts.


### Tests and benchmarks

Tests are build and run with:

    stack test

[BurntSushi's language agnostic test suite](https://github.com/BurntSushi/toml-test)
is embedded in the test suite executable.  Using a shell script (that
lives in `test/BurntSushi`) the latest tests can be fetched from
its Github repository.

The benchmarks, that use the amazing [`criterion`](http://www.serpentine.com/criterion)
library, are build and run with:

    stack build :benchmarks


### Contributions

Most welcome! Please raise issues, start discussions, give comments or
submit pull-requests.
This is one of the first Haskell libraries I wrote, feedback is
much appreciated.


### Features

* Compatibility to the TOML spec is proven by an extensive test suite
* Incorporates [BurntSushi's language agnostic test suite](https://github.com/BurntSushi/toml-test)
* Has an internal representation that easily maps to JSON
* Provides an [Aeson](https://hackage.haskell.org/package/aeson)-style JSON interface (suggested by Greg Weber)
* Useful error messages (thanks to using Parsec over Attoparsec)
* Understands arrays as described in [this issue](https://github.com/toml-lang/toml/issues/254)
* Fails on mix-type arrays (as per spec)
* Comes with a benchmark suite to make performance gains/regressions measurable
* Tries to be well documented (please raise an issue if you find documentation lacking)


### Todo

* Release a stable 1.0 release and submit it to [Stackage](http://stackage.org)
* More documentation
* Make all tests pass (currently some more obscure corner cases don't pass)
* Add property tests with QuickCheck (the internet says it's possible for parsers)
* Extensively test error cases
* Try using `Vector` instead of `List` (measure performance increase with the benchmarks)
* See how lenses may (or may not) fit into this package, or an additional package


### Acknowledgements

Originally this project started off by improving the `toml` package by
Spiros Eliopoulos.


### Copyright and licensing

This package includes BurntSushi's language agnostic
[TOML tests](https://github.com/BurntSushi/toml-test), which are WTFPL
licensed.

The TOML examples that are used as part of the benchmarks are copied
from Tom Preston-Werner's TOML spec which is MIT licensed.

For all other files in this project the copyrights are specified in the
`htoml.cabal` file, they are distributed under the BSD3 license as found
in the `LICENSE` file.
