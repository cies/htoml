htoml
=====

[![Build Status](https://travis-ci.org/cies/htoml.svg?branch=master)](https://travis-ci.org/cies/htoml)
[![Latest version on Hackage](https://img.shields.io/hackage/v/htoml.svg)](https://hackage.haskell.org/package/html)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/htoml.svg)](https://hackage.haskell.org/package/html)

[![htoml on Stackage LTS 4](http://stackage.org/package/htoml/badge/lts-4)](http://stackage.org/lts-4/package/htoml)
[![htoml on Stackage LTS 5](http://stackage.org/package/htoml/badge/lts-5)](http://stackage.org/lts-5/package/htoml)
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
[TOML spec](https://github.com/mojombo/toml).
Compatibility between `htoml` version and TOML (as proven by
[BurntSushi's language agnostic TOML test suite]())
is as follows:

* [TOML v0.4.0](https://github.com/toml-lang/toml/releases/tag/v0.4.0)
is implemented by  `htoml >= 1.0.0.0`
* *(currently only one item in this mapping, more will follow)*


### Documentation

Apart from this README, documentation for this package may
(or may not) be found on [Hackage](https://hackage.haskell.org/package/htoml).


### Quick start

Installing `htoml` is easy. Either by using
[Stack](http://haskellstack.org) (recommended):

    stack install htoml

Or by using Cabal:

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

Add a `--resolver` flag to the `stack init` command to specify
a specific package snapshot, e.g.: `--resolver lts-4.1`.

In case you have missing dependencies (possibly `file-embed`),
they can be added to the `extra-deps` in `stack.yaml`
automatically with:

    stack solver --update-config

We can now start exploring `htoml` from a GHCi REPL. For instance
by reading a `.toml` file from the banchmarks, with:

```haskell
txt <- readFile "benchmarks/example.toml"
let r = parseTomlDoc "" txt
r
```

..,which prints:

    Right (fromList [("database",VTable (fromList [("enabled",VBoolean True),("po [...]

Then converting it to [Aeason's JSON](https://hackage.haskell.org/package/aeson), with:

```haskell
let Right toml = r
toJSON toml
```

..which prints:

    Object (fromList [("database",Object (fromList [("enabled",Bool True),("po [...]

Finally here an parse error produced with:

```haskell
let Left err = parseTomlDoc "" "== invalid toml =="
err
```

...which errors out showing:

    (line 1, column 1):
    unexpected '='
    expecting "#", "\n", "\r\n", letter or digit, "_", "-", "\"", "'", "[" or end of input

**Note:** Some of the above outputs are truncated, indicated by `[...]`.


### How to pull data from a TOML file after parsing it

Once you have sucessfully parsed a TOML file you most likely want to pull
some piecces of data out of the resulting data structure.

To do so you have two main options. The first is to use pattern matching.
For example let's consider the following `parseResult`:

```haskell
Right (fromList [("server",VTable (fromList [("enabled",VBoolean True)] ) )] )
```

Which could be pattern matched with:

```haskell
case parseResult of
  Left  _ -> "Could not parse file"
  Right m -> case m ! "server" of
    VTable mm -> case mm ! "enabled" of
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
exist; possible using the `lens` library as
[documented here](https://github.com/cies/htoml/issues/8).


### Compatibility

Currently we are testing against several versions of GHC with
[Travis CI](https://travis-ci.org/cies/htoml) as defined in the `env` section of our
[`.travis.yml`](https://github.com/cies/htoml/blob/master/.travis.yml).
`lts-2` implies GHC 7.8.4, `lts-3` implies GHC 7.10.2, `lts-4`/`lts-5`
imply GHC 7.10.3, and `nightly` is build with a regularly updated version of GHC.


### Version contraints of `htoml`'s dependencies

If you encounter any problems because `htoml`'s dependecies are
constrained either too much or too little, please
[file a issue](https://github.com/cies/htoml/issues) for that.
Or off even better submit a PR.


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
* Available on [Stackage](http://stackage.org) (see top of this README for badges
  indicating TOMLs *inclusion in Stackage status*)


### Todo

* More documentation and start to use the proper Haddock idioms
* Add property tests with QuickCheck (the internet says it's possible for parsers)
* Extensively test error cases (probably improving error reporting along the way)
* See how lenses may (or may not) fit into this package, or an additional package
* Consider moving to [one of the more modern parser combinators](https://www.reddit.com/r/haskell/comments/46u45o/what_is_the_current_state_of_parser_libraries_in)
  in Haskell (`megaparsec` maybe?) -- possibly wait until a clear winner shows

Do you see todo that looks like fun thing to implement and you can spare the time?
Please knoe that PRs are welcome :)


### Acknowledgements

Originally this project started off by improving the `toml` package by
Spiros Eliopoulos.

[HuwCampbell](https://github.com/HuwCampbell) helped a lot by making tests
pass and implementing "explicitness tracking" in Parsec's parser state.


### Copyright and licensing

This package includes BurntSushi's language agnostic
[TOML tests](https://github.com/BurntSushi/toml-test), which are WTFPL
licensed.

The TOML examples that are used as part of the benchmarks are copied
from Tom Preston-Werner's TOML spec which is MIT licensed.

For all other files in this project the copyrights are specified in the
`htoml.cabal` file, they are distributed under the BSD3 license as found
in the `LICENSE` file.
