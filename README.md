htoml
=====

A [TOML](https://github.com/mojombo/toml) parser library in
[Haskell](http://haskell-lang.org).

TOML is the obvious, minimal configuration language by
[Tom Preston-Werner](https://github.com/mojombo).
It is an alternative to the [XML](http://www.w3.org/TR/REC-xml/),
[YAML](http://www.yaml.org/spec/1.2/spec.html) and
[INI](http://en.wikipedia.org/wiki/INI_file) formats for the purpose of
configuration files, as the first two are too heavy for that prupose,
and the latter is underspecified.
Toml is to configuration files, like what Markdown is for rich-text.

This library aims to be compatible with the latest version of the
[TOML spec](https://github.com/mojombo/toml), currently that is
[v0.3.1](https://github.com/toml-lang/toml/releases/tag/v0.3.1).

The documentation for this package may (or may not) be found on
[Hackage](https://hackage.haskell.org/package/htoml).


### Quick start

To quickly show some features of `htoml` we use `GHCi`, from the
root of the repository run:

    cabal repl

It picks up configuration from the `.ghci` file, so we can immediately
start exploring:

    > txt <- readFile "benchmarks/example.toml"
    > let r = parseTomlDoc "" txt
    > r
    Right (fromList [("database",NTable (fromList [("enabled",NTValue (VBoolean True)), [...]

    > let Right toml = r
    > toJSON toml
    Object (fromList [("database",Object (fromList [("enabled",Bool True),("ports",Array [...]

    > let Left error = parseTomlDoc "" "== invalid toml =="
    > error
    (line 1, column 1):
    unexpected '='
    expecting "#", "[" or end of input

First notice that some outputs are truncated.


### Tests and benchmarks

The test suite is build by default, `cabal configure --disable-tests` disables them.
The benchmark suite is not run by default, `cabal configure --enable-benchmarks` enables them.

With `cabal build` both of these suites are build as executables and
put somewhere in `dist/`. Passing `--help` to them will reveal their
options.

[BurntSushi's language agnostic test suite](https://github.com/BurntSushi/toml-test)
is embedded in the test suite executable.  Using a shell script (that
lives in `test/BurntSushi`) the latest tests can be fetched from
BurntSushi's repository.


### Contributions

Most welcome! Please raise issues, start discussions, give comments or
submit pull-requests.
This is one of the first Haskell libraries I wrote, any feedback is
much appreciated.


### Features

* Follows the latest version of the TOML spec, proven by an extensive test suite
* Incorporates [BurntSushi's language agnostic test suite](https://github.com/BurntSushi/toml-test)
* Has an internal representation that easily maps to JSON
* Provides a JSON interface (suggested by Greg Weber)
* Useful error messages (thanks to using Parsec over Attoparsec)
* Understands arrays as described in [this issue](https://github.com/toml-lang/toml/issues/254)
* Provides a benchmark suite
* Fails on mix-type arrays (as per spec)
* Haddock documentation


### Todo

* Release a stable 1.0 release and submit it to [Stackage](http://stackage.org)
* More documentation
* Moke all tests pass (currently some more obscure corner cases don't pass)
* Add more tests (maybe find a more mature TOML parser and steal their tests)
* Add property tests with QuickCheck (the internet says it's possible for parsers)
* Extensively test error cases
* Try using Vector instead of List (measure performance increase with the benchmarks)
* See how lenses may (or may not) fit into this package


### Acknoledgements

Originally this project started off by improving the `toml` package by
Spiros Eliopoulos.


### License

BSD3 as found in the `LICENSE` file.
