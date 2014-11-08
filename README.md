Toml
====

A [TOML](https://github.com/mojombo/toml) parser library in [Haskell](http://haskell-lang.org).

TOML is the obvious, minimal configuration language by
[Tom Preston-Werner](https://github.com/mojombo).
It is an alternative to the [XML](http://www.w3.org/TR/REC-xml/),
[YAML](http://www.yaml.org/spec/1.2/spec.html) and
[INI](http://en.wikipedia.org/wiki/INI_file) formats for the purpose of
configuration files, as the first two are too heavy for that prupose,
and the latter is underspecified.
Toml is to configuration files, like what Markdown is for rich-text.

This library is compatible with
[v0.2.0](https://github.com/toml-lang/toml/releases/tag/v0.2.0)
of the TOML spec..

The documentation for this package may (or may not) be found on [Hackage](https://hackage.haskell.org/package/toml).


### Tests and benchmarks

The test suite is build by default, `cabal configure --disable-tests` disables them.
The benchmark suite is not run by default, `cabal configure --enable-benchmarks` enables them.

With `cabal build` both of these suites are build as executables and
put somewhere in `dist/`. Passing `--help` to them will reveal their
options.

[BurntSushi's language agnostic test suite](https://github.com/BurntSushi/toml-test)
is embedded in the test suite executable.  A shell-script lives in `test/BurntSushi`
by which the latest tests can be fetched from BurntSushi's repository.


### Contributions

Most welcome; by issues, discussions, pointers and pull-requests.
This is one of the first Haskell libraries I have written, any
feedback is much appreciated.


### Todo

* Add more documentation
* Add more tests (maybe find a more mature TOML parser and steal their tests)
* Add tests for errors
* Add property tests with QuickCheck (the internet sais it's possible for parsers)
* See how lenses may (or may not) fit into this package


### Tada

* Add a JSON interface to it (as Greg suggested on Reddit)
* Make error messages more useful
* Understand arrays as in [this issue](https://github.com/toml-lang/toml/issues/254)
* Use Parsec instead of Attoparsec (find Attoparsec version in branch)
* Small benchmark suite
* Parser fails on mix-type arrays (as per spec)
* Haddock docs
* Have an internal representation that easily maps to JSON (will not completely, but more like it now)
* Incorporate [BurntSushi's language agnostic test suite](https://github.com/BurntSushi/toml-test)
* Complete the tests by covering the last two sections of the TOML spec (Tables)
* Breakingly change the output format to better represent the tree structure that Tables introduce
* Breakingly change the value types to be more consistent
* Breakingly change `TOML` to `Toml` all over the code
* Implement table arrays
