toml
====

A [TOML](https://github.com/mojombo/toml) parser library in [Haskell](http://haskell-lang.org).

TOML is the obvious, minimal configuration language by [Tom Preston-Werner](https://github.com/mojombo).

The documentation for this package may (or may not) be found on [Hackage](https://hackage.haskell.org/package/toml).


### todo

* Add more documentation
* Add more tests (maybe find a more mature TOML parser and steal their tests)
* Add property tests with QuickCheck (the internet sais it's possible for parsers)
* Make all tests pass
* Investigate if the [language agnostic test suite](https://github.com/BurntSushi/toml-test) is interesting for this project
* Make error messages more useful (and add tests for that)
* See how lenses may (or may not) fit into this package
* See if we can learn more from how Aeson does things


### tada

* Haddock docs
* Complete the tests by covering the last two sections of the TOML spec (Tables)
* Breakingly change the output format to better represent the tree structure that Tables introduce
* Breakingly change the value types to be more consistent
* Breakingly change `TOML` to `Toml` all over the code
