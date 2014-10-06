toml
====

A [TOML](toml) parser library in [Haskell](hs).

TOML is the obvious, minimal configuration language by [Tom Preston-Werner](guy).

The documentation for this package may (or may not) be found on [Hackage](docs).


### todo

* Add documentation
* Complete the tests by covering the last two sections of the TOML spec (Tables)
* Breakingly change the output format to better represent the tree structure that Tables introduce
* Breakingly change the value types to be more consistent
* Breakingly change `TOML` to `Toml` all over the code
* Add more tests (maybe find a more mature TOML parser and steal their tests)
* Add property tests with QuickCheck (the internet sais it's possible for parsers)
* Make all tests pass
* Investigate if the [language agnostic test suite](agno) is interesting for this project
* Make error messages more useful (and add tests for that)
* See how lenses may (or may not) fit into this package
* See if we can learn more from how Aeson does things


[toml]: https://github.com/mojombo/toml
[hs]:   http://haskell-lang.org
[guy]:  https://github.com/mojombo
[docs]: https://hackage.haskell.org/package/toml
[agno]: https://github.com/BurntSushi/toml-test
