toml
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


### contributions

Most welcome; by issues, discussions, pointers and pull-requests.
This is one of the first Haskell libraries I have written, any
feedback is much appreciated.


### todo

* Add more documentation
* Add more tests (maybe find a more mature TOML parser and steal their tests)
* Add property tests with QuickCheck (the internet sais it's possible for parsers)
* Investigate if the [language agnostic test suite](https://github.com/BurntSushi/toml-test) is interesting for this project
* Make error messages more useful (and add tests for that)
* See how lenses may (or may not) fit into this package
* See if we can learn more from how Aeson does things


### tada

* Parser fails on mix-type arrays (as per spec)
* Haddock docs
* Complete the tests by covering the last two sections of the TOML spec (Tables)
* Breakingly change the output format to better represent the tree structure that Tables introduce
* Breakingly change the value types to be more consistent
* Breakingly change `TOML` to `Toml` all over the code
* Implement table arrays
