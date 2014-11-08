#!/bin/sh

rm valid/*
rmdir valid
rm invalid/*
rmdir invalid

git clone https://github.com/BurntSushi/toml-test

mv toml-test/tests/valid .
mv toml-test/tests/invalid .

rm -rf toml-test
