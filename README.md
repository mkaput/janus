# Janus [![Build Status](https://travis-ci.com/mkaput/janus.svg?token=aVEpCFScCNWKbxmmuK83&branch=master)](https://travis-ci.com/mkaput/janus)

Simple scripting language written in Haskell, "ambitious" project for Functional Programming Course at AGH UST

## Installation

```shell
stack build
stack install
```

## Running Janus scripts

```shell
janus-run filename.js

# or using stack
stack exec janus-run -- examples/helloworld.js
```

## REPL

```shell
janus-repl

# or using stack
stack exec janus-repl
```

## Documentation

We have some basic [Language Specification](doc/Language-Specification.md) in `doc/` directory,
and most of functions are documented as Haddock comments. Generated documentation is hosted
[here](https://mkaput.github.io/janus/), and you can of course also run `stack haddock`.

## Running tests

```shell
stack test
```
