# Pang a Lambda!
This is the Haskell game Pang a Lambda implemented using the Functional
Reactive Programming library Yampa.

## Installation

You can install it with*:

```
$ git clone https://github.com/keera-studios/pang-a-lambda.git
$ cd pang-a-lambda*
$ cabal update
$ cabal sandbox init
$ cabal install Yampa-0.10.4/
$ cabal install pang-a-lambda.cabal
$ ./.cabal-sandbox/bin/pang-a-lambda
```

*__Additional notes__:

The game is available on [hackage](http://hackage.haskell.org/package/pang-a-lambda), however, you will need the Yampa included in this repository to install the game.

## Documentation

To try and make things as clear as possible, the code includes a much haddock
documentation and comments as we could reasonably fit. You can compile
those with:

```
$ git clone https://github.com/keera-studios/pang-a-lambda.git
$ cd pang-a-lambda-*
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure && cabal haddock --executables --internal
```
