# Pang a Lambda!
This is the Haskell game Pang a Lambda implemented using the Functional
Reactive Programming library Yampa.

## Installation

You can install it with*:

```
$ git clone https://github.com/keera-studios/sage.git
$ git clone -b 'develop-games' https://github.com/ivanperez-keera/Yampa.git
$ git clone -b 'develop-sage' https://github.com/keera-studios/pang-a-lambda.git
$ cabal update
$ cabal sandbox init
$ cabal sandbox add-source sage/**/haskell/
$ cabal sandbox add-source sage/sage-**/haskell/
$ cabal sandbox add-source Yampa/
$ cabal sandbox add-source pang-a-lambda/
$ cabal install -fexpose-core Yampa/ pang-a-lambda/
$ ./.cabal-sandbox/bin/pang-a-lambda
```

*__Additional notes__:

The game is available on
[hackage](http://hackage.haskell.org/package/pang-a-lambda). However, you will
need the Yampa in branch _develop-games_ to install this game.

## Documentation

To try and make things as clear as possible, the code includes a much haddock
documentation and comments as we could reasonably fit. You can compile
those with:

```
$ git clone https://github.com/keera-studios/sage.git
$ git clone -b 'develop-games' https://github.com/ivanperez-keera/Yampa.git
$ git clone -b 'develop-sage' https://github.com/keera-studios/pang-a-lambda.git
$ cabal update
$ cabal sandbox init
$ cabal sandbox add-source sage/**/haskell/
$ cabal sandbox add-source sage/sage-**/haskell/
$ cabal sandbox add-source Yampa/
$ cabal sandbox add-source pang-a-lambda/
$ cabal install -fexpose-core Yampa/ pang-a-lambda/ --enable-documentation
```
