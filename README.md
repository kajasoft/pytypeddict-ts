# pytypeddict-ts
### Python TypedDicts to TypeScript translator

Translate Python 3.11 TypedDict defintions into TypeScript type
definitions from the command line.

## Installation

The GHC Haskell compiler is required. To install a compatible version
of Haskell, we suggest you use [ghc-up][1]. 

[1]:https://www.haskell.org/ghcup

```
# install a recent haskell compiler
ghcup install ghc 9.2.5
ghcup install set ghc 9.2.5
cabal update
# install pytypeddict-ts in ~/.cabal/bin
cabal install
```


## Motivation

This project was inspired by `py-ts-interfaces`, which states the
motivation perfectly:

    In web applications where Python is used in the backend and
    TypeScript is used in the frontend, it is often the case that the
    client will make calls to the backend to request some data with some
    specific pre-defined "shape".  On the client-side, an `interface`
    for this data is usually defined and if the Python backend authors
    use typechecking, like with [mypy](http://mypy-lang.org/), the
    project authors may be typing the JSON response values as well.

    This results in a duplication of code.  If the shape changes in the
    backend, the related interface must also be reflect its changes in
    the frontend.  At best, this is annoying to maintain.  At worst,
    over time the interfaces may diverge and cause bugs.

    This library aims to have a single source of truth that describes
    the shape of the payload between the backend and the frontend.

We created `pytypeddict-ts` because we prefer lighter-weight Python
TypedDicts to define our types that cross the Python-TypeScript
boundary, whereas the authors of `py-ts-interfaces` opt for
Dataclasses. Thus `py-ts-interfaces` works for Dataclasses, whereas
`pytypeddict-ts` works for TypedDicts.


## Usage

Basic usage entails a Unix pipeline:

  cat types.py | pytypeddict-ts > types.ts




## License

MIT License
