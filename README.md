# pytypeddict-ts
### Python TypedDict to TypeScript translator

Translate Python 3.10+ TypedDict definitions into TypeScript type
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

This project was inspired by [py-ts-interfaces][2], which states the
motivation perfectly:

[2]:https://github.com/cordero/py-to-ts-interfaces

> In web applications where Python is used in the backend and
> TypeScript is used in the frontend, it is often the case that the
> client will make calls to the backend to request some data with some
> specific pre-defined "shape".  On the client-side, an `interface`
> for this data is usually defined and if the Python backend authors
> use typechecking, like with [mypy](http://mypy-lang.org/), the
> project authors may be typing the JSON response values as well.
>
> This results in a duplication of code.  If the shape changes in the
> backend, the related interface must also be reflect its changes in
> the frontend.  At best, this is annoying to maintain.  At worst,
> over time the interfaces may diverge and cause bugs.
>
> This library aims to have a single source of truth that describes
> the shape of the payload between the backend and the frontend.

We created `pytypeddict-ts` to translate Python [TypedDict][typeddict]
definitions across the Python-TypeScript boundary.  `py-ts-interfaces`
translates [Data Classes][dataclass] to TypeScript, but not
TypedDicts.

[typeddict]:https://peps.python.org/pep-0589/
[dataclass]:https://docs.python.org/3/library/dataclasses.html

We're using `pytypeddict-ts` in production on the streaming search
engine [instantwatcher.com][3], which has a Django backend and
TypeScript frontend code for search.

[3]:https://www.instantwatcher.com

## Usage

Basic usage:

    cat types.py | pytypeddict-ts > types.ts

Example input `types.py`:

    TimeStamp = int

    ContentType = Literal['movie', 'series', 'season', 'episode']

    class StreamingService(TypedDict):
        link: str
        service_name: str 

    class Title(TypedDict):
        actors: list[str]
        content_type: ContentType
        genre_names: list[str]
        id: int
        image: str | None
        imdb_id: str
        imdb_rating: float | None
        imdb_vote_count: int | None
        overview: str
        runtime: NotRequired[int | None] # comment
        title: str
        year: int | None
        streaming_services: list[StreamingService]
        added_to_streaming: TimeStamp | None

Output `types.ts`:

    export type TimeStamp = number;

    export type ContentType = "movie" | "series" | "season" | "episode";

    export type StreamingService = {
      link: string;
      service_name: string;
    };

    export type Title = {
      actors: Array<string>;
      content_type: ContentType;
      genre_names: Array<string>;
      id: number;
      image: string | null;
      imdb_id: string;
      imdb_rating: number | null;
      imdb_vote_count: number | null;
      overview: string;
      runtime?: number | null;
      title: string;
      year: number | null;
      streaming_services: Array<StreamingService>;
      added_to_streaming: TimeStamp | null;
    };


`pytypeddict-ts` does not parse any more Python than it needs to
translate TypedDict definitions, so you must exclude extraneous 
code from the input. We use Unix tools like `sed` to accomplish this;
e.g.,

    #!/bin/bash
    {
      sed -n '/^TimeStamp/ p' types.py 
      sed -n '/^ContentType/ p' types.py 
      sed -n '/^class StreamingService/,/^ *$/ p' types.py 
      sed -n '/^class Title/,/^ *$/ p' types.py 
    } | pytypeddict-ts


## Supported Type Mappings

| Python                          | Typescript                    |
|:-------------------------------:|:-----------------------------:|
| None                            | null                          |
| str                             | string                        |
| int                             | number                        |
| float                           | number                        |
| bool                            | boolean                       |
| list[T]                         | Array[T]                      |
| tuple[T, U]                     | [T, U]                        |
| dict[T, U]                      | Record<T, U>                  |
| T \| \U                         | T \| U                        |
| Optional[T]                     | T \| null                     |
| Union[T, U, V]                  | T \| U \| V                   |
| Literal['foo', 'bar']           | "foo" \| "bar"                |
| myfield: NotRequired[T]         | myfield?: T                   |


We haven't implemented support for translations beyond these as they
suffice for our needs, but if you have a particular translation need,
open an issue. Depending on our bandwidth, we may try to implement it.
If you know Haskell, of course, you are welcome to contribute.


## References

* [py-ts-interfaces][cordero] The original py-ts-interfaces
* [py-ts-interfaces][syndallic] A fork extension of ps-ts-interfaces 

[cordero]:https://github.com/cs-cordero/py-ts-interfaces
[syndallic]:https://github.com/Syndallic/py-to-ts-interfaces

## License

MIT License
