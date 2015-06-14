
# elm-transducers 

A [transducer](http://clojure.org/transducers) is a composable way of processing a series of values.  Many basic transducers correspond to functions you may be familiar with for processing `List`s or `Signal`s.

## Transducers enable efficient processing

Transducers can be used to combine processing operations in a way that allows processing to be done more efficiently.

When using `List.map`, it is more efficient to compose multiple functions and then map the list with the composed function than to map the list with each function independently because the list will only be traversed once.  Similarly, transducers can be used to process `List`s more efficiently, but it is not limited to mapping operations.  `filter`, `take`, `drop`, and any other transducer can be efficiently composed.

```elm
import List as L
import Transduce as T exposing ((>>>))

slowMapChain = [1, 2, 3] |> L.map ((+) 10) |> L.map toString
fastMapChain = [1, 2, 3] |> L.map ((+) 10 >> toString)

slowChain = [1, 2, 3] |> L.filter ((/=) 2) |> L.map toString
fastChain = [1, 2, 3] |> T.transduceList (T.filter ((/=) 2) >>> T.map toString)
```

## Transducers can be reused

Transducers can be reused with many different data types.  `List`, `Signal`, `Array`, `Set`, `Dict` are supported by this library, and you can define your own transducer processes to work with other data types.  You can also define transducer processes that convert between types (for example, transducing from a `List` into a `Set`).

```elm
import Maybe
import String
import Transducer as T exposing ((>>>))

port stringSource : Signal String

parseValidInts =
	T.map String.toInt
	>>> T.map toMaybe
    >>> T.filter ((/=) Nothing)
	>>> T.map (Maybe.withDefault 0)

exampleList : List Int
exampleList = T.transduceList parseValidInts ["123", "-34", "35.0", "SDF", "7"]

exampleSignal : Signal Int
exampleSignal = T.transduceSignal parseValidInts stringSource

exampleConvert : Set Int
exampleConvert = T.transduce List.foldr Set.insert Set.empty parseValidInts ["123", "-34", "35.0", "SDF", "7"]
```


## Differences from clojure's tranducers

  - In elm, it is more natural for the `Reducer` type to be `a -> r -> r` instead of `r -> a -> r`
  - Elm is pure, meaning that elm transducers cannot hide state.  As a result, transducers cannot simply be functions as they are in clojure.  This also means that the transducer type must include a type parameter for the transducer's state.

 ## See also

  - [Understanding Clojure transducers through types](http://conscientiousprogrammer.com/blog/2014/08/07/understanding-cloure-transducers-through-types/)
  - [jonathanhefner/elm-seq](https://github.com/jonathanhefner/elm-seq)
  - [Quick recap/commentary: Clojure transducers](https://gist.github.com/ptaoussanis/e537bd8ffdc943bbbce7)
  - [Some trivial examples of using Clojure Transducers](http://ianrumford.github.io/blog/2014/08/08/Some-trivial-examples-of-using-Clojure-Transducers/)
  - [Type-safe transducers in Clojure. And Scala. And Haskell.](http://blog.podsnap.com/ducers2.html)
  - [Transducers are fundamental](http://ignaciothayer.com/post/Transducers-Are-Fundamental/)
