import Transducer exposing (..)
import Transducer as T exposing ((>>>))
import Transducer.Debug exposing (..)
import Graphics.Element exposing (show, flow, down)
import Set
import Array
import Mouse
import String
import Result
import Debug

double : Transducer a a r ()
double =
    { init = ()
    , step = \xf a (_,r) -> ((), r |> xf a |> xf a)
    , complete = \xf (_,r) -> r
    }

generate : a -> Transducer a a r ()
generate extra =
    { init = ()
    , step = \xf a (_,r) -> ((),xf a r)
    , complete = \xf (_,r) -> xf extra r
    }

take' n = debug "take" <| take n
map' fn = debug "map" <| map fn
filter' fn = debug "filter" <| filter fn
generate' a = debug "generate" <| generate a
double' = debug "double" <| double

combined =
    filter (\x -> x >= 3)
    >>> map toString
    >>> double
    >>> take 3
    >>> generate "999"
    >>> generate "777"
    >>> partition 3

combined' = debug "combined" combined

parseValidInts : Transducer String Int r ((((), ()), ()), ())
parseValidInts =
    T.map String.toInt
    >>> T.map Result.toMaybe
    >>> T.filter ((/=) Nothing)
    >>> T.map (Maybe.withDefault 0)

render e = flow down
    [ e
    , show <| transduceList (take 2) ["A", "B", "C", "D"]
    , show <| transduceList (map toString) [1, 2, 3, 4]
    , show <| transduceList (filter (\x -> x >= 3)) [1, 5, 2, 3, 4]
    , show <| transduceList (double) ["A", "X", "B"]
    , show <| transduceList combined [1, 2, 3, 4]
    , show <| transduceArray (take 2) (Array.initialize 5 identity)
    , show <| transduceArray (take 2 >>> map toString) (Array.initialize 5 identity)
    --, show <| transduceArray combined (Array.initialize 5 identity)
    --, show <| transduce List.foldr Set.insert (Set.singleton "9") combined [1, 2, 3, 4]
    --, show <| transduce Set.foldr Set.insert Set.empty combined (Set.fromList [8])
    , show <| transduce List.foldr (+) 0 (double `comp` generate 100) [1,2,3]
    , show <| transduce Set.foldr (+) 0 (double `comp` generate 100) (Set.fromList [1,2,3])
    , show <| transduceList parseValidInts ["123", "-34", "35.0", "SDF", "7"]
    ]
    
mt =
    filter (\(x,y) -> y <= 100)
    >>> map show
main = transduceSignal (mt) (show "No movement yet with y <= 100") Mouse.position
    |> Signal.map render
