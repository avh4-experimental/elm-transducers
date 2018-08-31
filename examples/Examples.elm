module Main exposing (main)

import Array
import Browser
import Browser.Events
import Html exposing (Html)
import Json.Decode
import Result
import Set
import String
import Transducer as T exposing (..)
import Transducer.Debug exposing (..)


double : Transducer a a r ()
double =
    { init = \reduce r -> ( (), r )
    , step = \xf a ( _, r ) -> ( (), r |> xf a |> xf a )
    , complete = \xf ( _, r ) -> r
    }


generate : a -> Transducer a a r ()
generate extra =
    { init = \reduce r -> ( (), r )
    , step = \xf a ( _, r ) -> ( (), xf a r )
    , complete = \xf ( _, r ) -> xf extra r
    }


take_ n =
    debug Debug.log Debug.log Debug.log "take" <| take n


map_ fn =
    debug Debug.log Debug.log Debug.log "map" <| map fn


filter_ fn =
    debug Debug.log Debug.log Debug.log "filter" <| filter fn


generate_ a =
    debug Debug.log Debug.log Debug.log "generate" <| generate a


double_ =
    debug Debug.log Debug.log Debug.log "double" <| double


combined =
    filter (\x -> x >= 3)
        |> composeWith (map Debug.toString)
        |> composeWith double
        |> composeWith (take 3)
        |> composeWith (generate "999")
        |> composeWith (generate "777")
        |> composeWith (partition 3)


combined_ =
    debug Debug.log Debug.log Debug.log "combined" combined


parseValidInts : Transducer String Int r ( ( (), () ), () )
parseValidInts =
    T.map String.toInt
        |> composeWith (T.filter ((/=) Nothing))
        |> composeWith (T.map (Maybe.withDefault 0))


show : a -> Html msg
show a =
    Html.text <| Debug.toString a


flowDown : List (Html msg) -> Html msg
flowDown children =
    children
        |> List.map (\child -> Html.div [] [ child ])
        |> Html.div []


render e =
    flowDown
        [ e
        , show <| transduceList (take 2) [ "A", "B", "C", "D" ]
        , show <| transduceList (map String.fromInt) [ 1, 2, 3, 4 ]
        , show <| transduceList (filter (\x -> x >= 3)) [ 1, 5, 2, 3, 4 ]
        , show <| transduceList double [ "A", "X", "B" ]
        , show <| transduceList combined [ 1, 2, 3, 4 ]
        , show <| transduceArray (take 2) (Array.initialize 5 identity)
        , show <| transduceArray (take 2 |> composeWith (map String.fromInt)) (Array.initialize 5 identity)

        --, show <| transduceArray combined (Array.initialize 5 identity)
        --, show <| transduce List.foldr Set.insert (Set.singleton "9") combined [1, 2, 3, 4]
        --, show <| transduce Set.foldr Set.insert Set.empty combined (Set.fromList [8])
        , show <| transduce List.foldr (+) 0 (double |> composeWith (generate 100)) [ 1, 2, 3 ]
        , show <| transduce Set.foldr (+) 0 (double |> composeWith (generate 100)) (Set.fromList [ 1, 2, 3 ])
        , show <| transduceList parseValidInts [ "123", "-34", "35.0", "SDF", "7" ]
        ]


mt =
    filter (\{ x, y } -> y <= 100)
        |> composeWith (map show)


decodeMouseLocation =
    Json.Decode.map2 (\x y -> { x = x, y = y })
        (Json.Decode.at [ "clientX" ] Json.Decode.int)
        (Json.Decode.at [ "clientY" ] Json.Decode.int)


main =
    Browser.element
        { init = \() -> ( Html.text "", Cmd.none )
        , subscriptions = \_ -> Browser.Events.onMouseMove decodeMouseLocation
        , update =
            \a _ ->
                ( transduceList mt [ a ]
                    |> Html.div []
                , Cmd.none
                )
        , view = render
        }
