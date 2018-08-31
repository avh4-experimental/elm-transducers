module TransducerTest exposing (all, combined, double, generate, parseValidInts)

import Array
import Expect
import Set
import Test exposing (..)
import Transducer as T exposing (..)


all : Test
all =
    describe "Transducer"
        [ describe "examples"
            [ test "1" <|
                \() ->
                    transduceList (take 2) [ "A", "B", "C", "D" ]
                        |> Expect.equal [ "C", "D" ]
            , test "2" <|
                \() ->
                    transduceList (map String.fromInt) [ 1, 2, 3, 4 ]
                        |> Expect.equal [ "1", "2", "3", "4" ]
            , test "3" <|
                \() ->
                    transduceList (filter (\x -> x >= 3)) [ 1, 5, 2, 3, 4 ]
                        |> Expect.equal [ 5, 3, 4 ]
            , test "4" <|
                \() ->
                    transduceList double [ "A", "X", "B" ]
                        |> Expect.equal [ "A", "A", "X", "X", "B", "B" ]
            , test "5" <|
                \() ->
                    transduceList combined [ 1, 2, 3, 4 ]
                        |> Expect.equal [ [ "777", "999" ], [ "3", "4", "4" ] ]
            , test "6" <|
                \() ->
                    transduceArray (take 2) (Array.initialize 5 identity)
                        |> Expect.equal (Array.fromList [ 0, 1 ])
            , test "7" <|
                \() ->
                    transduceArray (take 2 |> composeWith (map String.fromInt)) (Array.initialize 5 identity)
                        |> Expect.equal (Array.fromList [ "0", "1" ])
            , test "8" <|
                \() ->
                    transduceArray combined (Array.initialize 5 identity)
                        |> Expect.equal (Array.fromList [ [ "4", "3", "3" ], [ "777", "999" ] ])
            , test "9" <|
                \() ->
                    transduce List.foldr Set.insert (Set.singleton [ "9" ]) combined [ 1, 2, 3, 4 ]
                        |> Expect.equal (Set.fromList [ [ "3", "4", "4" ], [ "777", "999" ], [ "9" ] ])
            , test "10" <|
                \() ->
                    transduce Set.foldr Set.insert Set.empty combined (Set.fromList [ 8 ])
                        |> Expect.equal (Set.fromList [ [ "777" ], [ "999", "8", "8" ] ])
            , test "11" <|
                \() ->
                    transduce List.foldr (+) 0 (double |> composeWith (generate 100)) [ 1, 2, 3 ]
                        |> Expect.equal 112
            , test "12" <|
                \() ->
                    transduce Set.foldr (+) 0 (double |> composeWith (generate 100)) (Set.fromList [ 1, 2, 3 ])
                        |> Expect.equal 112
            , test "13" <|
                \() ->
                    transduceList parseValidInts [ "123", "-34", "35.0", "SDF", "7" ]
                        |> Expect.equal [ 123, -34, 7 ]
            ]
        ]


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


combined =
    filter (\x -> x >= 3)
        |> composeWith (map Debug.toString)
        |> composeWith double
        |> composeWith (take 3)
        |> composeWith (generate "999")
        |> composeWith (generate "777")
        |> composeWith (partition 3)


parseValidInts : Transducer String Int r ( ( (), () ), () )
parseValidInts =
    T.map String.toInt
        |> composeWith (T.filter ((/=) Nothing))
        |> composeWith (T.map (Maybe.withDefault 0))
