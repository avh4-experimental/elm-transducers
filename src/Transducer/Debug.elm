module Transducer.Debug exposing (debug)

{-| This module provides support for debugging transducers.


# Logging

@docs debug

-}

import Transducer exposing (..)


{-| Wrap an existing transducer such that input and output of the transducer
will be logged with `Debug.log`.

Because Elm 0.19 no longer allows published packages to use `Debug.log`,
you must provide `Debug.log` as a parameter to this function.
Also, because Elm lacks Rank-N types, you must provide `Debug.log` three times
so it can be specialized for the input, the output, and the state of the transducer.

    import Debug

    filter' pred =
        debug Debug.log Debug.log Debug.log "filter" (filter pred)

    main =
        show (transduceList (filter' ((/=) 2)) [ 1, 2, 3 ])


    -- Console output:
    --     filter: input: 3
    --     filter: state: ()
    --     filter -> : 3
    --     filter: input: 2
    --     filter: state: ()
    --     filter: input: 1
    --     filter: state: ()
    --     filter -> : 1
    --     filter: complete: ()

-}
debug : (String -> a -> a) -> (String -> b -> b) -> (String -> s -> s) -> String -> Transducer a b r s -> Transducer a b r s
debug debugLogA debugLogB debugLogS name t =
    let
        logInput =
            debugLogA (name ++ ": input")

        logState =
            debugLogS (name ++ ": state")

        logComplete =
            debugLogS (name ++ ": complete")

        logProxy reduce input =
            reduce (debugLogB (name ++ " -> ") input)
    in
    { init = t.init
    , step =
        \reduce input ( s, r ) ->
            (logProxy reduce |> t.step) (logInput input) ( logState s, r )
    , complete =
        \reduce ( s, r ) ->
            t.complete (logProxy reduce) ( logComplete s, r )
    }
