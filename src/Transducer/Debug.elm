module Transducer.Debug exposing (..)

{-| This module provides support for debugging transducers.

# Logging
@docs debug

-}

import Debug
import Transducer exposing (..)


{-| Wrap an existing transducer such that input and output of the transducer
will be logged with `Debug.log`.

    filter' pred = debug "filter" (filter pred)
    main = show (transduceList (filter' ((/=) 2)) [1, 2, 3])
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
debug : String -> Transducer a b r s -> Transducer a b r s
debug name t =
    let
        logInput =
            Debug.log (name ++ ": input")

        logState =
            Debug.log (name ++ ": state")

        logComplete =
            Debug.log (name ++ ": complete")

        logProxy reduce input =
            reduce (Debug.log (name ++ " -> ") input)
    in
        { init = t.init
        , step =
            \reduce input ( s, r ) ->
                (logProxy reduce |> t.step) (logInput input) ( logState s, r )
        , complete =
            \reduce ( s, r ) ->
                t.complete (logProxy reduce) ( logComplete s, r )
        }
