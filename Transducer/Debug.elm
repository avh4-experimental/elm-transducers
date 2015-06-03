module Transducer.Debug where

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
        dxf xf = \a -> xf (Debug.log (name ++ " -> ") a)
    in
        { init = t.init
        , step = \xf a (s,r) -> (dxf xf |> t.step) (Debug.log (name ++ ": input") a) (Debug.log (name ++ ": state") s,r)
        , complete = \xf (s,r) -> t.complete (dxf xf) (Debug.log (name ++ ": complete") s,r)
        }
