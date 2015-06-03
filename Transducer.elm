module Transducer where

import Html
import Debug

type alias Transducer a s b =
    { init: s
    , step: a -> s -> (List b, s)
    , completion: s -> List b
    }

map : (a -> b) -> Transducer a () b
map fn =
    { init = ()
    , step = \a _ -> ([fn a], ())
    , completion = \_ -> []
    }

filter : (a -> Bool) -> Transducer a () a
filter fn =
    { init = ()
    , step = \a _ -> if fn a then ([a], ()) else ([], ())
    , completion = \_ -> []
    }

take : Int -> Transducer a Int a
take n =
    { init = n
    , step = \a s -> if s <= 0 then ([],0) else ([a],s-1)
    , completion = \_ -> []
    }

--comb : Transducer a s b -> Transducer b s' c -> Transducer a s' c
--comb t1 t2 =
--    let
--        step a s = x
--    in
--        { init = (t1.init, t2.init)
--        , step = transduceList t2 
--        , completion = x
--        }

debug : String -> Transducer a s b -> Transducer a s b
debug name t =
    { init = Debug.log (name ++ ": init") t.init
    , step = \a s -> Debug.log (name ++ ": step") <| t.step (Debug.log (name ++ ": step") a) s
    , completion = \s -> Debug.log (name ++ ": completion") <| t.completion s
    }

transduceList : Transducer a s r -> List a -> List b
transduceList t items =
    let
        reduce = (::)
        step s init' items' =
            case items' of
                [] ->
                    let
                        outs = t.completion s
                    in
                        List.foldl reduce init' outs
                next::rest ->
                    let
                        (outs,s') = t.step next s
                        init'' = List.foldl reduce init' outs
                    in
                        step s' init'' rest
    in
        step t.init [] items

take' n = debug "take" <| take n
map' fn = debug "map" <| map fn
filter' fn = debug "filter" <| filter fn

--main = Html.text <| toString <| transduceList (take' 2) ["A", "B", "C", "D"]
--main = Html.text <| toString <| transduceList (map' toString) [1, 2, 3, 4]
main = Html.text <| toString <| transduceList (filter' (\x -> x >= 3)) [1, 2, 3, 4]

--combined = filter' (\x -> x >= 3) `comb` map' toString `comb` take' 2
--combined' = debug "combined" combined
--main = Html.text <| toString <| transduceList combined' [1, 2, 3, 4]
