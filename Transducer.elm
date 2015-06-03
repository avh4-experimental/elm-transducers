module Transducer4 where

import Set exposing (Set)

import Html
import Debug

type alias Reducer a r = a -> r -> r
type alias Transducer a b r s = 
    { init: s
    , step: Reducer b r -> Reducer a (s,r)
    , complete: Reducer b r -> (s,r) -> r
    }

map : (a -> b) -> Transducer a b r ()
map f =
    { init = ()
    , step = \xf a (_,r) -> ((),xf (f a) r)
    , complete = \xf (_,r) -> r
    }

filter : (a -> Bool) -> Transducer a a r ()
filter f =
    { init = ()
    , step = \xf a (_,r) -> if (f a) then ((),xf a r) else ((),r)
    , complete = \xf (_,r) -> r
    }

take : Int -> Transducer a a r Int
take n =
    { init = n
    , step = \xf a (s,r) -> if (s > 0) then (s-1,xf a r) else (0,r)
    , complete = \xf (s,r) -> r
    }

double : Transducer a a r ()
double =
    { init = ()
    , step = \xf a (_,r) -> ((), r |> xf a |> xf a)
    , complete = \xf (_,r) -> r
    }

comp : Transducer a b (s2,r) s1 -> Transducer b c r s2 -> Transducer a c r (s1,s2)
comp t1 t2 =
    { init = (t1.init, t2.init)
    , step = \xf a ((s1,s2),r) ->
        (t1.step (t2.step xf)) a (s1,(s2,r))
        |> \(ss1',(ss2',rr')) -> ((ss1',ss2'),rr')
    , complete = \xf ((s1,s2),r) ->
        (t1.complete (t2.step xf)) (s1,(s2,r))
        |> t2.complete xf
    }

debug : String -> Transducer a b r s -> Transducer a b r s
debug name t =
    let
        dxf xf = \a -> xf (Debug.log (name ++ " -> ") a)
    in
        { init = t.init
        , step = \xf a (s,r) -> (dxf xf |> t.step) (Debug.log (name ++ ": input") a) (Debug.log (name ++ ": state") s,r)
        , complete = \xf (s,r) -> t.complete (dxf xf) (Debug.log (name ++ ": complete") s,r)
        }

generate : a -> Transducer a a r ()
generate extra =
    { init = ()
    , step = \xf a (_,r) -> ((),xf a r)
    , complete = \xf (_,r) -> xf extra r
    }

type alias Fold a b c = (a -> b -> b) -> b -> c -> b

transduce : Fold a (s,r) x -> Reducer b r -> r -> Transducer a b r s -> x -> r
transduce fold reduce init t source =
    fold (t.step reduce) (t.init,init) source
    |> t.complete reduce

transduceSet : Transducer comparable comparable' (Set comparable'') s -> Set comparable -> Set comparable''
transduceSet = transduce Set.foldr Set.insert Set.empty

transduceList : Transducer a b (List b) s -> List a -> List b
transduceList = transduce List.foldr (::) []

take' n = debug "take" <| take n
map' fn = debug "map" <| map fn
filter' fn = debug "filter" <| filter fn
generate' a = debug "generate" <| generate a
double' = debug "double" <| double

----main = Html.text <| toString <| transduceListToList (take 2) ["A", "B", "C", "D"]
----main = Html.text <| toString <| transduceListToList (map toString) [1, 2, 3, 4]
----main = Html.text <| toString <| transduceListToList (filter (\x -> x >= 3)) [1, 5, 2, 3, 4]
----main = Html.text <| toString <| transduceListToList (double) ["A", "X", "B"]

combined =
    filter (\x -> x >= 3)
    `comp` map toString
    `comp` double
    `comp` take 3
    `comp` generate "999"
    `comp` generate "777"
combined' = debug "combined" combined
main = Html.text <| toString <| transduceList combined [1, 2, 3, 4]
--main = Html.text <| toString <| transduceSet combined (Set.singleton 8)


