module Transducer2 where

import Html
import Debug


type alias Reducer a r = r -> a -> r
type alias Transducer a b r = Reducer a r -> Reducer b r

--map : (a -> b) -> (r -> b -> r) -> r -> a -> r
--map : (a -> b) -> Reducer b r -> Reducer a r
map : (a -> b) -> Transducer b a r
map f xf r a = xf r (f a)

--filter : (a -> Bool) -> Transducer a a r
filter : (a -> Bool) -> (r -> a -> r) -> (r -> a -> r)
filter f xf r a = if (f a) then xf r a else r

take : Int -> Transducer a a r
take n xf r a = if (n >= 0) then xf (take (n-1) xf r a) a else r
--take n xf =
--    let
--        na = n
--    in
--        \r a ->

--take n =
--    { init = n
--    , step = \a s -> if s <= 0 then ([],0) else ([a],s-1)
--    , completion = \_ -> []
--    }

double : Transducer a a r
double xf r a = xf (xf r a) a

comb : Transducer b a r -> Transducer c b r -> Transducer c a r
comb t1 t2 = t1 << t2

--debug : String -> ((r -> a -> r) -> r -> b -> r) -> (r -> a -> r) -> r -> b -> r
debug : String -> Transducer a b r -> Transducer a b r
debug name t xf r a = (xf |> t) r (Debug.log name a)
--debug name xf r a = 
--    let
--        xf' xr xa = xf (Debug.log "xr" xr) (Debug.log "xa" xa)
--    in
--        Debug.log "out" (xf' (Debug.log "r" r) (Debug.log "a" a))
    --Debug.log name (xf (Debug.log name r) (Debug.log name a))
--debug name t =
--    { init = Debug.log (name ++ ": init") t.init
--    , step = \a s -> Debug.log (name ++ ": step") <| t.step (Debug.log (name ++ ": step") a) s
--    , completion = \s -> Debug.log (name ++ ": completion") <| t.completion s
--    }

--transduceList : Transducer b a (List a) -> List a -> List b
transduceList : Transducer a b (List a) -> List b -> List a
transduceList xf a = List.foldr (flip <| xf (flip (::))) [] a
--transduceList xf = List.foldl (xf conj) []
--transduceList : Transducer a s r -> (a -> b -> b) -> b -> List a -> b
--transduceList t reduce init items =
--    let
--        step s init' items' =
--            case items' of
--                [] ->
--                    let
--                        outs = t.completion s
--                    in
--                        List.foldl reduce init' outs
--                next::rest ->
--                    let
--                        (outs,s') = t.step next s
--                        init'' = List.foldl reduce init' outs
--                    in
--                        step s' init'' rest
--    in
--        step t.init init items

take' n = debug "take" <| take n
map' fn = debug "map" <| map fn
filter' fn = debug "filter" <| filter fn

--main = Html.text <| toString <| transduceList (take 2) ["A", "B", "C", "D"]
--main = Html.text <| toString <| transduceList (map toString) [1, 2, 3, 4]
--main = Html.text <| toString <| transduceList (filter (\x -> x >= 3)) [1, 5, 2, 3, 4]
--main = Html.text <| toString <| transduceList (double) ["A", "X", "B"]

combined = filter' (\x -> x >= 3) `comb` map' toString `comb` double -- `comb` take' 2
combined' = debug "combined" combined
main = Html.text <| toString <| transduceList combined' [1, 2, 3, 4]


