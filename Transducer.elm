module Transducer3 where

import Html
import Debug


type alias Reducer a r s = r -> a -> (r,Maybe s)
type alias Transducer a b r s = Maybe s -> Reducer a r s -> Reducer b r s

map : (a -> b) -> Transducer b a r ()
map f s xf r a = xf r (f a)

filter : (a -> Bool) -> Transducer a a r ()
filter f s xf r a = if (f a) then xf r a else (r,s)

take : Int -> Transducer a a r Int
take n s xf r a = 
    let
        s' = s |> Maybe.withDefault n
    in
        if (s' >= 0) then xf r a else (r,Just 0)
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

double : Transducer a a r ()
double s0 xf s r a =
    let
        (r',s') = xf s r a
    in
        xf s' r' a

comb : Transducer b a r s' -> Transducer c b r s'' -> Transducer c a r (s',s'')
comb t1 t2 s0 xf (s1,s2) r a =
    --let
    --    s2' = 
    (xf |> t2 s2 |> t1 s1) (s1,s2) r a

--debug : String -> ((r -> a -> r) -> r -> b -> r) -> (r -> a -> r) -> r -> b -> r
debug : String -> Transducer a b r s -> Transducer a b r s
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

transduceList : Transducer a b (List a) s -> List b -> List a
transduceList xf a = List.foldr (flip <| xf (flip (::))) ([],Nothing) a
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


