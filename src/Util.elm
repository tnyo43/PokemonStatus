module Util exposing (..)


updateList : List a -> Int -> a -> Maybe (List a)
updateList lst i x =
    if i < 0 || i >= List.length lst then Nothing
    else
        let       
            sub l j y =
                let
                    (k, tail) =
                        case List.tail l of
                            Just t -> (j, t)
                            Nothing -> (0, [])
                in
                if k == 0 then
                    y :: tail
                else
                    case List.head l of
                        Just h -> h :: sub tail (k-1) y
                        Nothing -> []
        in
        Just (sub lst i x)

getFromList : List a -> Int -> Maybe a
getFromList lst i =
    if i == 0 then List.head lst
    else
        case List.tail lst of
            Just t -> getFromList t (i-1)
            Nothing -> Nothing