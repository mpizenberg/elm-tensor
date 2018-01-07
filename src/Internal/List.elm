module Internal.List
    exposing
        ( foldl2
        )


foldl2 : (a -> a -> b -> b) -> b -> List a -> List a -> b
foldl2 f initialValue =
    let
        helper acc l1 l2 =
            case ( l1, l2 ) of
                ( x1 :: xs1, x2 :: xs2 ) ->
                    helper (f x1 x2 acc) xs1 xs2

                _ ->
                    acc
    in
    helper initialValue
