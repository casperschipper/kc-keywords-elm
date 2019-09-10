module Util exposing (zip, zipWith)


zip : List a -> List b -> List ( a, b )
zip list1 list2 =
    case ( list1, list2 ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( a :: ass, b :: bs ) ->
            ( a, b ) :: zip ass bs


zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f l1 l2 =
    case ( l1, l2 ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( a :: ass, b :: bs ) ->
            f a b :: zipWith f ass bs
