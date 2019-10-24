module Util exposing (RGBColor, flip, hexColor, stringToColor, zip, zipWith)

import Hex


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x


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


type RGBColor
    = Color Int Int Int


hexColor : RGBColor -> String
hexColor color =
    case color of
        Color r g b ->
            String.concat <| "#" :: List.map Hex.toString [ r, g, b ]


stringToColor : String -> RGBColor
stringToColor string =
    let
        countLetter : String -> Int
        countLetter letter =
            List.length <| String.indexes letter string

        mapTriple f ( a, b, c ) =
            ( f a, f b, f c )

        countCommon haystack =
            let
                a =
                    countLetter "e" + countLetter "d"

                b =
                    countLetter "a" + countLetter "r"

                c =
                    countLetter "i" + countLetter "m"
            in
            mapTriple toFloat ( a, b, c )

        normalize : ( Int, Int, Int ) -> ( Float, Float, Float )
        normalize ( a, b, c ) =
            let
                total =
                    toFloat (a + b + c)
            in
            ( toFloat a / total, toFloat b / total, toFloat c / total )

        mup m ( a, b, c ) =
            ( a * m, b * m, c * m )

        offset o ( a, b, c ) =
            ( o + a, o + b, o + c )

        ( red, green, blue ) =
            mapTriple floor <| offset 32 <| mup 64 <| countCommon string
    in
    Color red green blue
