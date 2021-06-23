module Util exposing (RGBColor, flip, hexColor, parenthesize, stringToColor)

import Hex


parenthesize : String -> String
parenthesize x =
    "(" ++ x ++ ")"


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x


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

        countCommon =
            let
                a =
                    countLetter "e" + countLetter "d"

                b =
                    countLetter "a" + countLetter "r"

                c =
                    countLetter "i" + countLetter "m"
            in
            mapTriple toFloat ( a, b, c )

        mup m ( a, b, c ) =
            ( a * m, b * m, c * m )

        offset o ( a, b, c ) =
            ( o + a, o + b, o + c )

        ( red, green, blue ) =
            mapTriple floor <| offset 32 <| mup 64 <| countCommon 
    in
    Color red green blue
