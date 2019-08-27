module Main exposing (Model(..), Msg(..), Research, decodeResearch, getResearch, init, main, subscriptions, update, view, viewMeta)

import Browser
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map5, string)



-- Research type


type alias Research =
    { id : Int
    , title : String
    , keywords : List String
    , created : String
    , author : String
    }


decodeResearch : Decoder (List Research)
decodeResearch =
    list entry


entry : Decoder Research
entry =
    map5 Research
        (field "id" int)
        (field "title" string)
        (field "keywords" (list string))
        (field "created" string)
        (field "author" <| field "name" string)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


test : String
test =
    """[{"id":409098,"type":"exposition","title":"LA COURANTE FRAN\\u00c7OISE. Historically informed performance of the French Courante for harpsichord during the second half of the seventeenth century following the criteria obtained from the Baroque Dance.","keywords":["Courante","Harpsichord","Dance","France","Clavecin","seventeenth centrury","baroque","Suite","Proportio sesquialtera","historically informed performance","Historical treatises","Chambonni\\u00e8res","Louis Couperin","d'Anglebert"],"created":"28/11/2017","status":"published","doi":{"id":null,"url":null},"published":"15/08/2019","published_in":[{"id":6,"name":"KC Research Portal","name_short":"KC Research Portal"}],"issue":{"id":479587,"number":"1","title":"Master Research Projects"},"author":{"id":394894,"name":"Diego Ruenes Rubiales"},"coauthors":[]}]"""



-- MODEL


type Model
    = Failure String
    | Loading
    | Success KeywordDict


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getResearch )



-- UPDATE


type Msg
    = Go
    | GotList (Result Http.Error (List Research))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Go ->
            ( Loading, getResearch )

        GotList result ->
            case result of
                Ok list ->
                    ( Success <| fillKeywordsDict list, Cmd.none )

                Err err ->
                    let
                        message =
                            case err of
                                Http.BadUrl string ->
                                    Failure <| "bad url" ++ string

                                Http.Timeout ->
                                    Failure "timeout"

                                Http.NetworkError ->
                                    Failure "network error"

                                Http.BadStatus int ->
                                    Failure <| "bad status" ++ String.fromInt int

                                Http.BadBody string ->
                                    Failure <| "body of improper format" ++ string
                    in
                    ( message, Cmd.none )



-- type Error
--     = BadUrl String
--     | Timeout
--     | NetworkError
--     | BadStatus Int
--     | BadBody String
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "KC master research" ]
        , viewResearchList model
        ]


viewResearchList : Model -> Html Msg
viewResearchList model =
    case model of
        Failure details ->
            div []
                [ text <| "Could not load the list -> " ++ details
                , button [ onClick Go ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success dict ->
            let
                list =
                    List.concat <| values dict
            in
            div []
                [ button [ onClick Go, style "display" "block" ] [ text "More Please!" ]
                , div [] <| List.map viewMeta list
                ]


applyValue : a -> List (a -> b) -> List b
applyValue value list =
    List.map (\f -> f value) list


viewMeta : Research -> Html Msg
viewMeta research =
    let
        renderField : ( String, String ) -> Html Msg
        renderField ( value, name ) =
            label
                []
                [ text name
                , p [] [ text value ]
                ]

        fields =
            [ ( String.fromInt research.id, "id" )
            , ( research.title, "title" )
            , ( String.join " " research.keywords, "keywords" )
            , ( research.created, "created" )
            , ( research.author, "author" )
            ]
    in
    div [] <| List.map renderField fields



-- HTTP


getResearch : Cmd Msg
getResearch =
    Http.get
        { url = "data/KCdate_26_Aug_2019.json"
        , expect = Http.expectJson GotList decodeResearch
        }


type alias KeywordDict =
    Dict String (List Research)



-- Dictionary


emptyKeywords : KeywordDict
emptyKeywords =
    Dict.empty


fillKeywordsDict : List Research -> KeywordDict
fillKeywordsDict research =
    let
        updateKey : Research -> String -> KeywordDict -> KeywordDict
        updateKey res key dct =
            case get key dct of
                Just v ->
                    insert key (res :: v) dct

                Nothing ->
                    insert key [ res ] dct

        updateDict : Research -> KeywordDict -> KeywordDict
        updateDict res dict =
            List.foldr
                (updateKey res)
                dict
                res.keywords
    in
    List.foldr updateDict emptyKeywords research
