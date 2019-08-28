module Main exposing (Model, Msg(..), Research, decodeResearch, getResearch, init, main, subscriptions, update, view, viewMeta)

import Browser
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map5, string)
import Table exposing (Column)


baseExpoUrl : String
baseExpoUrl =
    "https://www.researchcatalogue.net/profile/show-exposition?exposition="



-- Research type


type alias Research =
    { id : Int
    , title : String
    , keywords : List String
    , created : String
    , author : String
    }


type alias ExpoLink =
    { title : String
    , url : String
    }


hyperlink : ExpoLink -> Html msg
hyperlink link =
    a [ href link.url ] [ text link.title ]


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


type LoadingStatus
    = Failure String
    | Loading
    | Success


type ViewType
    = TableView
    | KeywordView


type alias Model =
    { researchList : List Research
    , keywordDict : KeywordDict
    , viewType : ViewType
    , tableState : Table.State
    , query : String
    , loadingStatus : LoadingStatus
    }


emptyModel : Model
emptyModel =
    { researchList = []
    , keywordDict = emptyKeywords
    , viewType = TableView
    , tableState = Table.initialSort "title"
    , query = ""
    , loadingStatus = Loading
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, getResearch )



-- UPDATE


type Msg
    = Go
    | GotList (Result Http.Error (List Research))
    | SetQuery String
    | SetTableState Table.State
    | SetViewType ViewType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Go ->
            ( { model | loadingStatus = Loading }, getResearch )

        GotList result ->
            case result of
                Ok list ->
                    ( { model
                        | loadingStatus = Success
                        , researchList = list
                        , keywordDict = fillKeywordsDict list
                      }
                    , Cmd.none
                    )

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
                    ( { model | loadingStatus = message }, Cmd.none )

        SetQuery newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }, Cmd.none )

        SetViewType newType ->
            ( { model | viewType = newType }, Cmd.none )


makeLink : Research -> ExpoLink
makeLink research =
    let
        link =
            baseExpoUrl ++ String.fromInt (.id research)
    in
    ExpoLink (.title research) link


config : Table.Config Research Msg
config =
    Table.config
        { toId = String.fromInt << .id
        , toMsg = SetTableState
        , columns =
            [ Table.intColumn "Id" .id
            , linkColumn "Title" makeLink
            , Table.stringColumn "Author" .author
            , Table.stringColumn "Created" .created
            , Table.stringColumn "Keywords" (String.join " " << .keywords)
            ]
        }



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
    case model.loadingStatus of
        Failure details ->
            div []
                [ text <| "Could not load the list -> " ++ details
                , button [ onClick Go ] [ text "Try Again!" ]
                ]

        Loading ->
            h1 [] [ text "Loading..." ]

        Success ->
            viewResearch model


viewResearch : Model -> Html Msg
viewResearch model =
    case model.viewType of
        TableView ->
            div []
                [ h1 [ id "KC-portal-research" ] [ text "KC master research" ]
                , viewResearchList model
                ]

        KeywordView ->
            viewKeywords model


viewKeywords : Model -> Html Msg
viewKeywords model =
    div [] [ text "nothing to see folks.." ]


viewResearchList : Model -> Html Msg
viewResearchList model =
    let
        lowerQuery =
            String.toLower model.query

        acceptableResearch =
            List.filter (String.contains lowerQuery << String.toLower << .author) model.researchList
    in
    div []
        [ h1 [] [ text "list view" ]
        , input [ placeholder "Search by Author", onInput SetQuery ] []
        , Table.view config model.tableState acceptableResearch
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
    table [] <| List.map renderField fields


linkColumn : String -> (data -> ExpoLink) -> Column data msg
linkColumn name toLink =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewLink (toLink data)
        , sorter = Table.increasingBy (.title << toLink)
        }


viewLink : ExpoLink -> Table.HtmlDetails msg
viewLink link =
    Table.HtmlDetails []
        [ hyperlink link ]



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
