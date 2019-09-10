module Main exposing (Model, Msg(..), Research, decodeResearch, getResearch, init, main, subscriptions, update, view, viewMeta)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Browser
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map5, string)
import Table exposing (Column, defaultCustomizations)
import Util exposing (zip, zipWith)


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


type alias LinkInfo =
    { title : String
    , url : String
    }


type Link
    = ResearchLink LinkInfo
    | KeywordLink LinkInfo


hyperlink : Link -> Html Msg
hyperlink link =
    case link of
        ResearchLink l ->
            viewResearchLink l

        KeywordLink l ->
            viewKeywordLink l


viewResearchLink : LinkInfo -> Html Msg
viewResearchLink link =
    a [ href link.url ] [ text link.title ]


viewKeywordLink : LinkInfo -> Html Msg
viewKeywordLink keyword =
    a
        [ class "keyword"
        , href keyword.url
        ]
        [ text keyword.title ]


keyToLinkInfo : String -> Link
keyToLinkInfo key =
    KeywordLink <| LinkInfo key ("#" ++ keywordLink key)


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


makeLink : Research -> Link
makeLink research =
    let
        link =
            baseExpoUrl ++ String.fromInt (.id research)
    in
    ResearchLink <| LinkInfo (.title research) link



-- config : Table.Config Research Msg
-- config =
--     Table.config
--         { toId = String.fromInt << .id
--         , toMsg = SetTableState
--         , columns =
--             [ Table.intColumn "Id" .id
--             , linkColumn "Title" makeLink
--             , Table.stringColumn "Author" .author
--             , createdColumn "Created" .created
--             , Table.stringColumn "Keywords" (String.join " " << .keywords)
--             ]
--         }


config : Table.Config Research Msg
config =
    Table.customConfig
        { toId = String.fromInt << .id
        , toMsg = SetTableState
        , columns =
            [ Table.intColumn "Id" .id
            , linkColumn "Title" makeLink
            , Table.stringColumn "Author" .author
            , createdColumn "Created" .created
            , Table.stringColumn "Keywords" (String.join " " << .keywords)
            ]
        , customizations = { defaultCustomizations | tableAttrs = [ class "table" ] }
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
    let
        radioSwitch =
            div [ class "row" ]
                [ ButtonGroup.radioButtonGroup []
                    [ ButtonGroup.radioButton
                        (model.viewType == TableView)
                        [ Button.primary, Button.onClick <| SetViewType TableView ]
                        [ text "list view" ]
                    , ButtonGroup.radioButton
                        (model.viewType == KeywordView)
                        [ Button.primary, Button.onClick <| SetViewType KeywordView ]
                        [ text "keyword view" ]
                    ]
                ]

        content =
            case model.viewType of
                TableView ->
                    div [ class "row" ]
                        [ h1 [ id "KC-portal-research" ]
                            [ text "KC master research" ]
                        , div
                            []
                            (viewResearchList
                                model
                            )
                        ]

                KeywordView ->
                    div [] [ viewKeywords model ]
    in
    div [ class "container" ] [ radioSwitch, content ]


viewKeywords : Model -> Html Msg
viewKeywords model =
    renderKeywords model.keywordDict


viewResearchList : Model -> List (Html Msg)
viewResearchList model =
    let
        lowerQuery =
            String.toLower model.query

        acceptableResearch =
            List.filter (String.contains lowerQuery << String.toLower << .author) model.researchList
    in
    [ div [ class "row" ]
        [ div
            [ class "col-sm-6" ]
            [ h1 [] [ text "list view" ] ]
        , div [ class "col-sm-6" ] [ input [ class "form-control", placeholder "Search by Author", onInput SetQuery ] [] ]
        ]
    , div
        [ class "table-responsive" ]
        [ Table.view config model.tableState acceptableResearch
        ]
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
    table [ class "table" ] <| List.map renderField fields


createdColumn : String -> (data -> String) -> Column data msg
createdColumn name toCreated =
    let
        sortableDateString =
            String.split "/" >> List.reverse >> String.join "/"
    in
    Table.customColumn
        { name = name
        , viewData = \data -> toCreated data
        , sorter = Table.increasingOrDecreasingBy <| sortableDateString << toCreated
        }


linkColumn : String -> (data -> Link) -> Column data msg
linkColumn name toLink =
    Table.veryCustomColumn
        { name = name
        , viewData = \data -> viewLink (toLink data)
        , sorter = Table.increasingBy <| .id
        }


viewLink : Link -> Table.HtmlDetails Msg
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


keywordLink : String -> String
keywordLink keyword =
    String.replace " " "-" keyword


renderKeywords : KeywordDict -> Html Msg
renderKeywords dict =
    let
        sortedKeys : List String
        sortedKeys =
            List.sort (keys dict)

        viewKey =
            hyperlink << keyToLinkInfo
    in
    div [ id "keyword-list" ] <|
        List.singleton <|
            researchByKeywordList
                sortedKeys
                dict


researchByKeywordList : List String -> KeywordDict -> Html Msg
researchByKeywordList sortedKeys dict =
    let
        renderRecord : Maybe (List Research) -> Html Msg
        renderRecord research =
            case research of
                Just [] ->
                    span [] [ text "empty keyword" ]

                Just list ->
                    div [] <| List.map (hyperlink << makeLink) list

                Nothing ->
                    span [] []

        renderKey =
            hyperlink << keyToLinkInfo

        renderKeyWithResearch key =
            div [ id <| keywordLink key ]
                [ h3 [] [ text key ]
                , a [ href "#keywords" ] [ text "return" ]
                , renderRecord (get key dict)
                ]
    in
    div [ class "row" ] <|
        List.concat
            [ List.map renderKey sortedKeys
            , List.map renderKeyWithResearch sortedKeys
            ]


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
