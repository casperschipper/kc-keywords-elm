module Main exposing (Model, Msg(..), Research, capitalize, decodeResearch, getResearch, init, main, subscriptions, update, view, viewMeta)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form as Form
import Browser
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map5, string)
import Random
import Table exposing (Column, defaultCustomizations)
import Util exposing (RGBColor, hexColor, stringToColor, zip, zipWith)


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


teacherTag : String
teacherTag =
    "Research by teachers of the Royal Conservatoire"


allTags : List String
allTags =
    [ teacherTag ]


isTag : String -> Bool
isTag =
    Util.flip List.member allTags


excludeTags : List String -> List String
excludeTags =
    List.filter (not << isTag)


isTeacherResearch : Research -> Bool
isTeacherResearch research =
    List.member teacherTag research.keywords


hyperlink : Link -> Html Msg
hyperlink link =
    case link of
        ResearchLink l ->
            viewResearchLink l

        KeywordLink l ->
            viewKeywordLink l


viewResearchLink : LinkInfo -> Html Msg
viewResearchLink link =
    a
        [ href link.url
        , target "_blank"
        ]
        [ text link.title ]


viewKeywordLink : LinkInfo -> Html Msg
viewKeywordLink keyword =
    a
        [ class "keyword"
        , href keyword.url
        , style "color" <| hexColor <| stringToColor keyword.title
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
    , teacherFilter : Bool
    }


emptyModel : Model
emptyModel =
    { researchList = []
    , keywordDict = emptyKeywords
    , viewType = KeywordView
    , tableState = Table.initialSort "title"
    , query = ""
    , loadingStatus = Loading
    , teacherFilter = False
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
    | SetTeacherFilter Bool



-- | GenColor
-- | NewColor


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

        SetTeacherFilter bool ->
            ( { model | teacherFilter = bool }, Cmd.none )



-- type alias ResearchData a =
--     { a
--         | researchList : List Research
--         , keywordDict : KeywordDict
--     }
-- applyTeacherFilter : ResearchData a -> ResearchData a
-- applyTeacherFilter data =
--     let
--         dictFilter key value =
--             isTeacherResearch value
--     in
--     { data
--         | researchList = List.filter data.researchList isTeacherResearch
--         , keywordDict = Dict.filter dictFilter data.keywordDict
--     }


makeLink : Research -> Link
makeLink research =
    let
        link =
            baseExpoUrl ++ String.fromInt (.id research)
    in
    ResearchLink <| LinkInfo (.title research) link


linkToUrl : Link -> String
linkToUrl link =
    case link of
        ResearchLink info ->
            info.url

        KeywordLink info ->
            info.url


viewAuthor : Research -> Html Msg
viewAuthor research =
    span [ class "author" ] [ text <| .author research ]



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
            , Table.stringColumn "Keywords" (String.join " " << excludeTags << .keywords)
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
            div [ class "mb-1" ]
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

        teacherFilterSwitch =
            label []
                [ text "filter by:"
                , div [ class "mb-1" ]
                    [ ButtonGroup.radioButtonGroup []
                        [ ButtonGroup.radioButton
                            (not
                                model.teacherFilter
                            )
                            [ Button.light, Button.onClick <| SetTeacherFilter False ]
                            [ text "All research" ]
                        , ButtonGroup.radioButton
                            model.teacherFilter
                            [ Button.light, Button.onClick <| SetTeacherFilter True ]
                            [ text "Research by teachers" ]
                        ]
                    ]
                ]

        content =
            case model.viewType of
                TableView ->
                    let
                        filtered =
                            if model.teacherFilter then
                                List.filter isTeacherResearch model.researchList

                            else
                                model.researchList
                    in
                    div
                        []
                        (viewResearchList model.tableState model.query filtered)

                KeywordView ->
                    let
                        filtered =
                            if model.teacherFilter then
                                Dict.filter (\key value -> List.any isTeacherResearch value) model.keywordDict

                            else
                                model.keywordDict
                    in
                    div [ id "keywords" ]
                        [ renderKeywords filtered ]
    in
    div [ id "top", class "container" ]
        [ div [ class "headers" ]
            [ h1 [] [ text "Research Results" ]
            , h4 [] [ text "Royal Conservatoire in The Hague" ]
            ]
        , radioSwitch
        , teacherFilterSwitch
        , content
        ]


viewResearchList : Table.State -> String -> List Research -> List (Html Msg)
viewResearchList tableState query researchList =
    let
        lowerQuery =
            String.toLower query

        acceptableResearch =
            List.filter (String.contains lowerQuery << String.toLower << .author) researchList
    in
    [ Form.form [ class "form-inline" ]
        [ Form.group []
            [ input
                [ class "form-control"
                , placeholder "Search by Author"
                , onInput SetQuery
                , style "margin" ".5rem 0"
                ]
                []
            ]
        ]
    , div
        [ class "table-responsive" ]
        [ Table.view config tableState acceptableResearch
        ]
    ]


applyValue : a -> List (a -> b) -> List b
applyValue value list =
    List.map (\f -> f value) list


renderField : ( String, String ) -> Html Msg
renderField ( value, name ) =
    label
        []
        [ text name
        , p [] [ text value ]
        ]


labelField : ( String, Html Msg ) -> Html Msg
labelField ( name, content ) =
    label [] [ text name, content ]


viewMeta : Research -> Html Msg
viewMeta research =
    let
        fields =
            [ ( String.fromInt research.id, "id" )
            , ( research.title, "title" )
            , ( String.join " " research.keywords, "keywords" )
            , ( research.created, "created" )
            , ( research.author, "author" )
            ]
    in
    div [] <| List.map renderField fields


viewShortMeta : Research -> Html Msg
viewShortMeta research =
    li [ class "short-meta" ]
        [ a [ href <| (linkToUrl << makeLink) research, target "_blank" ] [ text <| .title research ]
        , span [ class "author-name" ] [ text <| .author research ]
        ]


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


getTitle : Link -> String
getTitle l =
    case l of
        ResearchLink i ->
            i.title

        KeywordLink i ->
            i.title


linkColumn : String -> (data -> Link) -> Column data Msg
linkColumn name toLink =
    Table.veryCustomColumn
        { name = name
        , viewData = viewLink << toLink
        , sorter = Table.increasingOrDecreasingBy <| getTitle << toLink
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
    div [] <|
        List.singleton <|
            researchByKeywordList
                (excludeTags sortedKeys)
                dict


headerForSize : Int -> List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html.Html Msg
headerForSize count =
    case count of
        1 ->
            h5

        2 ->
            h4

        3 ->
            h3

        4 ->
            h2

        _ ->
            h1


scaleLink : Int -> List (Html.Attribute Msg) -> Html Msg -> Html Msg
scaleLink amount attrs html =
    case amount of
        5 ->
            h1 attrs [ html ]

        4 ->
            h2 attrs [ html ]

        3 ->
            h3 attrs [ html ]

        2 ->
            h4 attrs [ html ]

        1 ->
            h5 attrs [ html ]

        _ ->
            h1 attrs [ html ]


researchByKeywordList : List String -> KeywordDict -> Html Msg
researchByKeywordList sortedKeys dict =
    let
        renderRecord : String -> Maybe (List Research) -> Html Msg
        renderRecord key research =
            case research of
                Just [] ->
                    span [] [ text "empty keyword" ]

                Just list ->
                    let
                        number =
                            List.length list
                    in
                    div [ id <| keywordLink key ]
                        [ h5 [ class "keyword-header" ] [ text key ]
                        , a
                            [ class "back-to-top"
                            , href "#top"
                            , title "back to top"
                            ]
                            [ text "back to top" ]
                        , ul [ class "research-for-keyword" ] <|
                            List.map viewShortMeta list
                        ]

                Nothing ->
                    span [] []

        renderKey key =
            let
                list =
                    get key dict

                n =
                    case list of
                        Just [] ->
                            0

                        Just l ->
                            List.length l

                        Nothing ->
                            0

                mkLink =
                    scaleLink n [ class "keywordlink" ] << hyperlink << keyToLinkInfo
            in
            mkLink key

        renderKeyWithResearch key =
            renderRecord key (get key dict)
    in
    div [] <|
        List.concat
            [ List.map renderKey sortedKeys
            , [ hr [] [] ]
            , [ div [ class "keyword-research-list" ] <| List.map renderKeyWithResearch sortedKeys ]
            ]


capitalize : String -> String
capitalize string =
    let
        head =
            String.left 1 string

        tail =
            String.dropLeft 1 string
    in
    String.append (String.toUpper head) tail


fillKeywordsDict : List Research -> KeywordDict
fillKeywordsDict research =
    let
        updateKey : Research -> String -> KeywordDict -> KeywordDict
        updateKey res key dct =
            let
                capitalizedKey =
                    capitalize key
            in
            case get capitalizedKey dct of
                Just v ->
                    insert capitalizedKey (res :: v) dct

                Nothing ->
                    insert capitalizedKey [ res ] dct

        updateDict : Research -> KeywordDict -> KeywordDict
        updateDict res dict =
            List.foldr
                (updateKey res)
                dict
                res.keywords
    in
    List.foldr updateDict emptyKeywords research
