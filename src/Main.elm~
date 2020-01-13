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
import Json.Decode exposing (Decoder, field, int, list, map, map8, maybe, string, succeed)
import Random
import Table exposing (Column, defaultCustomizations)
import Util exposing (RGBColor, hexColor, liftA2Bool, parenthesize, stringToColor, zip, zipWith)



-- Config


baseExpoUrl =
    "https://www.researchcatalogue.net/profile/show-exposition?exposition="


dataUrl =
    "data/internal_research.json"


localIssueId =
    -- used to identify local publications
    534751



-- Local Types


type alias Research =
    { id : Int
    , title : String
    , keywords : List String
    , created : String
    , author : String
    , researchType : ResearchType
    , issueId : Maybe Int
    , publicationStatus : PublicationStatus -- should be string ? then a valu0-=98
    }


type alias LinkInfo =
    { title : String
    , url : String
    }


type Link
    = ResearchLink LinkInfo
    | KeywordLink LinkInfo


type ResearchType
    = Teacher
    | Student
    | Lectorate
    | Unknown


type PublicationStatus
    = InProgress
    | Published
    | LocalPublication
    | Undecided


type Filter
    = All
    | Only ResearchType



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
    , filter : Filter
    , includeInternalResearch : Bool
    }


emptyModel : Model
emptyModel =
    { researchList = []
    , keywordDict = emptyKeywords
    , viewType = KeywordView
    , tableState = Table.initialSort "title"
    , query = ""
    , loadingStatus = Loading
    , filter = All
    , includeInternalResearch = True
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, getResearch )



-- helper funcs


teacherTag : String
teacherTag =
    "Research by teachers of the Royal Conservatoire"


lectorateTag : String
lectorateTag =
    "KonCon Lectorate"


allTags : List String
allTags =
    [ teacherTag, lectorateTag ]


isTag : String -> Bool
isTag =
    Util.flip List.member allTags


excludeTags : List String -> List String
excludeTags =
    List.filter (not << isTag)


isTeacherResearch : Research -> Bool
isTeacherResearch =
    List.member teacherTag << .keywords


isLectorateResearch : Research -> Bool
isLectorateResearch =
    List.member lectorateTag << .keywords


isStudentResearch : Research -> Bool
isStudentResearch =
    not << liftA2Bool (||) isTeacherResearch isLectorateResearch


calcStatus : Research -> PublicationStatus
calcStatus research =
    case research.publicationStatus of
        InProgress ->
            InProgress

        _ ->
            case research.issueId of
                Just id ->
                    if id == localIssueId then
                        LocalPublication

                    else
                        Published

                Nothing ->
                    Published


statusToString : PublicationStatus -> String
statusToString status =
    case status of
        InProgress ->
            "in progress"

        Published ->
            "published"

        LocalPublication ->
            "local publication"

        Undecided ->
            "..."



-- not << (isTeacherResearch || isLectorateResearch)


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



-- JSON Decoders


decodeResearch : Decoder (List Research)
decodeResearch =
    Json.Decode.list entry


entry : Decoder Research
entry =
    let
        researchType : Research -> Research
        researchType research =
            if isTeacherResearch research then
                { research | researchType = Teacher }

            else if isLectorateResearch research then
                { research | researchType = Lectorate }

            else
                -- there is no tag for students
                { research | researchType = Student }

        researchPublicationStatus : Research -> Research
        researchPublicationStatus research =
            { research | publicationStatus = calcStatus research }

        statusFromString : String -> PublicationStatus
        statusFromString statusString =
            case statusString of
                "published" ->
                    Published

                "in progress" ->
                    InProgress

                _ ->
                    Undecided
    in
    Json.Decode.map (researchType << researchPublicationStatus)
        (Json.Decode.map8 Research
            (field "id" int)
            (field "title" string)
            (field "keywords" (Json.Decode.list string))
            (field "created" string)
            (field "author" <| field "name" string)
            (succeed Unknown)
            (maybe (field "issue" <| field "id" int))
            (Json.Decode.map statusFromString (field "status" string))
        )



-- UPDATE


type Msg
    = Go
    | GotList (Result Http.Error (List Research))
    | SetQuery String
    | SetTableState Table.State
    | SetViewType ViewType
    | SetFilter Filter
    | ToggleInternalPublicationFilter



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

        SetFilter filter ->
            let
                newView =
                    case filter of
                        Only Teacher ->
                            TableView

                        Only Lectorate ->
                            TableView

                        _ ->
                            model.viewType
            in
            ( { model | filter = filter, viewType = newView }, Cmd.none )

        ToggleInternalPublicationFilter ->
            ( { model | includeInternalResearch = not model.includeInternalResearch }, Cmd.none )



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


attrsFromResearch : Research -> List (Attribute Msg)
attrsFromResearch research =
    case research.publicationStatus of
        LocalPublication ->
            [ class "local-publication" ]

        _ ->
            [ class "global-publication" ]


config : Table.Config Research Msg
config =
    Table.customConfig
        { toId = String.fromInt << .id
        , toMsg = SetTableState
        , columns =
            [ typeColumn "Type" .researchType
            , linkColumn "Title" makeLink
            , Table.stringColumn "Author" .author
            , createdColumn "Created" .created
            , Table.stringColumn "Keywords" (String.join ", " << List.map capitalize << excludeTags << .keywords)
            , Table.stringColumn "Status" (statusToString << .publicationStatus)
            ]
        , customizations = { defaultCustomizations | tableAttrs = [ class "table" ], rowAttrs = attrsFromResearch }
        }


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


filterResearch : Filter -> List Research -> List Research
filterResearch filter list =
    case filter of
        All ->
            list

        Only filterType ->
            List.filter ((==) filterType << .researchType) list


viewResearch : Model -> Html Msg
viewResearch model =
    let
        radioSwitchView =
            label []
                [ text "Switch view:"
                , div [ class "mb-1" ]
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
                ]

        publicInternalSwitch =
            label [ class "ml-1" ]
                [ text "Internal publications: "
                , div []
                    [ ButtonGroup.radioButtonGroup []
                        [ ButtonGroup.radioButton
                            (not
                                model.includeInternalResearch
                            )
                            [ Button.info, Button.onClick ToggleInternalPublicationFilter ]
                            [ text "Hide" ]
                        , ButtonGroup.radioButton
                            model.includeInternalResearch
                            [ Button.info, Button.onClick ToggleInternalPublicationFilter ]
                            [ text "Show" ]
                        ]
                    , if model.includeInternalResearch then
                        p [ class "internal-note" ]
                            [ text "KC internal publications shown :", span [ class "important-note" ] [ text " access for KC staff and students only" ] ]

                      else
                        p [ class "internal-note" ] [ text "Internal publications are hidden" ]
                    ]
                ]

        filterSwitch =
            let
                current =
                    model.filter
            in
            label []
                [ text "Show research by:"
                , div [ class "mb-1" ]
                    [ ButtonGroup.radioButtonGroup []
                        [ ButtonGroup.radioButton
                            (current == All)
                            [ Button.light, Button.onClick <| SetFilter All ]
                            [ text "All" ]
                        , ButtonGroup.radioButton
                            (current == Only Teacher)
                            [ Button.light, Button.onClick <| SetFilter (Only Teacher) ]
                            [ text "Teachers" ]
                        , ButtonGroup.radioButton
                            (current == Only Student)
                            [ Button.light, Button.onClick <| SetFilter (Only Student) ]
                            [ text "Students" ]
                        , ButtonGroup.radioButton
                            (current == Only Lectorate)
                            [ Button.light, Button.onClick <| SetFilter (Only Lectorate) ]
                            [ text "The lectorate 'Music, Education and Society'" ]
                        ]
                    ]
                ]

        filtered =
            -- Student/Teacher etc..
            filterResearch model.filter model.researchList

        filteredOnStatus =
            -- publication status
            if model.includeInternalResearch then
                filtered

            else
                List.filter
                    (\research ->
                        case research.publicationStatus of
                            LocalPublication ->
                                model.includeInternalResearch

                            _ ->
                                True
                    )
                    filtered

        content =
            case model.viewType of
                TableView ->
                    div
                        []
                        (viewResearchList model.tableState model.query filteredOnStatus)

                KeywordView ->
                    let
                        filteredDict =
                            fillKeywordsDict <| filteredOnStatus
                    in
                    div [ id "keywords" ]
                        [ renderKeywords filteredDict ]
    in
    div [ id "top", class "container" ]
        [ div [ class "headers" ]
            [ h1 [] [ text "Research Results" ]
            , h4 [] [ text "Royal Conservatoire in The Hague" ]
            ]
        , filterSwitch
        , br [] []
        , publicInternalSwitch
        , br [] []
        , radioSwitchView
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
    li ([ class "research-meta" ] ++ attrsFromResearch research)
        [ p []
            [ a
                [ href <| (linkToUrl << makeLink) research, target "_blank" ]
                [ text <| research.title ]
            , span
                [ class "research-meta-status", title "publication status" ]
                [ text <| " " ++ (parenthesize <| statusToString research.publicationStatus) ]
            ]
        , p [ class "research-meta-author" ]
            [ text <| research.author
            , span [ class "research-meta-type" ] [ text <| " " ++ (parenthesize <| typeToString research.researchType) ]
            ]
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


typeColumn : String -> (data -> ResearchType) -> Column data msg
typeColumn name getType =
    Table.customColumn
        { name = name
        , viewData = typeToString << getType
        , sorter = Table.increasingOrDecreasingBy <| typeToString << getType
        }


typeToString : ResearchType -> String
typeToString researchType =
    case researchType of
        Teacher ->
            "Teacher"

        Student ->
            "Student"

        Lectorate ->
            "Lectorate"

        Unknown ->
            "Unknown"


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
        { url = dataUrl
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
