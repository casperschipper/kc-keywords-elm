module Main exposing (Msg(..), Research, main)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (UrlRequest(..))
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, maybe, string, succeed)
import Json.Decode.Extra as JDE
import List exposing (any)
import List.Extra as L
import Set
import Set.Any exposing (AnySet(..))
import Table exposing (Column, defaultCustomizations)
import Time exposing (Posix)
import Util exposing (hexColor, parenthesize, stringToColor)



{- This is an elm application to create a table overview of RC search results
   this particular implementation is for the KC portal
-}
-- Config


baseExpoUrl : String
baseExpoUrl =
    "https://www.researchcatalogue.net/profile/show-exposition?exposition="


dataUrl : String
dataUrl =
    "data/internal_research.json"


localIssueId : number
localIssueId =
    -- used to identify local publications of KC portal
    534751



-- These are specific keywords we use in KC


teacherTag : String
teacherTag =
    "Research by teachers of the Royal Conservatoire"


lectorateTag : String
lectorateTag =
    "KonCon Lectorate"



-- Local Types


type alias Research =
    { id : Int
    , title : String
    , keywords : List String
    , created : String
    , author : String
    , researchType : ResearchType -- This type local to KC, may differ for other portals
    , issueId : Maybe Int
    , publicationStatus : PublicationStatus -- should be string?
    , publication : Maybe String
    , lastChanged : Maybe Posix
    }


type alias LinkInfo =
    { title : String
    , url : String
    }



-- There are internal and public expositions in the RC, internal means only portal members can see it.


type LinkVisibility
    = InternalLink
    | PublicLink


type Link
    = ResearchLink LinkInfo LinkVisibility
    | KeywordLink LinkInfo



-- Research categories:


type ResearchType
    = Teacher
    | Student
    | Lectorate
    | Kcpedia
    | Unknown


toString : ResearchType -> String
toString rtype =
    case rtype of
        Teacher ->
            "teacher"

        Student ->
            "student"

        Lectorate ->
            "lectorate"

        Kcpedia ->
            "kcpedia"

        Unknown ->
            "unkown"


type PublicationStatus
    = InProgress
    | Published
    | LocalPublication
    | Undecided


type Filter
    = Filter (AnySet String ResearchType)


toggleType : ResearchType -> Filter -> Filter
toggleType rtype (Filter f) =
    if Set.Any.member rtype f then
        Filter (Set.Any.remove rtype f)

    else
        Filter (Set.Any.insert rtype f)


isEnabled : ResearchType -> Filter -> Bool
isEnabled rtype (Filter f) =
    Set.Any.member rtype f


allFilter =
    Filter (Set.Any.fromList toString [ Student, Teacher, Lectorate, Kcpedia, Unknown ])


defaultFilter =
    Filter (Set.Any.fromList toString [ Student, Teacher, Lectorate ])



-- MAIN


main : Program () Model Msg
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


type DateFilter
    = Year Int
    | AnyYear


type alias Model =
    { researchList : List Research
    , years : Set.Set Year
    , keywordDict : KeywordDict
    , viewType : ViewType
    , tableState : Table.State
    , query : String
    , titleQuery : String
    , loadingStatus : LoadingStatus
    , filter : Filter
    , dateFilter : DateFilter
    , includeInternalResearch : ScopeFilter
    , includeUnpublishedResearch : Bool
    , includeKCPedia : Bool
    , dropdown : Dropdown.State
    }



-- initialize model


emptyModel : Model
emptyModel =
    { researchList = []
    , keywordDict = emptyKeywords
    , viewType = KeywordView
    , years = Set.empty
    , tableState = Table.sortBy "Modified" True
    , query = ""
    , titleQuery = ""
    , loadingStatus = Loading
    , filter = defaultFilter
    , dateFilter = AnyYear
    , includeInternalResearch = ShowInternal
    , includeUnpublishedResearch = True
    , includeKCPedia = False
    , dropdown = Dropdown.initialState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, getResearch )



-- helper funcs


kcpediaTag : String
kcpediaTag =
    "kcpedia"


allTags : List String
allTags =
    [ teacherTag, lectorateTag, kcpediaTag ]


isTag : String -> Bool
isTag =
    Util.flip List.member allTags


excludeTags : List String -> List String
excludeTags =
    List.filter (not << isTag)


isTeacherResearch : Research -> Bool
isTeacherResearch =
    List.member teacherTag << .keywords


isMusicResearch : Research -> Bool
isMusicResearch research =
    research.keywords |> List.member "music"


isLectorateResearch : Research -> Bool
isLectorateResearch =
    List.member lectorateTag << .keywords


isKcpediaResearch : Research -> Bool
isKcpediaResearch research =
    research |> .keywords |> List.member kcpediaTag


isPublished : PublicationStatus -> Bool
isPublished pubStatus =
    case pubStatus of
        Published ->
            True

        LocalPublication ->
            True

        Undecided ->
            False

        InProgress ->
            False



-- Logically, if something is not teacher nor lectorate, it must be student
-- Determine which kind of publication status the research has:


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
            "public"

        LocalPublication ->
            "KonCon portal members only!"

        Undecided ->
            "..."



-- not << (isTeacherResearch || isLectorateResearch)


hyperlink : Link -> Html Msg
hyperlink link =
    case link of
        ResearchLink l status ->
            viewResearchLink l status

        KeywordLink l ->
            viewKeywordLink l


viewResearchLink : LinkInfo -> LinkVisibility -> Html Msg
viewResearchLink link vis =
    case vis of
        PublicLink ->
            a
                [ href link.url
                , target "_blank"
                ]
                [ text link.title ]

        InternalLink ->
            a
                [ href link.url
                , target "_blank"
                ]
                [ keyIcon, text link.title ]


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
-- this decodes the JSON search result from the advanced search in RC:


decodeResearch : Decoder (List Research)
decodeResearch =
    Json.Decode.list entry



-- A single research item in the search results


entry : Decoder Research
entry =
    let
        researchType : Research -> Research
        researchType research =
            if isTeacherResearch research then
                { research | researchType = Teacher }

            else if isLectorateResearch research then
                { research | researchType = Lectorate }

            else if isKcpediaResearch research then
                { research | researchType = Kcpedia }

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

                "progress" ->
                    InProgress

                _ ->
                    Undecided
    in
    Json.Decode.map (researchType << researchPublicationStatus) <|
        (Json.Decode.succeed
            Research
            |> JDE.andMap (field "id" int)
            |> JDE.andMap (field "title" string)
            |> JDE.andMap (field "keywords" (Json.Decode.list string))
            |> JDE.andMap (field "created" string)
            |> JDE.andMap (field "author" <| field "name" string)
            |> JDE.andMap (succeed Unknown)
            |> JDE.andMap (maybe (field "issue" <| field "id" int))
            |> JDE.andMap (Json.Decode.map statusFromString (field "status" string))
            |> JDE.andMap (maybe (field "published" string))
            |> JDE.andMap (field "last-modified" decodePosix |> Json.Decode.map Just)
        )


decodePosix : Decoder Time.Posix
decodePosix =
    int |> Json.Decode.map (\x -> x * 1000 |> Time.millisToPosix)


posixToString : Time.Posix -> String
posixToString posix =
    let
        year =
            posix |> Time.toYear Time.utc

        month =
            posix
                |> Time.toMonth Time.utc
                |> (\m ->
                        case m of
                            Time.Jan ->
                                1

                            Time.Feb ->
                                2

                            Time.Mar ->
                                3

                            Time.Apr ->
                                4

                            Time.May ->
                                5

                            Time.Jun ->
                                6

                            Time.Jul ->
                                7

                            Time.Aug ->
                                8

                            Time.Sep ->
                                9

                            Time.Oct ->
                                10

                            Time.Nov ->
                                11

                            Time.Dec ->
                                12
                   )

        day =
            posix |> Time.toDay Time.utc
    in
    [ day, month, year ] |> List.map String.fromInt |> String.join "/"



-- UPDATE


type alias Year =
    Int


type Msg
    = Go
    | GotList (Result Http.Error (List Research))
    | SetQuery String
    | SetTitleQuery String
    | SetTableState Table.State
    | SetViewType ViewType
    | SetFilter ResearchType
    | ScopeFilter ScopeFilter
    | TogglePublishedFilter Bool
    | ToggleKCPedia Bool
    | DropdownMsg Dropdown.State
    | SetYearFilter Year


type ScopeFilter
    = ShowInternal
    | HideInternal



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
                    let
                        unique =
                            L.uniqueBy .id list
                    in
                    ( { model
                        | loadingStatus = Success
                        , researchList = unique
                        , keywordDict = fillKeywordsDict list
                        , years = allYears list
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

        SetTitleQuery newTitle ->
            ( { model | titleQuery = newTitle }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }, Cmd.none )

        SetViewType newType ->
            ( { model | viewType = newType, query = "", titleQuery = "" }, Cmd.none )

        SetFilter rtype ->
            let
                newView =
                    TableView

                --TODO, maybe smart view switch again
            in
            ( { model
                | filter = toggleType rtype model.filter
                , viewType = newView
              }
            , Cmd.none
            )

        ScopeFilter state ->
            ( { model | includeInternalResearch = state }, Cmd.none )

        TogglePublishedFilter includeUnpublished ->
            ( { model | includeUnpublishedResearch = includeUnpublished }, Cmd.none )

        ToggleKCPedia include ->
            ( { model | includeKCPedia = include }, Cmd.none )

        DropdownMsg newstate ->
            ( { model | dropdown = newstate }
            , Cmd.none
            )

        SetYearFilter year ->
            ( { model
                | dateFilter =
                    if year == -1 then
                        AnyYear

                    else
                        Year year
              }
            , Cmd.none
            )


makeLink : Research -> Link
makeLink research =
    let
        link =
            baseExpoUrl ++ String.fromInt (.id research)

        vis =
            case research.publicationStatus of
                LocalPublication ->
                    InternalLink

                _ ->
                    PublicLink
    in
    ResearchLink (LinkInfo (displayTitle research) link) vis


displayTitle : Research -> String
displayTitle research =
    research.title |> String.replace "&amp;" "&"


linkToUrl : Link -> String
linkToUrl link =
    case link of
        ResearchLink info _ ->
            info.url

        KeywordLink info ->
            info.url


attrsFromResearch : Research -> List (Attribute Msg)
attrsFromResearch research =
    case research.publicationStatus of
        LocalPublication ->
            [ class "local-publication" ]

        _ ->
            [ class "global-publication" ]


getDate : Research -> Maybe String
getDate research =
    case research.publicationStatus of
        InProgress ->
            Just "in progress"

        Undecided ->
            Just "unknown"

        Published ->
            research.publication

        LocalPublication ->
            research.publication


getModified : Research -> Maybe String
getModified =
    .lastChanged >> Maybe.map posixToString


config : Table.Config Research Msg
config =
    Table.customConfig
        { toId = String.fromInt << .id
        , toMsg = SetTableState
        , columns =
            [ typeColumn "Type" .researchType
            , linkColumn "Title" makeLink
            , Table.stringColumn "Author" .author
            , dateColumn "Status" getDate
            , dateColumn "Modified" getModified
            , Table.stringColumn "Keywords" (String.join ", " << List.map capitalize << excludeTags << .keywords)
            , Table.stringColumn "Visibility" (statusToString << .publicationStatus)
            ]
        , customizations = { defaultCustomizations | tableAttrs = [ class "table" ], rowAttrs = attrsFromResearch }
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Dropdown.subscriptions model.dropdown DropdownMsg



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
filterResearch (Filter filter) list =
    list |> List.filter (\r -> Set.Any.member r.researchType filter)


filterByYear : Year -> List Research -> List Research
filterByYear year research =
    List.filter
        (\rs ->
            case getYear rs.created of
                Just y ->
                    y == year

                Nothing ->
                    False
        )
        research


viewResearch : Model -> Html Msg
viewResearch model =
    let
        radioSwitchView =
            label []
                [ text "Switch view:"
                , div [ class "sm-1" ]
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

        publicInternalSwitch2 =
            label []
                [ div [ class "sm-1" ]
                    [ Checkbox.checkbox
                        [ Checkbox.checked (model.includeInternalResearch == ShowInternal)
                        , Checkbox.id "internal-switch"
                        , Checkbox.onCheck
                            (\checked ->
                                if checked then
                                    ScopeFilter ShowInternal

                                else
                                    ScopeFilter HideInternal
                            )
                        ]
                        (case model.includeInternalResearch of
                            ShowInternal ->
                                "Show internal"

                            HideInternal ->
                                "Show internal"
                        )
                    ]
                , case model.includeInternalResearch of
                    ShowInternal ->
                        text ""

                    HideInternal ->
                        text ""
                ]

        publishedSwitch =
            label [ class "sm-3" ]
                [ div []
                    [ Checkbox.checkbox
                        [ Checkbox.id "show-published-toggle"
                        , Checkbox.onCheck TogglePublishedFilter
                        , Checkbox.checked model.includeUnpublishedResearch
                        ]
                        "Include work in progress"
                    ]
                ]

        -- filterSwitch =
        --     let
        --         current =
        --             model.filter
        --     in
        --     label []
        --         [ text "Show research by:"
        --         , div [ class "mb-1" ]
        --             [ ButtonGroup.checkboxButtonGroup
        --                 [ ButtonGroup.small ]
        --                 [ ButtonGroup.checkboxButton
        --                     (isEnabled Teacher current)
        --                     [ Button.light, Button.onClick <| SetFilter Teacher ]
        --                     [ text "Teachers" ]
        --                 , Button.checkboxButton
        --                     (isEnabled Student current)
        --                     [ Button.light, Button.onClick <| SetFilter Student ]
        --                     [ text "Students" ]
        --                 , Button.checkboxButton
        --                     (isEnabled Lectorate current)
        --                     [ Button.light, Button.onClick <| SetFilter Lectorate ]
        --                     [ text "The lectorate 'Music, Education and Society'" ]
        --                 , Button.checkboxButton
        --                     (isEnabled Kcpedia current)
        --                     [ Button.light, Button.onClick <| SetFilter Kcpedia ]
        --                     [ text "KCPedia" ]
        --                 , Button.checkboxButton
        --                     (isEnabled Music current)
        --                     [ Button.light, Button.onClick <| SetFilter Music ]
        --                     [ text "Music" ]
        --                 ]
        --             ]
        --         ]
        filterSwitchToggles : Html Msg
        filterSwitchToggles =
            let
                current =
                    model.filter
            in
            div []
                [ text "Show research by:"
                , div [ class "sm-1" ]
                    [ Button.checkboxButton
                        (isEnabled Teacher current)
                        [ Button.onClick <| SetFilter Teacher ]
                        [ text "Teachers" ]
                    , Button.checkboxButton
                        (isEnabled Student current)
                        [ Button.onClick <| SetFilter Student ]
                        [ text "Students" ]
                    , Button.checkboxButton
                        (isEnabled Lectorate current)
                        [ Button.onClick <| SetFilter Lectorate ]
                        [ text "The lectorate 'Music, Education and Society'" ]
                    , Button.checkboxButton
                        (isEnabled Kcpedia current)
                        [ Button.onClick <| SetFilter Kcpedia ]
                        [ text "KCPedia" ]
                    ]
                ]

        dateFilterToString dateFilterState =
            case dateFilterState of
                AnyYear ->
                    "any"

                Year x ->
                    String.fromInt x

        years =
            model.years |> Set.toList

        yearOption y =
            Dropdown.buttonItem [ onClick (SetYearFilter y) ] [ text <| String.fromInt y ]

        yearFilter =
            label []
                [ text "filter by year"
                , div [ class "sm-1" ]
                    [ Dropdown.dropdown
                        model.dropdown
                        { options = [ Dropdown.alignMenuRight ]
                        , toggleMsg = DropdownMsg
                        , toggleButton =
                            Dropdown.toggle [] [ text (model.dateFilter |> dateFilterToString) ]
                        , items =
                            Dropdown.buttonItem [ onClick (SetYearFilter -1) ] [ text "Any year" ]
                                :: (List.map yearOption years |> List.reverse)
                        }
                    ]
                ]

        filtered =
            -- Student/Teacher etc..
            filterResearch model.filter model.researchList

        filteredByYear =
            case model.dateFilter of
                AnyYear ->
                    filtered

                Year y ->
                    filterByYear y filtered

        filteredOnStatus =
            -- publication status
            case model.includeInternalResearch of
                ShowInternal ->
                    filteredByYear

                HideInternal ->
                    List.filter
                        (\research ->
                            case research.publicationStatus of
                                LocalPublication ->
                                    False

                                _ ->
                                    True
                        )
                        filteredByYear

        filteredOnPublication =
            if model.includeUnpublishedResearch then
                filteredOnStatus

            else
                filteredOnStatus |> List.filter (\r -> isPublished r.publicationStatus)

        content =
            case model.viewType of
                TableView ->
                    div
                        []
                        (viewResearchList model.tableState model.titleQuery model.query filteredOnPublication)

                KeywordView ->
                    let
                        filteredDict =
                            fillKeywordsDict <| filteredOnStatus
                    in
                    div [ id "keywords" ]
                        [ renderKeywords model.query filteredDict ]

        headers =
            div [ class "headers" ]
                [ h1 [] [ text "Research Results" ]
                , h4 [] [ text "Royal Conservatoire in The Hague" ]
                ]
    in
    Grid.container [ id "top" ]
        [ headers
        , Grid.row [  ]
            [ Grid.col [ Col.xs8, Col.attrs [ Border.all, Spacing.p2, Border.rounded, Spacing.mr2 ] ] [ filterSwitchToggles ]
            , Grid.col [ Col.xs3, Col.attrs [ Border.all, Spacing.p2, Border.rounded ] ] [ publishedSwitch, publicInternalSwitch2 ]
            ]
        , Grid.row []
            [ Grid.col [] [ radioSwitchView ]
            ]
        , content
        ]


viewResearchList : Table.State -> String -> String -> List Research -> List (Html Msg)
viewResearchList tableState titleQuery query researchList =
    let
        lowerQuery =
            String.toLower query

        lowerTitle =
            String.toLower titleQuery

        acceptableResearch =
            List.filter (String.contains lowerTitle << String.toLower << .title) <|
                List.filter (String.contains lowerQuery << String.toLower << .author) researchList

        statistics =
            List.length acceptableResearch |> String.fromInt |> (\numString -> numString ++ " results")
    in
    [ Form.form [ class "form-inline" ]
        [ Form.group []
            [ input
                [ class "form-control"
                , placeholder "Search by author"
                , onInput SetQuery
                , style "margin" ".5rem 0"
                ]
                []
            , input
                [ Display.inline
                , class "form-control"
                , Spacing.m1
                , placeholder "Search by title"
                , onInput SetTitleQuery
                , style "margin" ".5rem 0"
                ]
                []
            ]
        ]
    , p [ class "table-statistics" ] [ text statistics ]
    , div
        [ class "table-responsive" ]
        [ Table.view config tableState acceptableResearch
        ]
    ]


viewShortMeta : Research -> Html Msg
viewShortMeta research =
    li (class "research-meta" :: attrsFromResearch research)
        [ p [ class "research-meta-title" ]
            [ hyperlink (makeLink research)
            ]
        , p
            [ class "research-meta-status", title "publication status" ]
            [ text <| statusToString research.publicationStatus ]
        , p [ class "research-meta-author" ]
            [ text <| research.author
            , span [ class "research-meta-type" ] [ text <| " " ++ (parenthesize <| typeToString research.researchType) ]
            ]
        ]


dateColumn : String -> (Research -> Maybe String) -> Column Research msg
dateColumn name toCreated =
    let
        sortableDateString mstr =
            mstr
                |> Maybe.withDefault "?"
                |> String.split "/"
                |> List.reverse
                |> String.join "/"
    in
    Table.customColumn
        { name = name
        , viewData = toCreated >> Maybe.withDefault "no date"
        , sorter =
            Table.increasingOrDecreasingBy <|
                (toCreated >> sortableDateString)
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

        Kcpedia ->
            "KCPedia"

        Unknown ->
            "Unknown"


getTitle : Link -> String
getTitle l =
    case l of
        ResearchLink i _ ->
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


renderKeywords : String -> KeywordDict -> Html Msg
renderKeywords query dict =
    let
        allKeys =
            keys dict

        lowerQuery =
            String.toLower query

        acceptableKeys =
            List.filter (String.contains lowerQuery << String.toLower) allKeys

        sortedKeys : List String
        sortedKeys =
            List.sort acceptableKeys

        queryForm =
            Form.form [ class "form-inline" ]
                [ Form.group []
                    [ input
                        [ class "form-control"
                        , placeholder "Search keywords"
                        , onInput SetQuery
                        , style "margin" ".5rem 0"
                        ]
                        []
                    ]
                ]
    in
    div [] <|
        [ queryForm
        , researchByKeywordList (excludeTags sortedKeys) dict
        ]


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


keyIcon : Html Msg
keyIcon =
    Html.img
        [ class "key-icon"
        , src "./key.svg"
        , width 25
        , height 25
        , title "this research is only accessible for students and staff"
        ]
        []


getYear : String -> Maybe Year
getYear str =
    str
        |> String.split "/"
        |> (\ls ->
                case ls of
                    _ :: _ :: y :: _ ->
                        String.toInt y

                    _ ->
                        Nothing
           )


allYears : List Research -> Set.Set Year
allYears lst =
    let
        years =
            lst |> List.map (\r -> r.created |> getYear) |> List.filterMap identity
    in
    List.foldl Set.insert Set.empty years
