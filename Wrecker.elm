module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg.Attributes as SvgAttr exposing (stroke, strokeDasharray, r, fill, strokeWidth)
import Svg.Events as SvgEvent
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Plot exposing (..)
import Json.Decode.Pipeline as Pipeline
import Date
import Svg exposing (Svg)
import Svg.Attributes exposing (stroke)
import Round exposing (roundNum)
import Dict exposing (Dict)
import Tuple


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Stats =
    { meanTime : Float
    , successHits : Int
    , serverErrorHits : Int
    , maxTime : Float
    , minTime : Float
    , userErrorHits : Int
    , totalTime : Float
    , failedHits : Int
    , variance : Float
    , hits : Int
    }


type alias RunInfo =
    { created : Date.Date
    , concurrency : Int
    , id : Int
    , match : String
    }


type alias Run =
    { pages : List String
    , stats : Stats
    , run : RunInfo
    }


type alias Model =
    { runTitles : List String
    , searchField : String
    , runs : List Run
    , graph : Title
    , hovered : Maybe Point
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { runTitles = []
            , searchField = ""
            , runs = []
            , graph = "Mean Time / Concurrency"
            , hovered = Nothing
            }
    in
        ( model, Http.send LoadRunTitles (getRuns "") )


type Msg
    = SearchFieldUpdated String
    | SearchButtonClicked
    | LoadRunTitles (Result Http.Error (List RunInfo))
    | LoadRunList (Result Http.Error (List RunInfo))
    | LoadRunStats (Result Http.Error Run)
    | Hover (Maybe Point)
    | RunTitleClicked String
    | ChangeGraphType String


type alias Title =
    String


type alias YLegend =
    String


type alias XLegend =
    String


type alias XValueGetter =
    Run -> Float


type alias YValueGetter =
    Run -> Float


type Graph
    = Scatter XLegend YLegend XValueGetter YValueGetter


validGraphs : List ( Title, Graph )
validGraphs =
    let
        baseScatter =
            Scatter "Concurrency" "Resp. Time (s)" (.run >> .concurrency >> toFloat)
    in
        [ ( "Mean Time / Concurrency", baseScatter (.stats >> .meanTime) )
        , ( "Fastest Time / Concurrency", baseScatter (.stats >> .minTime) )
        , ( "Slowest Time / Concurrency", baseScatter (.stats >> .maxTime) )
        , ( "Aggregated Time / Concurrency", baseScatter (.stats >> .totalTime) )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchFieldUpdated name ->
            ( { model | searchField = name }, Cmd.none )

        SearchButtonClicked ->
            ( { model | runs = [] }, Http.send LoadRunList (getRuns model.searchField) )

        RunTitleClicked name ->
            ( { model | searchField = name, runs = [] }, Http.send LoadRunList (getRuns name) )

        LoadRunTitles (Err _) ->
            ( model, Cmd.none )

        LoadRunTitles (Ok runs) ->
            ( { model | runTitles = extractTitles runs }, Cmd.none )

        LoadRunList (Err _) ->
            ( model, Cmd.none )

        LoadRunList (Ok runs) ->
            let
                toRequest { id } =
                    Http.send LoadRunStats (getSingleRun id)

                requests =
                    List.map toRequest runs
            in
                ( model, Cmd.batch requests )

        LoadRunStats (Err _) ->
            ( model, Cmd.none )

        LoadRunStats (Ok run) ->
            ( { model | runs = run :: model.runs }, Cmd.none )

        Hover point ->
            ( { model | hovered = point }, Cmd.none )

        ChangeGraphType name ->
            let
                defaultType =
                    validGraphs
                        |> List.head
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault ""

                newType =
                    validGraphs
                        |> List.filter (\( title, _ ) -> title == name)
                        |> List.map Tuple.first
                        |> List.head
                        |> Maybe.withDefault defaultType
            in
                ( { model | graph = newType }, Cmd.none )


getRuns : String -> Http.Request (List RunInfo)
getRuns name =
    Http.get ("http://localhost:3000/runs?match=" ++ (Http.encodeUri name))
        (Decode.field "runs" (Decode.list decodeRunInfo))


getSingleRun : Int -> Http.Request Run
getSingleRun id =
    Http.get ("http://localhost:3000/runs/" ++ toString id) decodeRun


extractTitles : List RunInfo -> List String
extractTitles runs =
    runs
        |> List.map (\{ match } -> ( match, 1 ))
        |> Dict.fromList
        |> Dict.keys


view : Model -> Html Msg
view model =
    div []
        [ node "link" [ rel "stylesheet", href "/assets/main.css" ] []
        , div [ class "view" ]
            [ div [ class "view--left" ] [ leftPanel model.searchField model.runTitles ]
            , div [ class "view--right" ] [ rightPanel model ]
            ]
        ]


leftPanel : String -> List String -> Html Msg
leftPanel defaultTitle runTitles =
    header [ class "view-header" ]
        [ h1 [ class "view-header__title" ] [ text "Wrecker-UI" ]
        , input
            [ type_ "text"
            , name "run_name"
            , placeholder "Search Run"
            , value defaultTitle
            , onEnter SearchButtonClicked
            , onInput SearchFieldUpdated
            ]
            []
        , button [ onClick SearchButtonClicked ] [ text "go" ]
        , ul [ class "view-header__runs" ] (List.map runListItem runTitles)
        ]


runListItem : String -> Html Msg
runListItem title =
    li [ onClick (RunTitleClicked title) ] [ a [ href "#" ] [ text title ] ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        on "keydown" (keyCode |> Decode.andThen isEnter)


rightPanel : Model -> Html Msg
rightPanel { hovered, runs, graph } =
    case runs of
        [] ->
            text ""

        _ ->
            div [ class "view-plot view-plot__closed" ]
                [ div [ class "view-plot--left" ] [ plotRuns graph hovered runs ]
                , div [ class "view-plot--right" ] [ rightMostPanel graph ]
                ]


rightMostPanel : Title -> Html Msg
rightMostPanel current =
    div []
        [ select [ onChange ChangeGraphType ] (List.map (renderGraphItem current) validGraphs) ]


onChange : (String -> Msg) -> Attribute Msg
onChange msg =
    let
        action =
            Decode.at [ "target", "value" ] Decode.string
                |> Decode.map msg
    in
        on "change" action


renderGraphItem : Title -> ( Title, Graph ) -> Html Msg
renderGraphItem current ( title, _ ) =
    option [ value title, selected (current == title) ] [ text title ]


pinkStroke : String
pinkStroke =
    "#ff9edf"


blueStroke : String
blueStroke =
    "#cfd8ea"


graphDesc : Title -> Maybe Graph
graphDesc title =
    validGraphs
        |> List.filter (\( t, _ ) -> title == t)
        |> List.map Tuple.second
        |> List.head


plotRuns : Title -> Maybe Point -> List Run -> Html Msg
plotRuns graphTitle hovered runs =
    case graphDesc graphTitle of
        Just (Scatter xLegend yLegend xGetter yGetter) ->
            viewSeriesCustom
                { defaultSeriesPlotCustomizations
                    | horizontalAxis = rangeFrameAxis hovered (.x >> roundNum 3)
                    , margin = { top = 20, bottom = 20, left = 150, right = 40 }
                    , toDomainLowest = \y -> y - 0.3
                    , junk = legend xLegend yLegend
                }
                [ scatterPlot xGetter yGetter hovered ]
                runs

        Nothing ->
            Debug.crash ("got invalid graph title: " ++ graphTitle)


legend : XLegend -> YLegend -> PlotSummary -> List (JunkCustomizations Msg)
legend xLegend yLegend summary =
    let
        verticalCenter =
            summary.y.dataMax / 2
    in
        [ junk (title yLegend) (summary.x.dataMin - 30) verticalCenter
        , junk (title xLegend) summary.x.max (summary.y.dataMin - (summary.y.dataMin - summary.y.min) / 1.3)
        ]


title : String -> Svg Msg
title txt =
    viewLabel
        [ fill blueStroke
        , SvgAttr.style "text-anchor: end; font-style: italic; font-size:10px"
        ]
        txt


scatterPlot :
    XValueGetter
    -> YValueGetter
    -> Maybe Point
    -> Series (List Run) Msg
scatterPlot xGetter yGetter hinting =
    { axis = rangeFrameAxis hinting (.y >> roundNum 3)
    , interpolation = None
    , toDataPoints = List.map (rangeFrameHintDot xGetter yGetter hinting)
    }


circle : Float -> Float -> Svg Msg
circle x y =
    Svg.circle
        [ r "5"
        , stroke "transparent"
        , strokeWidth "3px"
        , fill pinkStroke
        , SvgEvent.onMouseOver (Hover (Just { x = x, y = y }))
        , SvgEvent.onMouseOut (Hover Nothing)
        ]
        []


flashyLine : Float -> Float -> Point -> Maybe (AxisSummary -> LineCustomizations)
flashyLine x y hinted =
    if hinted.x == x && hinted.y == y then
        Just (fullLine [ stroke "#a3a3a3", strokeDasharray "2, 10" ])
    else
        Nothing


rangeFrameHintDot :
    XValueGetter
    -> YValueGetter
    -> Maybe Point
    -> Run
    -> DataPoint Msg
rangeFrameHintDot xGetter yGetter hinted run =
    let
        x =
            xGetter run

        y =
            yGetter run
    in
        { view = Just (circle x y)
        , xLine = Maybe.andThen (flashyLine x y) hinted
        , yLine = Maybe.andThen (flashyLine x y) hinted
        , xTick = Just (simpleTick x)
        , yTick = Just (simpleTick y)
        , hint = Nothing
        , x = x
        , y = y
        }


rangeFrameAxis : Maybe Point -> (Point -> Float) -> Axis
rangeFrameAxis hinted toValue =
    customAxis <|
        \summary ->
            { position = closestToZero
            , axisLine = Nothing
            , ticks = List.map (roundNum 3 >> simpleTick) [ summary.dataMin, summary.dataMax ]
            , labels =
                case hinted of
                    Nothing ->
                        List.map (roundNum 3 >> simpleLabel) [ summary.dataMin, summary.dataMax ]

                    Just _ ->
                        hintLabel hinted toValue
            , flipAnchor = False
            }


hintLabel : Maybe Point -> (Point -> Float) -> List LabelCustomizations
hintLabel hinted toValue =
    hinted
        |> Maybe.map (toValue >> simpleLabel >> List.singleton)
        |> Maybe.withDefault []



----------------
--- JSON decode
----------------


decodeRun : Decode.Decoder Run
decodeRun =
    Pipeline.decode Run
        |> Pipeline.required "pages" (Decode.list Decode.string)
        |> Pipeline.required "stats" decodeStats
        |> Pipeline.required "run" decodeRunInfo


decodeStats : Decode.Decoder Stats
decodeStats =
    Pipeline.decode Stats
        |> Pipeline.required "meanTime" Decode.float
        |> Pipeline.required "successHits" Decode.int
        |> Pipeline.required "serverErrorHits" Decode.int
        |> Pipeline.required "maxTime" Decode.float
        |> Pipeline.required "minTime" Decode.float
        |> Pipeline.required "userErrorHits" Decode.int
        |> Pipeline.required "totalTime" Decode.float
        |> Pipeline.required "failedHits" Decode.int
        |> Pipeline.required "variance" Decode.float
        |> Pipeline.required "hits" Decode.int


decodeRunInfo : Decode.Decoder RunInfo
decodeRunInfo =
    Pipeline.decode RunInfo
        |> Pipeline.required "created" (Decode.string |> Decode.andThen decodeDateTime)
        |> Pipeline.required "concurrency" Decode.int
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "match" Decode.string


decodeDateTime : String -> Decode.Decoder Date.Date
decodeDateTime datetimeString =
    case Date.fromString datetimeString of
        Err e ->
            Decode.fail e

        Ok date ->
            Decode.succeed date
