module Graph
    exposing
        ( Graph(..)
        , Msg
        , Model
        , Title
        , GraphData
        , defaultModel
        , update
        , plotRuns
        , validGraphs
        , chooseGraphType
        , assignColors
        , buildGroups
        )

import Data exposing (Run, Page)
import Date
import Dict exposing (Dict)
import Html exposing (..)
import List.Extra as EList
import Plot exposing (..)
import Round exposing (roundNum)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr exposing (stroke, strokeDasharray, r, fill, strokeWidth)
import Svg.Attributes exposing (stroke)
import Svg.Events as SvgEvent


{-| Creates a Title alias to easier identify a string it refers to a graph title
-}
type alias Title =
    String


{-| Creates a Title alias to easier identify a string it refers to a graph
legend in the Y axis
-}
type alias YLegend =
    String


{-| Creates a Title alias to easier identify a string it refers to a graph
legend in the X axis
-}
type alias XLegend =
    String


{-| Creates a Title alias to easier identify a function as a data getter
to be used for coordinates in the X axis.
-}
type alias XValueGetter =
    Run -> Float


{-| Creates a Title alias to easier identify a function as a data getter
to be used for coordinates in the Y axis.
-}
type alias YValueGetter =
    Run -> Float


{-| Represents all the possible graph types this application is capable of
rendering.
-}
type Graph
    = Scatter XLegend YLegend XValueGetter YValueGetter
    | Timeline YLegend YValueGetter


{-| Quick alias to avoid typing when representing a pair of (color, rundata)
This pair is used when plotting the graph and the color is used to fill the circle
for each point in the plot.
-}
type alias GraphData =
    ( String, Run )


type Msg
    = Hover (Maybe Point)


type alias Model =
    { hovered : Maybe Point }


defaultModel : Model
defaultModel =
    Model Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update (Hover point) model =
    -- When the mouse pointer is over or has left a point in the graph, we need to record that change
    -- so that we can render or hide the selection hints.
    ( { model | hovered = point }, Cmd.none )


chooseGraphType : Title -> List Run -> ( Maybe Title, Maybe Int )
chooseGraphType name runs =
    -- If the user choses to change the graph type we need to first extract the graph definition for
    -- the selection, and then store in the model the graph type, so it can be rendered in the view.
    let
        graphType =
            validGraphs
                |> List.filter (\( title, _ ) -> title == name)
                |> List.head

        -- When comparing diffent runs by concurrency level, we need to pick an
        -- appropriate concurrency level for the first showing of the graph.
        -- In this case just selecting the maximum concurrency may be just good enough.
        appropriateConcurrency runs =
            runs
                |> List.map (.run >> .concurrency)
                |> List.maximum
    in
        case graphType of
            Nothing ->
                ( Nothing, Nothing )

            Just ( t, Timeline _ _ ) ->
                ( Just t, appropriateConcurrency runs )

            Just ( t, _ ) ->
                ( Just t, Nothing )


{-| A (name, graph) values list with all the plots that can be selected. Many
are variations of the same type of graph, only varying in the columns they are
displaying
-}
validGraphs : List ( Title, Graph )
validGraphs =
    let
        -- Let's declare a base definition for a Scatter plot that many other plots can
        -- extend with additional options. (.run >> .concurrency) means first get the run
        -- field and then get its concurrency field
        baseScatter =
            Scatter "Concurrency" "Resp. Time (s)" (.run >> .concurrency >> toFloat)
    in
        [ ( "Mean Time / Concurrency", baseScatter (.stats >> .meanTime) )
        , ( "Percentile 95 / Concurrency", baseScatter (.stats >> .quantile95) )
        , ( "Fastest Time / Concurrency", baseScatter (.stats >> .minTime) )
        , ( "Slowest Time / Concurrency", baseScatter (.stats >> .maxTime) )
        , ( "Aggregated Time / Concurrency", baseScatter (.stats >> .totalTime) )
        , ( "Variance / Concurrency", baseScatter (.stats >> .variance) )
        , ( "Mean Time Comparison", Timeline "Resp. Time (s)" (.stats >> .meanTime) )
        , ( "Percentile 95 Comparison", Timeline "Resp. Time (s)" (.stats >> .quantile95) )
        , ( "Slowest Time Comparison", Timeline "Resp. Time (s)" (.stats >> .maxTime) )
        , ( "Variance Comparison", Timeline "Resp. Time (s)" (.stats >> .variance) )
        ]


{-| Returns the list of colors that can be assigned to the groups.
Unfortunately I only found a reduced list of nice colors to use, so
we cannot display more runs than colors in this list!
-}
allColors : List String
allColors =
    [ "#ff9edf"
    , "#cfd8ea"
    , "#77DD77"
    , "#AEC6CF"
    , "#CB99C9"
    , "#B39EB5"
    , "#FFB347"
    , "#FF6961"
    , "#836953"
    , "#779ECB"
    , "#FDFD96"
    , "#F49AC2"
    , "#CFCFC4"
    , "#B19CD9"
    , "#03C03C"
    , "#a0344e"
    , "#d7909b"
    , "#ddd4d3"
    , "#c8d7ca"
    , "#8cac90"
    , "#492212"
    , "#b06e40"
    , "#d4a06e"
    , "#e8ceb5"
    , "#363634"
    , "#524636"
    , "#ac7330"
    , "#b19a78"
    , "#d1c5ab"
    , "#617373"
    ]


{-| Finds all runs with the same group name and assigns them a color based on this
similarity. Returns the pair (color, run) in a list.
-}
assignColors : List Run -> List GraphData
assignColors runs =
    runs
        |> buildGroups
        |> EList.zip allColors
        |> List.map (\( color, ( _, runGroup ) ) -> List.map (\r -> ( color, r )) runGroup)
        |> List.concat


{-| Returns a List of pairs where the first in the pair is the groupName
and the secnd is the list of runs having the same groupName.
-}
buildGroups : List Run -> List ( String, List Run )
buildGroups runs =
    let
        insertOrUpdate run currentList =
            case currentList of
                Nothing ->
                    Just [ run ]

                Just l ->
                    Just (run :: l)

        buildDict ( groupName, run ) dict =
            Dict.update groupName (insertOrUpdate run) dict
    in
        runs
            |> List.map (\r -> ( r.run.groupName, r ))
            |> List.foldl buildDict Dict.empty
            |> Dict.toList


graphDefinition : Title -> Maybe Graph
graphDefinition title =
    validGraphs
        |> List.filter (\( t, _ ) -> title == t)
        |> List.map Tuple.second
        |> List.head


assembleRunData : Maybe (Dict Int Page) -> List GraphData -> List GraphData
assembleRunData pages runs =
    case pages of
        Nothing ->
            runs

        Just pageStats ->
            runs
                |> List.filterMap
                    (\( color, r ) ->
                        case Dict.get r.run.id pageStats of
                            Nothing ->
                                Nothing

                            Just { stats } ->
                                -- Replace the run stats with the page stats
                                Just ( color, { r | stats = stats } )
                    )


plotRuns :
    Model
    -> Title
    -> List Run
    -> List String
    -> Maybe Int
    -> Maybe (Dict Int Page)
    -> Html Msg
plotRuns { hovered } graph runs filteredGroups concurrencyComparison pageStats =
    let
        selectRunData graphData =
            assembleRunData pageStats graphData
    in
        case graphDefinition graph of
            Just (Scatter xLegend yLegend xGetter yGetter) ->
                basicSeries hovered
                    xLegend
                    yLegend
                    [ scatterPlot xGetter yGetter hovered ]
                    (runs |> assignColors |> selectRunData |> filterGroups filteredGroups)

            Just (Timeline yLegend yGetter) ->
                let
                    level =
                        Maybe.withDefault 0 concurrencyComparison

                    runData =
                        runs
                            |> List.filter (\r -> r.run.concurrency == level)

                    dates =
                        runData
                            |> List.map (.run >> .created >> Date.toTime)
                            |> EList.unique
                            |> List.sort
                            |> List.indexedMap (\index date -> ( date, toFloat index ))
                            |> Dict.fromList

                    dateGetter d =
                        dates
                            |> Dict.get (Date.toTime (d.run.created))
                            |> Maybe.withDefault 0
                in
                    basicSeries hovered
                        ""
                        yLegend
                        [ linePlot dateGetter yGetter hovered ]
                        (runData |> assignColors |> selectRunData)

            Nothing ->
                Debug.crash ("got invalid graph title: " ++ graph)


basicSeries :
    Maybe Point
    -> XLegend
    -> YLegend
    -> List (Series data msg)
    -> data
    -> Html msg
basicSeries hovered xLegend yLegend =
    viewSeriesCustom
        { defaultSeriesPlotCustomizations
            | horizontalAxis = rangeFrameAxis hovered (.x >> roundNum 3)
            , margin = { top = 20, bottom = 20, left = 150, right = 40 }
            , toDomainLowest = \y -> y - 0.3
            , junk = legend xLegend yLegend
        }


legend : XLegend -> YLegend -> PlotSummary -> List (JunkCustomizations msg)
legend xLegend yLegend summary =
    let
        verticalCenter =
            summary.y.dataMax / 2
    in
        [ junk (title yLegend) (summary.x.dataMin - 30) verticalCenter
        , junk (title xLegend) summary.x.max (summary.y.dataMin - (summary.y.dataMin - summary.y.min) / 1.3)
        ]


title : String -> Svg msg
title txt =
    viewLabel
        [ fill blueStroke
        , SvgAttr.style "text-anchor: end; font-style: italic; font-size:10px"
        ]
        txt


blueStroke : String
blueStroke =
    "#cfd8ea"


filterGroups : List String -> List GraphData -> List GraphData
filterGroups filters data =
    case filters of
        [] ->
            data

        _ ->
            data
                |> List.filter (\( _, d ) -> List.member d.run.groupName filters)


scatterPlot :
    XValueGetter
    -> YValueGetter
    -> Maybe Point
    -> Series (List GraphData) Msg
scatterPlot xGetter yGetter hinting =
    { axis = rangeFrameAxis hinting (.y >> roundNum 3)
    , interpolation = None
    , toDataPoints =
        List.sortBy (Tuple.second >> .run >> .created >> Date.toTime)
            >> List.map (rangeFrameHintDot xGetter yGetter hinting)
    }


circle : String -> Float -> Float -> Svg Msg
circle color x y =
    Svg.circle
        [ r "5"
        , stroke "transparent"
        , strokeWidth "3px"
        , fill color
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
    -> GraphData
    -> DataPoint Msg
rangeFrameHintDot xGetter yGetter hinted ( color, run ) =
    let
        x =
            xGetter run

        y =
            yGetter run
    in
        { view = Just (circle color x y)
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


linePlot :
    XValueGetter
    -> YValueGetter
    -> Maybe Point
    -> Series (List GraphData) Msg
linePlot xGetter yGetter hinting =
    { axis = hintedAxisAtMin hinting
    , interpolation = Monotone Nothing [ stroke blueStroke ]
    , toDataPoints = List.map (rangeFrameHintDot xGetter yGetter hinting)
    }


hintedAxisAtMin : Maybe Point -> Axis
hintedAxisAtMin hinted =
    customAxis <|
        \summary ->
            { position = Basics.min
            , axisLine = Nothing
            , ticks = List.map simpleTick (decentPositions summary)
            , labels =
                case hinted of
                    Nothing ->
                        (List.map simpleLabel (decentPositions summary))
                            ++ [ roundNum 3 summary.dataMax |> simpleLabel ]

                    Just _ ->
                        hintLabel hinted (.y >> roundNum 3)
            , flipAnchor = False
            }
