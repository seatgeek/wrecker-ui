module Graph
    exposing
        ( Graph(..)
        , Msg
        , Model
        , defaultModel
        , update
        , view
        , validGraphs
        , chooseGraphType
        )

import Data exposing (Run, Page, RunGroup)
import Dict exposing (Dict)
import Html exposing (..)
import LineGraph
import Graph.Types exposing (..)


{-| Represents all the possible graph types this application is capable of
rendering.
-}
type Graph
    = Scatter XLegend XValueGetter YLegend YValueGetter
    | Timeline YLegend YValueGetter


type Msg
    = LineGraphMsg LineGraph.Msg


type alias Model =
    { lineGraphModel : LineGraph.Model }


defaultModel : Model
defaultModel =
    Model (LineGraph.defaultModel)


update : Msg -> Model -> ( Model, Cmd Msg )
update (LineGraphMsg msg) model =
    let
        ( lineGraphModel, cmd ) =
            LineGraph.update msg model.lineGraphModel
    in
        ( { model | lineGraphModel = lineGraphModel }, Cmd.map LineGraphMsg cmd )


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
        legendLabel (YValueGetter getter) append =
            getter >> round1000 >> toString >> flip (++) append

        concurrency =
            .run >> .concurrency >> toFloat

        concurrencyAxis =
            XValueGetter concurrency

        concurrencyLegend =
            XLegend "Concurrency" (concurrency >> toString >> (++) "Concurrency: ")

        timeLegend getter =
            YLegend "Resp." (legendLabel getter "secs")

        totalLegend getter =
            YLegend "Total" (legendLabel getter "reqs")

        baseScatter getter =
            Scatter concurrencyLegend concurrencyAxis (timeLegend getter) getter

        totalScatter getter =
            Scatter concurrencyLegend concurrencyAxis (totalLegend getter) getter

        throughput getter =
            Scatter concurrencyLegend
                concurrencyAxis
                (YLegend "Users/sec" (legendLabel getter "u/s"))
                getter

        stats getter =
            YValueGetter (.stats >> getter)

        intStats getter =
            stats (getter >> toFloat)

        throughputGetter =
            YValueGetter (\s -> toFloat s.run.concurrency / s.stats.meanTime)
    in
        [ ( "Mean Time / Concurrency", baseScatter (stats .meanTime) )
        , ( "Percentile 95 / Concurrency", baseScatter (stats .quantile95) )
        , ( "Total Requests / Concurrency", totalScatter (intStats .hits) )
        , ( "Failed Requests / Concurrency", totalScatter (intStats (\s -> s.serverErrorHits + s.failedHits)) )
        , ( "2xx Requests / Concurrency", totalScatter (intStats .successHits) )
        , ( "4xx Requests / Concurrency", totalScatter (intStats .userErrorHits) )
        , ( "Throughput / Concurrency", throughput throughputGetter )
        , ( "Fastest Time / Concurrency", baseScatter (stats .minTime) )
        , ( "Slowest Time / Concurrency", baseScatter (stats .maxTime) )
        , ( "Aggregated Time / Concurrency", baseScatter (stats .totalTime) )
        , ( "Variance / Concurrency", baseScatter (stats .variance) )
        ]


graphDefinition : Title -> Maybe Graph
graphDefinition title =
    validGraphs
        |> List.filter (\( t, _ ) -> title == t)
        |> List.map Tuple.second
        |> List.head


view :
    Model
    -> Title
    -> Dict Int RunGroup
    -> Dict Int (List Run)
    -> Html Msg
view model graph allGroups runs =
    case graphDefinition graph of
        Just (Scatter xLegend xGetter yLegend yGetter) ->
            Html.map LineGraphMsg <|
                LineGraph.view
                    (LineGraph.Config xLegend xGetter yLegend yGetter)
                    model.lineGraphModel
                    allGroups
                    runs

        _ ->
            Debug.crash ("got invalid graph title: " ++ graph)



-- UTILS


round1000 : Float -> Float
round1000 float =
    toFloat (round (float * 1000)) / 1000
