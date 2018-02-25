module LineGraph exposing (Config, Msg, Model, defaultModel, update, view)

import Html
import LineChart
import LineChart.Junk as Junk
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Junk as Junk
import LineChart.Dots as Dots
import LineChart.Grid as Grid
import LineChart.Dots as Dots
import LineChart.Line as Line
import LineChart.Colors as Colors
import LineChart.Events as Events
import LineChart.Legends as Legends
import LineChart.Container as Container
import LineChart.Interpolation as Interpolation
import LineChart.Axis.Intersection as Intersection
import Data exposing (Run, RunGroup)
import Dict exposing (Dict)
import Graph.Types exposing (..)
import Graph.Coloring exposing (pickColor)


-- MODEL


type alias AllRunGroups =
    Dict Int RunGroup


type alias Data =
    Dict Int (List Run)


type alias Model =
    { hinted : List Run
    }


type alias Config =
    { xLegend : XLegend
    , xGetter : XValueGetter
    , yLegend : YLegend
    , yGetter : YValueGetter
    }



-- API


defaultModel : Model
defaultModel =
    Model []


setHint : List Run -> Model -> Model
setHint hinted model =
    { model | hinted = hinted }



-- UPDATE


type Msg
    = Hint (List Run)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hint points ->
            model
                |> setHint points
                |> addCmd Cmd.none


addCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
addCmd cmd model =
    ( model, Cmd.none )



-- VIEW


view : Config -> Model -> AllRunGroups -> Data -> Html.Html Msg
view config model allRunGroups data =
    Html.div []
        [ LineChart.viewCustom (chartConfig config model) (createSeries allRunGroups data)
        ]


createSeries allRunGroups allRuns =
    let
        getRunGroup groupId =
            Dict.get groupId allRunGroups
    in
        allRuns
            |> Dict.toList
            |> List.map (Tuple.mapFirst getRunGroup >> uncurry createLineSerie)


createLineSerie runGroup runData =
    case runGroup of
        Just group ->
            LineChart.line (pickColor group.id) Dots.circle group.title runData

        Nothing ->
            LineChart.line Colors.transparent Dots.circle "" []



-- CHART CONFIG


chartConfig : Config -> Model -> LineChart.Config Run Msg
chartConfig config model =
    let
        (XLegend xTitle xTitler) =
            config.xLegend

        (XValueGetter xGetter) =
            config.xGetter

        (YLegend yTitle yTitler) =
            config.yLegend

        (YValueGetter yGetter) =
            config.yGetter
    in
        { y = Axis.default 800 yTitle yGetter
        , x = Axis.default 1200 xTitle xGetter
        , container = containerConfig
        , interpolation = Interpolation.monotone
        , intersection = Intersection.default
        , legends = Legends.none
        , events = Events.hoverMany Hint
        , junk = Junk.hoverMany model.hinted xTitler yTitler
        , grid = Grid.dots 1 Colors.gray
        , area = Area.default
        , line = Line.default
        , dots = Dots.default
        }


containerConfig : Container.Config Msg
containerConfig =
    Container.custom
        { attributesHtml = []
        , attributesSvg = []
        , size = Container.relative
        , margin = Container.Margin 30 100 30 70
        , id = "line-chart-area"
        }
