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
import List.Extra as EList
import String.Extra exposing (leftOf, rightOfBack, fromInt, clean)


{-| Bolilerplate: Wires the application together
-}
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-----------------------------------
-- Types and definitions
-----------------------------------


{-| Represents the statistics as they come from the wrecker-ui server
-}
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
    , quantile95 : Float
    }


{-| Contains the basic info for a Run done using wrecker
-}
type alias RunInfo =
    { created : Date.Date
    , concurrency : Int
    , groupName : String
    , id : Int
    , match : String
    }


{-| A run is a collection of pages that were tested, the load stats
and also the basic information related to the run itself.
-}
type alias Run =
    { pages : List String
    , stats : Stats
    , run : RunInfo
    }


{-| A page is the combination of a url and a run id. It contains the
statistics related to loading such url
-}
type alias Page =
    { url : String
    , runId : Int
    , stats : Stats
    }


{-| Wrecker-UI is able to schedule new tests from the iterface. Test Runs can
have any of the following statuses as reported by the server.
-}
type RunStatus
    = Running
    | Done
    | Scheduled
    | NotYet


{-| Represents the list of possible tests that can be run in the server, and
their corresponding latest known status.
-}
type TestList
    = TestList (Dict String RunStatus)


{-| Quick alias to avoid typing when representing a pair of (color, rundata)
This pair is used when plotting the graph and the color is used to fill the circle
for each point in the plot.
-}
type alias GraphData =
    ( String, Run )


{-| What should we shouw in the interface? Either the plot view or the list of tests to
run with their latest known status.
-}
type Screen
    = PlotScreen
    | ScheduleRunScreen


{-| It is possibible to parametrize test runs in the server. This type contains the fields
that can be parametrized for each run.
-}
type alias SchedulerOptions =
    { testTitle : String
    , annotationTitle : String
    , concurrencyStart : Int
    , concurrencyEnd : Int
    , stepSize : Int
    }


{-| This is a helper type that is used for rendering the scheduler options form.
By enumerating all fields that can be modified, we can easily track changes to each
of those fields.
-}
type SchedulerField
    = AnnotationTitle
    | ConcurrencyStart
    | ConcurrencyEnd
    | StepSize


{-| Statistics can be explored down to a per-page level. This type represents the
fact that the user has chosed to see the statistics for a specific page. The first
member in the constructor represents the page url and the second member are the
statistics for such url indexed by run id.
-}
type PageSelection
    = PageSelection String (Dict Int Page)


{-| The Model contains all the applicaiton state that is used to render the plot
and the rest of the UI
-}
type alias Model =
    { runTitles : List String
    , searchField : String
    , runs : List Run
    , graph : Title
    , hovered : Maybe Point
    , filteredGroups : List String
    , concurrencyComparison : Maybe Int
    , selectedPage : Maybe PageSelection
    , testList : TestList
    , currentScreen : Screen
    , schedulerOptions : Maybe SchedulerOptions
    }


{-| Creates an empty model and also creates the initial command to execute when
the applicaiton starts. That is, loading the list of runs from the server.
-}
init : ( Model, Cmd Msg )
init =
    let
        model =
            { runTitles = []
            , searchField = ""
            , runs = []
            , graph = "Mean Time / Concurrency"
            , hovered = Nothing
            , filteredGroups = []
            , selectedPage = Nothing
            , concurrencyComparison = Nothing
            , testList = TestList Dict.empty
            , currentScreen = PlotScreen
            , schedulerOptions = Nothing
            }
    in
        model ! [ Http.send LoadRunTitles (getRuns ""), Http.send LoadTestSchedule getTestList ]


{-| Contains a list of all the possible actions that can be executed within the
application. Each action contains a set of arguments associated with it.
-}
type Msg
    = SearchFieldUpdated String
    | SearchButtonClicked
    | LoadRunTitles (Result Http.Error (List RunInfo))
    | LoadRunList (Result Http.Error (List RunInfo))
    | LoadRunStats (Result Http.Error (List Run))
    | LoadPageStats (Result Http.Error (List Page))
    | Hover (Maybe Point)
    | RunTitleClicked String
    | ChangeGraphType String
    | ToggleFilterGroup String
    | PageNameClicked String
    | ChangeConcurrencyComparison Int
    | ChangeScreen Screen
    | TestTitleClicked String
    | LoadTestSchedule (Result Http.Error TestList)
    | SchedulerFieldChanged SchedulerField String
    | ScheduleTestClicked
    | ScheduleTestSent (Result Http.Error Decode.Value)


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



-----------------------------------
-- Update Logic
-----------------------------------


{-| Handles all the actions performed in the app and returns a pair with the mutated model
and the next command to be executed to continue with the normal application flow.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchFieldUpdated name ->
            -- When writing anything in the search field we just need to store it.
            -- no other changes are required
            ( { model | searchField = name }, Cmd.none )

        SearchButtonClicked ->
            -- But when the search button is clicked, then we need to reset the previous
            -- results state and trigger a HTTP request to get the results.
            ( { model
                | runs = []
                , filteredGroups = []
              }
            , Http.send LoadRunList (getRuns model.searchField)
            )

        RunTitleClicked name ->
            -- Similarly to clicking the search button, when clicming on one of the test
            -- titles, we need to reset the previous results and then trigger the same
            -- HTTP request to fetch new results.
            ( { model
                | searchField = name
                , runs = []
                , filteredGroups = []
              }
            , Http.send LoadRunList (getRuns name)
            )

        LoadRunTitles (Err error) ->
            -- If there was an error loading the run titles, we just log it in the console
            debugError model error

        LoadRunTitles (Ok runs) ->
            ( { model | runTitles = extractTitles runs }, Cmd.none )

        LoadRunList (Err error) ->
            -- If there was an error loading the list of tests, we just log it in the console
            debugError model error

        LoadRunList (Ok runs) ->
            -- But if we could successfully load the run list, then we try to find all the stats
            -- for such list immediately using a HTTP request
            ( model, Http.send LoadRunStats (getManyRuns (List.map .id runs)) )

        LoadRunStats (Err error) ->
            -- If there was an error loading the run stats, we just log it in the console
            debugError model error

        LoadRunStats (Ok runs) ->
            -- But if we could successfully load the run stats, then we store the results and
            -- calculate run groups that need be shown
            let
                newRuns =
                    List.append runs model.runs
            in
                ( { model
                    | runs = newRuns
                    , filteredGroups = defaultFilteredGroups newRuns
                    , selectedPage = Nothing
                  }
                , Cmd.none
                )

        LoadPageStats (Err error) ->
            -- If there was an error loading the page stats, we just log it in the console
            debugError model error

        LoadPageStats (Ok pages) ->
            -- But if we successfully get the HTTP response for the page stats, then we need
            -- to store them, after indexing them by runId
            case model.selectedPage of
                Nothing ->
                    -- If nothing was selected, yet we still got some http results, than means we
                    -- are facing a race condition. Better respect that the user has made no selection
                    -- and discaard the results entirely.
                    ( model, Cmd.none )

                Just (PageSelection page _) ->
                    let
                        -- let's create a dictionary of page stats indexed by their runId
                        indexed =
                            pages
                                |> List.map (\p -> ( p.runId, p ))
                                |> Dict.fromList
                    in
                        ( { model | selectedPage = Just (PageSelection page indexed) }, Cmd.none )

        LoadTestSchedule (Err error) ->
            -- If there was an error loading the test schedule, we just log it in the console
            debugError model error

        LoadTestSchedule (Ok tests) ->
            -- After getting the ajax results for the test schedule we need do nothing else but store them
            ( { model | testList = tests }, Cmd.none )

        Hover point ->
            -- When the mouse pointer is over or has left a point in the graph, we need to record that change
            -- so that we can render or hide the selection hints.
            ( { model | hovered = point }, Cmd.none )

        ChangeGraphType name ->
            -- If the user choses to change the graph type we need to first extract the graph definition for
            -- the selection, and then store in the model the graph type, so it can be rendered in the view.
            let
                graphType =
                    validGraphs
                        |> List.filter (\( title, _ ) -> title == name)
                        |> List.head

                ( newType, newConcurrency ) =
                    case graphType of
                        Nothing ->
                            ( model.graph, model.concurrencyComparison )

                        Just ( t, Timeline _ _ ) ->
                            ( t, appropriateConcurrency model.runs )

                        Just ( t, _ ) ->
                            ( t, Nothing )

                -- When comparing diffent runs by concurrency level, we need to pick an
                -- appropriate concurrency level for the first showing of the graph.
                -- In this case just selecting the maximum concurrency may be just good enough.
                appropriateConcurrency runs =
                    runs
                        |> List.map (.run >> .concurrency)
                        |> List.maximum
            in
                ( { model
                    | graph = newType
                    , concurrencyComparison = newConcurrency
                  }
                , Cmd.none
                )

        ToggleFilterGroup group ->
            -- If a group name is clicked, then we need to hide its corresponding points in the graph.
            -- When it is clicked for a second time, then we show them again.
            -- We also need to make sure that there is always at least one group visible in the plot.
            let
                filteredGroups =
                    if List.member group model.filteredGroups then
                        List.filter ((/=) group) model.filteredGroups
                    else
                        group :: model.filteredGroups

                newFilteredGroups =
                    if List.isEmpty filteredGroups then
                        defaultFilteredGroups model.runs
                    else
                        filteredGroups
            in
                ( { model | filteredGroups = newFilteredGroups }, Cmd.none )

        PageNameClicked page ->
            -- When a page URL is clicked in the list, we want update the plot to show the statistics for
            -- that specific page. We do this by fetching the stats from the server for all of the runs
            -- that currently are being shown in the plot.
            let
                effect =
                    Http.send LoadPageStats (getPageStats (model.runs |> List.map (.run >> .id)) page)

                result =
                    ( { model | selectedPage = Just (PageSelection page Dict.empty) }, effect )
            in
                case model.selectedPage of
                    Nothing ->
                        result

                    Just (PageSelection currentPage _) ->
                        if page == currentPage then
                            -- If the user clicks the page name for the second time, then we clear
                            -- the selection
                            ( { model | selectedPage = Nothing }, Cmd.none )
                        else
                            result

        ChangeConcurrencyComparison concurrency ->
            -- When changing the concurrency level for the comparison, we just need to store the selection
            ( { model | concurrencyComparison = Just concurrency }, Cmd.none )

        ChangeScreen screen ->
            -- When toggling between the schedule runs view and the plot view, we need to load the
            -- basic information for each of them via a HTTP request.
            -- Additionally, we store the "view" selection for the rendering phase
            let
                effect =
                    case screen of
                        ScheduleRunScreen ->
                            Http.send LoadTestSchedule getTestList

                        PlotScreen ->
                            Http.send LoadRunTitles (getRuns "")
            in
                ( { model | currentScreen = screen }, effect )

        TestTitleClicked title ->
            -- In the Scheduling view, when the user clicks a test title, we populate the default
            -- scheduling options to show in the form.
            let
                schedulerOptions =
                    model.schedulerOptions
                        |> Maybe.map (\s -> { s | testTitle = title })
                        |> Maybe.withDefault (SchedulerOptions title "" 10 300 10)
            in
                ( { model | schedulerOptions = Just schedulerOptions }, Cmd.none )

        SchedulerFieldChanged field value ->
            -- This is a catch-all for any changes done to the fields in the scheduling options form.
            -- depending on the value for the field parameter, we update a different field in the scheduling
            -- options record.
            let
                updateSchedulerField opts =
                    case field of
                        AnnotationTitle ->
                            { opts | annotationTitle = value }

                        ConcurrencyStart ->
                            { opts | concurrencyStart = Result.withDefault opts.concurrencyStart (String.toInt value) }

                        ConcurrencyEnd ->
                            { opts | concurrencyEnd = Result.withDefault opts.concurrencyEnd (String.toInt value) }

                        StepSize ->
                            { opts | stepSize = Result.withDefault opts.stepSize (String.toInt value) }

                schedulerOptions =
                    model.schedulerOptions
                        |> Maybe.map updateSchedulerField
            in
                ( { model | schedulerOptions = schedulerOptions }, Cmd.none )

        ScheduleTestClicked ->
            -- Once the schedule test button is clicked, we take the scheduling options record and post it
            -- using a HTTP request, so that the test is run in the server.
            let
                effect =
                    model.schedulerOptions
                        |> Maybe.map (\o -> Http.send ScheduleTestSent (postTestSchedule o))
                        |> Maybe.withDefault Cmd.none
            in
                ( model, effect )

        ScheduleTestSent (Err err) ->
            -- If we got an error when sending the test to the scheduler, we log the error, but also
            -- hide the scheduler form.
            let
                _ =
                    Debug.log "ScheduleTestSent" err
            in
                ( { model | schedulerOptions = Nothing }, Cmd.none )

        ScheduleTestSent _ ->
            -- If the test is created successfully in the scheduler, we re-load the scheduler information
            -- from the server in order to reflect the new status in the list of tests.
            ( { model | schedulerOptions = Nothing }, Http.send LoadTestSchedule getTestList )


debugError : Model -> a -> ( Model, Cmd msg )
debugError model err =
    let
        _ =
            Debug.log "error:" err
    in
        ( model, Cmd.none )


{-| Returns a Request object that can be used to load a list of RunInfo. This is
used for displaying the list of runs that can be selected in the menu.
-}
getRuns : String -> Http.Request (List RunInfo)
getRuns name =
    Http.get ("/runs?match=" ++ (Http.encodeUri name))
        (Decode.field "runs" (Decode.list decodeRunInfo))


{-| Returns a Request object that can be used to load all the info related to a single Run
-}
getSingleRun : Int -> Http.Request Run
getSingleRun id =
    Http.get ("/runs/" ++ toString id) decodeRun


{-| Returns a Request object that can be used to load a list of run statistics
-}
getManyRuns : List Int -> Http.Request (List Run)
getManyRuns ids =
    let
        --Convert the list of ids into a comma separated string of numbers
        idsQ =
            ids
                |> List.foldl (\id acc -> acc ++ fromInt id ++ ",") ""
    in
        Http.get ("/runs/rollup/?ids=" ++ idsQ) (Decode.list decodeRun)


{-| Returns a Request object that can be used to load a list of page statistics
-}
getPageStats : List Int -> String -> Http.Request (List Page)
getPageStats ids page =
    let
        --Convert the list of ids into a comma separated string of numbers
        idsQ =
            ids
                |> List.foldl (\id acc -> acc ++ fromInt id ++ ",") ""
    in
        Http.get
            ("/runs/page?ids=" ++ idsQ ++ "&name=" ++ (Http.encodeUri page))
            (Decode.list decodePage)


{-| Returns a Request object that can be used to load the list of tests titles. This is
used for displaying the list of runs that can be selected when scheduling a Run.
-}
getTestList : Http.Request TestList
getTestList =
    Http.get "/test-list" decodeTestList


postTestSchedule : SchedulerOptions -> Http.Request Decode.Value
postTestSchedule options =
    let
        body =
            Http.multipartBody
                [ Http.stringPart "testTitle" options.testTitle
                , Http.stringPart "groupName" options.annotationTitle
                , Http.stringPart "concurrencyStart" (fromInt options.concurrencyStart)
                , Http.stringPart "concurrencyEnd" (fromInt options.concurrencyEnd)
                , Http.stringPart "stepSize" (fromInt options.stepSize)
                ]
    in
        Http.post "/test-list" body Decode.value


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


{-| Returns the list of groups names that shoudl be selected by default when
displaying the plot.
-}
defaultFilteredGroups : List Run -> List String
defaultFilteredGroups runs =
    runs
        |> buildGroups
        |> List.map Tuple.first


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


{-| Gets the unique run tiles from a list of Runs
-}
extractTitles : List RunInfo -> List String
extractTitles runs =
    runs
        |> List.map .match
        |> EList.unique
        |> List.sort



-----------------------------------
-- View Logic
-----------------------------------


{-| Takes a model state and renders all the HTML in the page out of it
-}
view : Model -> Html Msg
view model =
    div []
        [ div [ class "view" ]
            [ div [ class "view--left" ] [ leftPanel model.currentScreen model.searchField model.runTitles ]
            , div [ class "view--right" ]
                [ case model.currentScreen of
                    PlotScreen ->
                        rightPlotPanel model

                    ScheduleRunScreen ->
                        rightSchedulePanel model.testList model.schedulerOptions
                ]
            ]
        ]


rightSchedulePanel : TestList -> Maybe SchedulerOptions -> Html Msg
rightSchedulePanel (TestList list) options =
    let
        testNames =
            list
                |> Dict.toList
                |> List.sortWith (\( a, _ ) ( b, _ ) -> natSort a b)

        active =
            options
                |> Maybe.map .testTitle
                |> Maybe.withDefault ""
    in
        div [ class "view-plot view-plot__closed" ]
            [ div [ class "view-plot--left" ] [ ul [] (List.map (buildTestItems active) testNames) ]
            , div [ class "view-plot--right" ]
                [ case options of
                    Nothing ->
                        text ""

                    Just opts ->
                        schedulerOptions opts
                ]
            ]


buildTestItems : String -> ( String, RunStatus ) -> Html Msg
buildTestItems selected ( title, status ) =
    let
        showStatus s =
            case s of
                NotYet ->
                    text ""

                _ ->
                    span [ class "status" ] [ text (" - " ++ toString s) ]

        classes =
            [ ( "selected", selected == title ) ]
    in
        li []
            [ a [ onClick (TestTitleClicked title), classList classes ] [ text title ]
            , showStatus status
            ]


schedulerOptions : SchedulerOptions -> Html Msg
schedulerOptions options =
    div [ class "view-plot--right__concurrency" ]
        [ label []
            [ span [] [ text "Test Annotation" ]
            , input [ onInput (SchedulerFieldChanged AnnotationTitle), placeholder "E.g. Migrated to PHP 7" ] []
            ]
        , label []
            [ span [] [ text "Concurrency Start" ]
            , input [ onInput (SchedulerFieldChanged ConcurrencyStart), type_ "number", value (fromInt options.concurrencyStart) ] []
            ]
        , label []
            [ span [] [ text "Concurrency Target" ]
            , input [ onInput (SchedulerFieldChanged ConcurrencyEnd), type_ "number", value (fromInt options.concurrencyEnd) ] []
            ]
        , label []
            [ span [] [ text "Step Size" ]
            , input [ onInput (SchedulerFieldChanged StepSize), type_ "number", value (fromInt options.stepSize) ] []
            ]
        , label []
            [ button [ onClick ScheduleTestClicked ] [ text "Schedule Test" ]
            ]
        ]


{-| Renders the left panel (where the search button and the list of runs is)
-}
leftPanel : Screen -> String -> List String -> Html Msg
leftPanel screen defaultTitle runTitles =
    let
        bottomItems =
            case screen of
                ScheduleRunScreen ->
                    []

                PlotScreen ->
                    [ div [ class "view-header__search" ]
                        [ input
                            [ type_ "text"
                            , name "run_name"
                            , placeholder "Search Run"
                            , value defaultTitle
                            , onEnter SearchButtonClicked
                            , onInput SearchFieldUpdated
                            ]
                            []
                        , button [ onClick SearchButtonClicked ] [ text "go" ]
                        ]
                    , ul [ class "view-header__runs" ]
                        (runTitles
                            |> List.sortWith natSort
                            |> List.map (runListItem defaultTitle)
                        )
                    ]
    in
        header [ class "view-header" ]
            ([ a [ onClick (ChangeScreen ScheduleRunScreen) ]
                [ text "Schedule Run ➕ " ]
             , h1 [ class "view-header__title" ]
                [ a [ onClick (ChangeScreen PlotScreen) ]
                    [ text "Wrecker-UI" ]
                ]
             ]
                ++ bottomItems
            )


runListItem : String -> String -> Html Msg
runListItem selected title =
    let
        classes =
            [ ( "selected", selected == title ) ]
    in
        li [ onClick (RunTitleClicked title) ] [ a [ classList classes ] [ text title ] ]


natSort : String -> String -> Order
natSort a b =
    case ( String.toInt (leftOf " -" a), String.toInt (leftOf " -" b) ) of
        ( Ok aOk, Ok bOk ) ->
            compare aOk bOk

        _ ->
            compare a b


{-| Renders the right panel (where the plot and other info are)
-}
rightPlotPanel : Model -> Html Msg
rightPlotPanel model =
    case model.runs of
        [] ->
            text ""

        _ ->
            div [ class "view-plot view-plot__closed" ]
                [ div [ class "view-plot--left" ] [ plotRuns model ]
                , div [ class "view-plot--right" ] [ rightmostPanel model ]
                ]


{-| Renders the right column where the graph selector and lists of pages reside
-}
rightmostPanel : Model -> Html Msg
rightmostPanel { graph, filteredGroups, runs, concurrencyComparison, selectedPage } =
    div []
        [ select [ onChange ChangeGraphType ] (List.map (renderGraphItem graph) validGraphs)
        , renderConcurrencySelector concurrencyComparison runs
        , renderGroups filteredGroups runs
        , h4 [] [ text "Pages" ]
        , renderPageList (extractSelectedPage selectedPage) runs
        , div [ class "scrollFader" ] []
        ]


extractSelectedPage : Maybe PageSelection -> String
extractSelectedPage selection =
    selection
        |> Maybe.map (\(PageSelection page _) -> page)
        |> Maybe.withDefault ""


renderGraphItem : Title -> ( Title, Graph ) -> Html Msg
renderGraphItem current ( title, _ ) =
    option [ value title, selected (current == title) ] [ text title ]


renderConcurrencySelector : Maybe Int -> List Run -> Html Msg
renderConcurrencySelector current runs =
    case current of
        Nothing ->
            text ""

        Just concurrency ->
            let
                levels =
                    runs
                        |> List.map (.run >> .concurrency)
                        |> EList.unique
                        |> List.sort

                buildOption level =
                    option [ value (toString level), selected (concurrency == level) ] [ text (toString level) ]
            in
                select [ onChangeInt ChangeConcurrencyComparison ] (List.map buildOption levels)


renderGroups : List String -> List Run -> Html Msg
renderGroups filteredGroups runs =
    let
        groups =
            runs
                |> assignColors
                |> EList.uniqueBy Tuple.first
                |> List.sortWith sortByGroupName
                |> List.reverse
    in
        ul []
            (List.map
                (renderGroupItem filteredGroups)
                groups
            )


sortByGroupName : ( String, Run ) -> ( String, Run ) -> Order
sortByGroupName ( _, aRun ) ( _, bRun ) =
    let
        extractDate r =
            r.run.groupName
                |> rightOfBack "- "
                |> clean
                |> Date.fromString
                |> Result.withDefault (Date.fromTime 0)
                |> Date.toTime
    in
        compare (extractDate aRun) (extractDate bRun)


renderGroupItem : List String -> GraphData -> Html Msg
renderGroupItem filtered ( color, runGroup ) =
    let
        isFiltered =
            case filtered of
                [] ->
                    False

                _ ->
                    filtered
                        |> List.filter (\g -> g == runGroup.run.groupName)
                        |> List.isEmpty
    in
        li
            [ style [ ( "cursor", "pointer" ) ]
            , classList [ ( "grayed-out", isFiltered ) ]
            , onClick (ToggleFilterGroup runGroup.run.groupName)
            ]
            [ span
                [ style [ ( "color", color ), ( "font-size", "25px" ) ] ]
                [ text "●", text " " ]
            , text runGroup.run.groupName
            ]


renderPageList : String -> List Run -> Html Msg
renderPageList selected runs =
    ul [ class "view-plot--right__pages" ] (List.map (renderPageItem selected) (uniquePages runs))


renderPageItem : String -> String -> Html Msg
renderPageItem selected page =
    let
        classes =
            [ ( "selected", selected == page ) ]
    in
        li [] [ a [ classList classes, onClick (PageNameClicked page) ] [ text page ] ]


uniquePages : List Run -> List String
uniquePages runs =
    runs
        |> List.map .pages
        |> List.concat
        |> EList.unique
        |> List.sort


graphDefinition : Title -> Maybe Graph
graphDefinition title =
    validGraphs
        |> List.filter (\( t, _ ) -> title == t)
        |> List.map Tuple.second
        |> List.head


assembleRunData : Maybe PageSelection -> List GraphData -> List GraphData
assembleRunData selectedPage runs =
    case selectedPage of
        Nothing ->
            runs

        Just (PageSelection _ pageStats) ->
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


plotRuns : Model -> Html Msg
plotRuns { hovered, runs, graph, filteredGroups, concurrencyComparison, selectedPage } =
    let
        selectRunData graphData =
            assembleRunData selectedPage graphData
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
    -> List (Series data Msg)
    -> data
    -> Html Msg
basicSeries hovered xLegend yLegend =
    viewSeriesCustom
        { defaultSeriesPlotCustomizations
            | horizontalAxis = rangeFrameAxis hovered (.x >> roundNum 3)
            , margin = { top = 20, bottom = 20, left = 150, right = 40 }
            , toDomainLowest = \y -> y - 0.3
            , junk = legend xLegend yLegend
        }


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
    , toDataPoints = List.map (rangeFrameHintDot xGetter yGetter hinting)
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



---------------------------------------------
--- View Utilities
---------------------------------------------


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


onChange : (String -> Msg) -> Attribute Msg
onChange msg =
    let
        action =
            Decode.at [ "target", "value" ] Decode.string
                |> Decode.map msg
    in
        on "change" action


onChangeInt : (Int -> Msg) -> Attribute Msg
onChangeInt msg =
    let
        action =
            Decode.at [ "target", "value" ] Decode.string
                |> Decode.andThen
                    (\txt ->
                        case String.toInt txt of
                            Ok int ->
                                Decode.succeed int

                            Err e ->
                                Decode.fail e
                    )
                |> Decode.map msg
    in
        on "change" action



---------------------------------------------
--- JSON Decoding
---------------------------------------------


decodeRun : Decode.Decoder Run
decodeRun =
    Pipeline.decode Run
        |> Pipeline.required "pages" (Decode.list Decode.string)
        |> Pipeline.required "stats" decodeStats
        |> Pipeline.required "run" decodeRunInfo


decodePage : Decode.Decoder Page
decodePage =
    Pipeline.decode Page
        |> Pipeline.required "url" Decode.string
        |> Pipeline.required "runId" Decode.int
        |> Pipeline.custom decodeStats


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
        |> Pipeline.required "quantile95" Decode.float


decodeRunInfo : Decode.Decoder RunInfo
decodeRunInfo =
    Pipeline.decode RunInfo
        |> Pipeline.required "created" (Decode.string |> Decode.andThen decodeDateTime)
        |> Pipeline.required "concurrency" Decode.int
        |> Pipeline.required "groupName" Decode.string
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "match" Decode.string


decodeDateTime : String -> Decode.Decoder Date.Date
decodeDateTime datetimeString =
    case Date.fromString datetimeString of
        Err e ->
            Decode.fail e

        Ok date ->
            Decode.succeed date


decodeTestList : Decode.Decoder TestList
decodeTestList =
    Pipeline.decode TestList
        |> Pipeline.required "tests" (Decode.dict decodeRunStatus)


decodeRunStatus : Decode.Decoder RunStatus
decodeRunStatus =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "running" ->
                        Decode.succeed Running

                    "done" ->
                        Decode.succeed Done

                    "scheduled" ->
                        Decode.succeed Scheduled

                    "none" ->
                        Decode.succeed NotYet

                    _ ->
                        Decode.fail ("Could not decode " ++ s ++ " as a RunStatus")
            )
