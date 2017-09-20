module Main exposing (..)

import Data exposing (Run, Page, RunInfo, Results, decodeRunInfo, decodeRun, decodePage, decodeResults)
import Date
import Dict exposing (Dict)
import Graph exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import List.Extra as EList
import Navigation exposing (Location)
import Return exposing (command, andThen, mapCmd)
import Router
import Scheduler exposing (SchedulerOptions)
import String.Extra exposing (leftOf, rightOfBack, fromInt, clean)
import Task
import Time
import Tuple
import Util exposing (natSort, return)


{-| Bolilerplate: Wires the application together
-}
main : Program Never Model Msg
main =
    Navigation.program (Router.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-----------------------------------
-- Types and definitions
-----------------------------------


{-| What should we shouw in the interface? Either the plot view or the list of tests to
run with their latest known status.
-}
type Screen
    = PlotScreen
    | ScheduleRunScreen


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
    , pages : Dict String (List Int) -- A reverse map of the pages tested for the loaded run stats
    , graph : Title
    , graphState : Graph.Model
    , filteredGroups : List String
    , concurrencyComparison : Maybe Int
    , selectedPage : Maybe PageSelection
    , currentScreen : Screen
    , schedulerState : Scheduler.Model
    , pollForResults : Maybe String
    }


{-| Creates an empty model and also creates the initial command to execute when
the applicaiton starts. That is, loading the list of runs from the server.
-}
init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( schedulerModel, scheduleInit ) =
            Scheduler.init

        initialReturn =
            setRoute (Router.fromLocation location)
                { runTitles = []
                , searchField = ""
                , runs = []
                , pages = Dict.empty
                , graph = "Mean Time / Concurrency"
                , graphState = Graph.defaultModel
                , filteredGroups = []
                , selectedPage = Nothing
                , concurrencyComparison = Nothing
                , currentScreen = PlotScreen
                , schedulerState = schedulerModel
                , pollForResults = Nothing
                }
    in
        initialReturn
            |> command (Http.send LoadRunTitles (getRuns ""))
            |> command (Cmd.map SchedulerMsg scheduleInit)


{-| Contains a list of all the possible actions that can be executed within the
application. Each action contains a set of arguments associated with it.
-}
type Msg
    = SetRoute (Maybe Router.Route)
    | SearchFieldUpdated String
    | SearchButtonClicked
    | LoadRunTitles (Result Http.Error (List RunInfo))
    | LoadRunStats (Result Http.Error Results)
    | LoadPageStats (Result Http.Error (List Page))
    | LoadFromLocation (Result Http.Error ( Results, String, Maybe (List Page) ))
    | RefreshRunStats (Result Http.Error ( Results, String, Maybe (List Page) ))
    | GraphMsg Graph.Msg
    | RunTitleClicked String
    | ChangeGraphType String
    | ToggleFilterGroup String
    | ShowOnlyGroup String
    | PageNameClicked String
    | ChangeConcurrencyComparison Int
    | ChangeScreen Screen
    | SchedulerMsg Scheduler.Msg
    | PollForResults String



-----------------------------------
-- Update Logic
-----------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.pollForResults of
        Nothing ->
            Sub.none

        Just testName ->
            Time.every (30 * Time.second) (\_ -> PollForResults testName)


{-| Handles all the actions performed in the app and returns a pair with the mutated model
and the next command to be executed to continue with the normal application flow.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute route ->
            setRoute route model

        ChangeScreen screen ->
            changeScreen screen model
                |> command (Router.modifyUrl (screenToRoute screen model))

        SearchFieldUpdated name ->
            -- When writing anything in the search field we just need to store it.
            -- no other changes are required
            updateSearchField name model
                |> return

        SearchButtonClicked ->
            -- When the search button is clicked, then we need to reset the previous
            -- results state and trigger a HTTP request to get the results.
            resetRunsState model
                |> startPollingForResults model.searchField
                |> return
                |> command (Http.send LoadRunStats (getManyRuns model.searchField))

        RunTitleClicked name ->
            -- Similarly to clicking the search button, when clicming on one of the test
            -- titles, we need to reset the previous results and then trigger the same
            -- HTTP request to fetch new results.
            model
                |> updateSearchField name
                |> startPollingForResults name
                |> resetRunsState
                |> return
                |> command (Http.send LoadRunStats (getManyRuns name))
                |> andThen updateTheUrl

        LoadRunTitles (Err error) ->
            -- If there was an error loading the run titles, we just log it in the console
            debugError model "LoadRunTitles" error

        LoadRunTitles (Ok runs) ->
            return { model | runTitles = extractTitles runs }

        LoadRunStats (Err error) ->
            -- If there was an error loading the run stats, we just log it in the console
            debugError model "LoadRunStats" error

        LoadRunStats (Ok { runs, pages }) ->
            -- But if we could successfully load the run stats, then we store the results and
            -- calculate run groups that need be shown
            { model | selectedPage = Nothing }
                |> setRuns runs
                |> setPageMap pages
                |> return

        LoadPageStats (Err error) ->
            -- If there was an error loading the page stats, we just log it in the console
            debugError model "LoadPageStats" error

        LoadPageStats (Ok pages) ->
            -- But if we successfully get the HTTP response for the page stats, then we need
            -- to store them, after indexing them by runId
            case model.selectedPage of
                Nothing ->
                    -- If nothing was selected, yet we still got some http results, than means we
                    -- are facing a race condition. Better respect that the user has made no selection
                    -- and discard the results entirely.
                    return model

                Just (PageSelection page _) ->
                    model
                        |> setPageSelection page pages
                        |> return

        LoadFromLocation (Err error) ->
            -- This is the case when we want to re-construct the state from the browser URL
            -- ... but an error happened
            debugError model "LoadFromLocation" error

        LoadFromLocation (Ok ( { runs, pages }, page, Just pageStats )) ->
            -- This is the case when we want to re-construct the state from the browser URL
            model
                |> setRuns runs
                |> setPageMap pages
                |> setPageSelection page pageStats
                |> return

        LoadFromLocation (Ok ( { runs, pages }, _, Nothing )) ->
            -- This is the case when we want to re-construct the state from the browser URL
            { model | selectedPage = Nothing }
                |> setRuns runs
                |> setPageMap pages
                |> return

        RefreshRunStats (Err error) ->
            -- This is the case we are auto-refreshing stats currenlty in the viewport
            -- ... but an error happened
            debugError model "RefreshRunStats" error

        RefreshRunStats (Ok ( { runs, pages }, page, Just pageStats )) ->
            -- This is the case when we want to re-construct the state from the browser URL
            { model | runs = runs }
                |> setPageMap pages
                |> setPageSelection page pageStats
                |> return

        RefreshRunStats (Ok ( { runs, pages }, _, Nothing )) ->
            -- This is the case when we want to re-construct the state from the browser URL
            { model | runs = runs }
                |> setPageMap pages
                |> return

        GraphMsg gMsg ->
            let
                ( m, c ) =
                    Graph.update gMsg model.graphState
            in
                ( { model | graphState = m }, Cmd.map GraphMsg c )

        ChangeGraphType name ->
            let
                ( newType, newConcurrency ) =
                    Graph.chooseGraphType name model.runs
            in
                { model | graph = newType |> Maybe.withDefault model.graph, concurrencyComparison = newConcurrency }
                    |> return
                    |> andThen updateTheUrl

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
                return { model | filteredGroups = newFilteredGroups }

        ShowOnlyGroup group ->
            -- If a group name is doubleclicked, then we need to only show its corresponding points in the graph
            -- and hide everything else.
            return { model | filteredGroups = [ group ] }

        PageNameClicked page ->
            -- When a page URL is clicked in the list, we want update the plot to show the statistics for
            -- that specific page. We do this by fetching the stats from the server for all of the runs
            -- that currently are being shown in the plot.
            let
                result =
                    { model | selectedPage = Just (PageSelection page Dict.empty) }
                        |> return
                        |> command (Http.send LoadPageStats (getPageByRuns model.runs page))
                        |> andThen updateTheUrl
            in
                case model.selectedPage of
                    Nothing ->
                        result

                    Just (PageSelection currentPage _) ->
                        if page == currentPage then
                            -- If the user clicks the page name for the second time, then we clear
                            -- the selection
                            { model | selectedPage = Nothing }
                                |> return
                                |> andThen updateTheUrl
                        else
                            result

        ChangeConcurrencyComparison concurrency ->
            -- When changing the concurrency level for the comparison, we just need to store the selection
            { model | concurrencyComparison = Just concurrency }
                |> return
                |> andThen updateTheUrl

        SchedulerMsg sMsg ->
            let
                ( m, c ) =
                    Scheduler.update sMsg model.schedulerState
            in
                ( { model | schedulerState = m }, Cmd.map SchedulerMsg c )

        PollForResults name ->
            return model
                |> command
                    (loadRunThenPage
                        RefreshRunStats
                        (Just name)
                        (model.selectedPage |> Maybe.map (\(PageSelection n _) -> n))
                    )


setRoute : Maybe Router.Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just Router.Home ->
            changeScreen PlotScreen model

        Just Router.ScheduleTest ->
            changeScreen ScheduleRunScreen model

        Just (Router.SimplePlot plotIndex maybeTestName maybePageName) ->
            { model | pollForResults = maybeTestName }
                |> setGraphByIndex plotIndex
                |> return
                |> command (loadRunThenPage LoadFromLocation maybeTestName maybePageName)

        Just (Router.ComparisonPlot plotIndex level maybeTestName maybePageName) ->
            { model | concurrencyComparison = Just level, pollForResults = maybeTestName }
                |> setGraphByIndex plotIndex
                |> return
                |> command (loadRunThenPage LoadFromLocation maybeTestName maybePageName)


changeScreen : Screen -> Model -> ( Model, Cmd Msg )
changeScreen screen model =
    -- When toggling between the schedule runs view and the plot view, we need to load the
    -- basic information for each of them via a HTTP request.
    -- Additionally, we store the "view" selection for the rendering phase
    let
        effect =
            case screen of
                ScheduleRunScreen ->
                    Cmd.map SchedulerMsg <| Tuple.second Scheduler.init

                PlotScreen ->
                    Http.send LoadRunTitles (getRuns "")
    in
        ( { model | currentScreen = screen }, effect )


updateSearchField : String -> Model -> Model
updateSearchField name model =
    { model | searchField = name }


resetRunsState : Model -> Model
resetRunsState model =
    { model
        | runs = []
        , filteredGroups = []
    }


setGraphByIndex : Int -> Model -> Model
setGraphByIndex index model =
    let
        graph =
            validGraphs
                |> List.indexedMap (\ix graph -> ( ix, graph ))
                |> List.filter (\( ix, _ ) -> ix == index)
                |> List.head
                |> Maybe.map (\( _, ( name, _ ) ) -> name)
    in
        { model | graph = Maybe.withDefault model.graph graph }


setRuns : List Run -> Model -> Model
setRuns runs model =
    { model
        | runs = runs
        , filteredGroups = defaultFilteredGroups runs
    }


setPageMap : Dict String (List Int) -> Model -> Model
setPageMap pages model =
    { model
        | pages = pages
    }


setPageSelection : String -> List Page -> Model -> Model
setPageSelection page pages model =
    let
        -- let's create a dictionary of page stats indexed by their runId
        indexed =
            pages
                |> List.map (\p -> ( p.runId, p ))
                |> Dict.fromList
    in
        { model | selectedPage = Just (PageSelection page indexed) }


startPollingForResults : String -> Model -> Model
startPollingForResults testName model =
    { model | pollForResults = Just testName }


debugError : Model -> String -> a -> ( Model, Cmd msg )
debugError model place err =
    let
        _ =
            Debug.log ("error at " ++ place ++ ":") err
    in
        ( model, Cmd.none )


type alias ReloadAction =
    Result Http.Error ( Results, String, Maybe (List Page) ) -> Msg


loadRunThenPage : ReloadAction -> Maybe String -> Maybe String -> Cmd Msg
loadRunThenPage action maybeTestName maybePageName =
    getManyRuns (Maybe.withDefault "__" maybeTestName)
        |> Http.toTask
        |> Task.andThen
            (\({ runs, pages } as results) ->
                case maybePageName of
                    Nothing ->
                        Task.succeed (Ok ( results, "", Nothing ))

                    Just page ->
                        getPageByRuns runs page
                            |> Http.toTask
                            |> Task.andThen (\stats -> Task.succeed (Ok ( results, page, Just stats )))
            )
        |> Task.onError (\e -> Task.succeed (Err e))
        |> Task.perform action



-----------------------
-- Route building
-----------------------


screenToRoute : Screen -> Model -> Router.Route
screenToRoute screen model =
    case screen of
        ScheduleRunScreen ->
            Router.ScheduleTest

        PlotScreen ->
            buildPlotRoute model


buildPlotRoute : Model -> Router.Route
buildPlotRoute model =
    let
        graphIndex =
            validGraphs
                |> List.indexedMap
                    (\index ( t, g ) ->
                        if t == model.graph then
                            Just ( index, g )
                        else
                            Nothing
                    )
                |> List.filterMap identity
                |> List.head

        page =
            model.selectedPage
                |> Maybe.map (\(PageSelection name _) -> name)

        level =
            Maybe.withDefault 0 model.concurrencyComparison
    in
        if String.trim model.searchField == "" then
            Router.Home
        else
            case graphIndex of
                Nothing ->
                    Router.Home

                Just ( index, Scatter _ _ _ _ ) ->
                    Router.SimplePlot index (Just model.searchField) page

                Just ( index, _ ) ->
                    Router.ComparisonPlot index level (Just model.searchField) page


updateTheUrl : Model -> ( Model, Cmd Msg )
updateTheUrl m =
    ( m, (buildPlotRoute m |> Router.modifyUrl) )



-----------------------
-- HTTP Requests
-----------------------


{-| Returns a Request object that can be used to load a list of RunInfo. This is
used for displaying the list of runs that can be selected in the menu.
-}
getRuns : String -> Http.Request (List RunInfo)
getRuns name =
    Http.get ("/runs?match=" ++ (Http.encodeUri name))
        (Decode.field "runs" (Decode.list decodeRunInfo))


{-| Returns a Request object that can be used to load a list of run statistics
-}
getManyRuns : String -> Http.Request Results
getManyRuns match =
    Http.get ("/runs/rollup/?match=" ++ Http.encodeUri match) decodeResults


{-| Returns a Request object that can be used to load a list of page statistics
-}
getPageByRuns : List Run -> String -> Http.Request (List Page)
getPageByRuns runs page =
    getPageStats (List.map (.run >> .id) runs) page


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



-----------------------
-- Helper functions
-----------------------


{-| Returns the list of groups names that shoudl be selected by default when
displaying the plot.
-}
defaultFilteredGroups : List Run -> List String
defaultFilteredGroups runs =
    runs
        |> buildGroups
        |> List.map Tuple.first


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
                        Html.map SchedulerMsg <|
                            Scheduler.view model.schedulerState
                ]
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


{-| Renders the right panel (where the plot and other info are)
-}
rightPlotPanel : Model -> Html Msg
rightPlotPanel model =
    case model.runs of
        [] ->
            text ""

        _ ->
            div [ class "view-plot view-plot__closed" ]
                [ div [ class "view-plot--left" ]
                    [ Html.map GraphMsg <|
                        plotRuns
                            model.graphState
                            model.graph
                            model.runs
                            model.filteredGroups
                            model.concurrencyComparison
                            (model.selectedPage |> Maybe.map (\(PageSelection _ p) -> p))
                    ]
                , div [ class "view-plot--right" ] [ rightmostPanel model ]
                ]


{-| Renders the right column where the graph selector and lists of pages reside
-}
rightmostPanel : Model -> Html Msg
rightmostPanel { graph, filteredGroups, runs, pages, concurrencyComparison, selectedPage } =
    div []
        [ select [ onChange ChangeGraphType ] (List.map (renderGraphItem graph) validGraphs)
        , renderConcurrencySelector concurrencyComparison runs
        , renderGroups filteredGroups runs
        , h4 [] [ text "Pages" ]
        , renderPageList (extractSelectedPage selectedPage) pages
        , div [ class "scrollFader" ] []
        ]


renderGraphItem : Title -> ( Title, Graph ) -> Html msg
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
            , onDoubleClick (ShowOnlyGroup runGroup.run.groupName)
            ]
            [ span
                [ style [ ( "color", color ), ( "font-size", "25px" ) ] ]
                [ text "●", text " " ]
            , text runGroup.run.groupName
            ]


renderPageList : String -> Dict String (List Int) -> Html Msg
renderPageList selected pages =
    ul [ class "view-plot--right__pages" ] (List.map (renderPageItem selected) (Dict.keys pages))


renderPageItem : String -> String -> Html Msg
renderPageItem selected page =
    let
        classes =
            [ ( "selected", selected == page ) ]
    in
        li [] [ a [ classList classes, onClick (PageNameClicked page) ] [ text page ] ]


extractSelectedPage : Maybe PageSelection -> String
extractSelectedPage selection =
    selection
        |> Maybe.map (\(PageSelection page _) -> page)
        |> Maybe.withDefault ""



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
