module Main exposing (..)

import Data exposing (Run, Page, RunInfo, Results, RunGroup, GroupSet, decodeRunInfo, decodeRun, decodePage, decodeResults)
import Dict exposing (Dict)
import Dict.Extra exposing (filterGroupBy)
import Graph
import Graph.Types exposing (Title)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy as Lazy
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
import Util exposing (natSort, return, onEnter, onChange, onChangeInt)
import ServerState
import RunGroupPanel
import Graph.Coloring exposing (pickColor)
import Color.Convert exposing (colorToHex)


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


{-| The Model contains all the application state that is used to render the plot
and the rest of the UI
-}
type alias Model =
    { serverState : ServerState.State
    , runs : List Run
    , runLoadNumber : Int -- To prevent displaying old results, we keep a counter
    , pages : Dict String (List Int) -- A reverse map of the pages tested for the loaded run stats
    , graph : Title
    , graphState : Graph.Model
    , currentRunGroups : List RunGroup
    , filteredGroups : List RunGroup
    , concurrencyComparison : Maybe Int
    , selectedPage : Maybe PageSelection
    , currentScreen : Screen
    , schedulerState : Scheduler.Model
    , displayTestResults : Maybe String
    , runGroupsPanel : RunGroupPanel.Model
    , selectedGroupSet : Maybe GroupSet
    , availableGroupSets : List GroupSet
    }


{-| Creates an empty model and also creates the initial command to execute when
the applicaiton starts. That is, loading the list of runs from the server.
-}
init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( schedulerModel, scheduleInit ) =
            Scheduler.init

        ( serverState, serverStateInit ) =
            ServerState.init

        initialReturn =
            setRoute (Router.fromLocation location)
                { serverState = serverState
                , runs = []
                , runLoadNumber = 0
                , pages = Dict.empty
                , graph = "Mean Time / Concurrency"
                , graphState = Graph.defaultModel
                , currentRunGroups = []
                , filteredGroups = []
                , selectedPage = Nothing
                , concurrencyComparison = Nothing
                , currentScreen = PlotScreen
                , schedulerState = schedulerModel
                , displayTestResults = Nothing
                , runGroupsPanel = RunGroupPanel.init
                , selectedGroupSet = Nothing
                , availableGroupSets = []
                }
    in
        initialReturn
            |> command (Cmd.map SchedulerMsg scheduleInit)
            |> command (Cmd.map ServerStateMsg serverStateInit)


{-| Contains a list of all the possible actions that can be executed within the
application. Each action contains a set of arguments associated with it.
-}
type Msg
    = SetRoute (Maybe Router.Route)
    | LoadRunStats Int (Result Http.Error Results)
    | LoadPageStats (Result Http.Error (List Page))
    | LoadFromLocation (Result Http.Error ( Results, String, Maybe (List Page), Maybe Int ))
    | RefreshRunStats (Result Http.Error ( Results, String, Maybe (List Page), Maybe Int ))
    | GraphMsg Graph.Msg
    | RunTitleClicked String
    | ChangeGraphType String
    | ToggleFilterGroup Int
    | ShowOnlyGroup Int
    | PageNameClicked String
    | ChangeConcurrencyComparison Int
    | ChangeScreen Screen
    | SchedulerMsg Scheduler.Msg
    | ServerStateMsg ServerState.Msg
    | PollForResults String
    | RunGroupPanelMsg RunGroupPanel.Msg
    | ChangeGroupSet Int



-----------------------------------
-- Update Logic
-----------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.displayTestResults of
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

        RunTitleClicked name ->
            -- When clicking on one of the test titles, we need to reset the previous results and then trigger the same
            -- HTTP request to fetch new results.
            { model | selectedGroupSet = Nothing }
                |> loadRunsData name Nothing

        ChangeGroupSet setId ->
            let
                selectedSet =
                    model.availableGroupSets
                        |> List.filter (\set -> set.id == setId)
                        |> List.head

                testName =
                    model.displayTestResults
                        |> Maybe.withDefault ""
            in
                loadRunsData testName selectedSet model

        LoadRunStats _ (Err error) ->
            -- If there was an error loading the run stats, we just log it in the console
            debugError model "LoadRunStats" error

        LoadRunStats loadNumber (Ok { runs, pages, runGroups, availableGroupSets }) ->
            -- But if we could successfully load the run stats, then we store the results and
            -- calculate run groups that need be shown
            if model.runLoadNumber > loadNumber then
                -- If we got old results from the server, discard them
                ( model, Cmd.none )
            else
                { model | selectedPage = Nothing, runLoadNumber = loadNumber }
                    |> setRuns runs runGroups availableGroupSets
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

        LoadFromLocation (Ok ( { runs, pages, runGroups, availableGroupSets }, page, Just pageStats, selectedGroupSet )) ->
            -- This is the case when we want to re-construct the state from the browser URL
            model
                |> setRuns runs runGroups availableGroupSets
                |> setPageMap pages
                |> setPageSelection page pageStats
                |> setGroupSetSelection selectedGroupSet availableGroupSets
                |> return

        LoadFromLocation (Ok ( { runs, pages, runGroups, availableGroupSets }, _, Nothing, selectedGroupSet )) ->
            -- This is the case when we want to re-construct the state from the browser URL
            { model | selectedPage = Nothing }
                |> setRuns runs runGroups availableGroupSets
                |> setPageMap pages
                |> setGroupSetSelection selectedGroupSet availableGroupSets
                |> return

        RefreshRunStats (Err error) ->
            -- This is the case we are auto-refreshing stats currenlty in the viewport
            -- ... but an error happened
            debugError model "RefreshRunStats" error

        RefreshRunStats (Ok ( { runs, pages, runGroups, availableGroupSets }, page, Just pageStats, selectedGroupSet )) ->
            -- This is the case when we want to re-construct the state from the browser URL
            { model | runs = runs, availableGroupSets = availableGroupSets, currentRunGroups = runGroups }
                |> setPageMap pages
                |> setPageSelection page pageStats
                |> setGroupSetSelection selectedGroupSet availableGroupSets
                |> return

        RefreshRunStats (Ok ( { runs, pages, runGroups, availableGroupSets }, _, Nothing, selectedGroupSet )) ->
            -- This is the case when we want to re-construct the state from the browser URL
            { model | runs = runs, availableGroupSets = availableGroupSets, currentRunGroups = runGroups }
                |> setPageMap pages
                |> setGroupSetSelection selectedGroupSet availableGroupSets
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

        ToggleFilterGroup groupId ->
            -- If a group name is clicked, then we need to hide its corresponding points in the graph.
            -- When it is clicked for a second time, then we show them again.
            -- We also need to make sure that there is always at least one group visible in the plot.
            let
                filteredGroups =
                    if List.filter (\g -> g.id == groupId) model.filteredGroups /= [] then
                        List.filter (\g -> g.id /= groupId) model.filteredGroups
                    else
                        List.append
                            (List.filter (\g -> g.id == groupId) model.currentRunGroups)
                            model.filteredGroups

                newFilteredGroups =
                    if List.isEmpty filteredGroups then
                        model.currentRunGroups
                    else
                        filteredGroups
            in
                return { model | filteredGroups = newFilteredGroups }

        ShowOnlyGroup group ->
            -- If a group name is doubleclicked, then we need to only show its corresponding points in the graph
            -- and hide everything else.
            return { model | filteredGroups = List.filter (\g -> g.id == group) model.currentRunGroups }

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
                ( m, c, reloadServerState ) =
                    Scheduler.update
                        model.serverState.groupSets
                        sMsg
                        model.schedulerState

                effect =
                    Cmd.map SchedulerMsg c

                allEffects =
                    if reloadServerState then
                        Cmd.batch [ effect, Cmd.map ServerStateMsg <| Tuple.second ServerState.init ]
                    else
                        effect
            in
                ( { model | schedulerState = m }, allEffects )

        ServerStateMsg sMsg ->
            let
                ( m, c ) =
                    ServerState.update sMsg model.serverState
            in
                ( { model | serverState = m }, Cmd.map ServerStateMsg c )

        RunGroupPanelMsg rMsg ->
            ( { model | runGroupsPanel = RunGroupPanel.update rMsg model.runGroupsPanel }, Cmd.none )

        PollForResults name ->
            return model
                |> command
                    (loadRunThenPage
                        RefreshRunStats
                        (Just name)
                        (Maybe.map .id model.selectedGroupSet)
                        (model.selectedPage |> Maybe.map (\(PageSelection n _) -> n))
                    )
                |> command (Cmd.map ServerStateMsg <| Tuple.second ServerState.init)


{-| An update helper function that is responsible for setting the test name to show results for
and filter by a particular group set. It will command the fetching of results and the changing
of the URL.
-}
loadRunsData : String -> Maybe GroupSet -> Model -> ( Model, Cmd Msg )
loadRunsData testName selectedSet model =
    let
        newRunNumber =
            model.runLoadNumber + 1
    in
        { model | runLoadNumber = newRunNumber }
            |> startPollingForResults testName selectedSet
            |> resetRunsState
            |> return
            |> command (Http.send (LoadRunStats newRunNumber) (getManyRuns testName (Maybe.map .id selectedSet)))
            |> andThen updateTheUrl


setRoute : Maybe Router.Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just Router.Home ->
            changeScreen PlotScreen model

        Just Router.ScheduleTest ->
            changeScreen ScheduleRunScreen model

        Just (Router.SimplePlot plotIndex maybeTestName maybePageName maybeSetId) ->
            { model | displayTestResults = maybeTestName }
                |> setGraphByIndex plotIndex
                |> return
                |> command
                    (loadRunThenPage LoadFromLocation maybeTestName maybeSetId maybePageName)

        Just (Router.ComparisonPlot plotIndex level maybeTestName maybePageName maybeSetId) ->
            { model | concurrencyComparison = Just level, displayTestResults = maybeTestName }
                |> setGraphByIndex plotIndex
                |> return
                |> command
                    (loadRunThenPage LoadFromLocation maybeTestName maybeSetId maybePageName)


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
                    Cmd.none

        allEffects =
            Cmd.batch [ effect, Cmd.map ServerStateMsg <| Tuple.second ServerState.init ]
    in
        ( { model | currentScreen = screen }, allEffects )


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
            Graph.validGraphs
                |> List.indexedMap (\ix graph -> ( ix, graph ))
                |> List.filter (\( ix, _ ) -> ix == index)
                |> List.head
                |> Maybe.map (\( _, ( name, _ ) ) -> name)
    in
        { model | graph = Maybe.withDefault model.graph graph }


setRuns : List Run -> List RunGroup -> List GroupSet -> Model -> Model
setRuns runs runGroups availableGroupSets model =
    { model
        | runs = runs
        , currentRunGroups = runGroups
        , filteredGroups = runGroups
        , availableGroupSets = availableGroupSets
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


setGroupSetSelection : Maybe Int -> List GroupSet -> Model -> Model
setGroupSetSelection maybeId groupSets model =
    let
        groupSetFromId =
            groupSets
                |> List.filter (\set -> Just set.id == maybeId)
                |> List.head
    in
        { model | selectedGroupSet = groupSetFromId }


startPollingForResults : String -> Maybe GroupSet -> Model -> Model
startPollingForResults testName selectedSet model =
    { model | displayTestResults = Just testName, selectedGroupSet = selectedSet }


debugError : Model -> String -> a -> ( Model, Cmd msg )
debugError model place err =
    let
        _ =
            Debug.log ("error at " ++ place ++ ":") err
    in
        ( model, Cmd.none )


type alias ReloadAction =
    Result Http.Error ( Results, String, Maybe (List Page), Maybe Int ) -> Msg


loadRunThenPage : ReloadAction -> Maybe String -> Maybe Int -> Maybe String -> Cmd Msg
loadRunThenPage action maybeTestName maybeGroupSet maybePageName =
    getManyRuns (Maybe.withDefault "__" maybeTestName) maybeGroupSet
        |> Http.toTask
        |> Task.andThen
            (\({ runs, pages } as results) ->
                case maybePageName of
                    Nothing ->
                        Task.succeed (Ok ( results, "", Nothing, maybeGroupSet ))

                    Just page ->
                        getPageByRuns runs page
                            |> Http.toTask
                            |> Task.andThen (\stats -> Task.succeed (Ok ( results, page, Just stats, maybeGroupSet )))
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
            Graph.validGraphs
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
        case model.displayTestResults of
            Nothing ->
                Router.Home

            Just testName ->
                case graphIndex of
                    Nothing ->
                        Router.Home

                    Just ( index, Graph.Scatter _ _ _ _ ) ->
                        Router.SimplePlot
                            index
                            model.displayTestResults
                            page
                            (model.selectedGroupSet |> Maybe.map .id)

                    Just ( index, _ ) ->
                        Router.ComparisonPlot
                            index
                            level
                            model.displayTestResults
                            page
                            (model.selectedGroupSet |> Maybe.map .id)


updateTheUrl : Model -> ( Model, Cmd Msg )
updateTheUrl m =
    ( m, (buildPlotRoute m |> Router.modifyUrl) )



-----------------------
-- HTTP Requests
-----------------------


{-| Returns a Request object that can be used to load a list of run statistics
-}
getManyRuns : String -> Maybe Int -> Http.Request Results
getManyRuns match groupSet =
    let
        groupSetParam =
            case groupSet of
                Just set ->
                    "&groupSet=" ++ Http.encodeUri (fromInt set)

                _ ->
                    ""
    in
        Http.get ("/runs/rollup/?groupLimit=30&match=" ++ Http.encodeUri match ++ groupSetParam) decodeResults


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



-----------------------------------
-- View Logic
-----------------------------------


{-| Takes a model state and renders all the HTML in the page out of it
-}
view : Model -> Html Msg
view model =
    div []
        [ div [ class "view" ]
            [ div [ class "view--left" ]
                [ Lazy.lazy3
                    leftPanel
                    model.currentScreen
                    model.displayTestResults
                    model.serverState.testList
                ]
            , div [ class "view--right" ]
                [ case model.currentScreen of
                    PlotScreen ->
                        rightPlotPanel model

                    ScheduleRunScreen ->
                        Html.map SchedulerMsg <|
                            Scheduler.view
                                model.serverState.testList
                                model.serverState.groupSets
                                model.schedulerState
                ]
            ]
        ]


{-| Renders the left panel (where the list of tests is)
-}
leftPanel : Screen -> Maybe String -> ServerState.TestList -> Html Msg
leftPanel screen defaultTitle (ServerState.TestList runTitles) =
    let
        bottomItems =
            case screen of
                ScheduleRunScreen ->
                    []

                PlotScreen ->
                    [ div [ class "view-header__search" ]
                        [ ul
                            [ class "view-header__runs" ]
                            (runTitles
                                |> Dict.toList
                                |> List.map (runListItem defaultTitle)
                            )
                        ]
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


runListItem : Maybe String -> ( String, ServerState.RunStatus ) -> Html Msg
runListItem selected ( title, status ) =
    let
        classes =
            [ ( "selected", selected == Just title ) ]

        indicator =
            case status of
                ServerState.Running ->
                    text " ▶️"

                ServerState.Done ->
                    text " ✅"

                _ ->
                    text ""
    in
        li [ onClick (RunTitleClicked title) ] [ a [ classList classes ] [ text title, indicator ] ]


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
                        Graph.view
                            model.graphState
                            model.graph
                            (indexedBy .id model.currentRunGroups)
                            (selectRunData model)
                    , Html.map RunGroupPanelMsg <|
                        Lazy.lazy3 RunGroupPanel.view
                            model.currentRunGroups
                            model.filteredGroups
                            model.runGroupsPanel
                    ]
                , div [ class "view-plot--right" ] [ rightmostPanel model ]
                ]


indexedBy : (v -> comparable) -> List v -> Dict comparable v
indexedBy getter list =
    list
        |> List.map (\elem -> ( getter elem, elem ))
        |> Dict.fromList


selectRunData : Model -> Dict Int (List Run)
selectRunData { selectedPage, filteredGroups, runs } =
    let
        groups =
            filteredGroups
                |> indexedBy .id

        existsInFiltered runGroupId =
            case Dict.get runGroupId groups of
                Nothing ->
                    Nothing

                Just _ ->
                    Just runGroupId
    in
        runs
            |> pageOrRunData selectedPage
            |> filterGroupBy (.run >> .runGroupId >> existsInFiltered)


pageOrRunData : Maybe PageSelection -> List Run -> List Run
pageOrRunData pages runs =
    case pages of
        Nothing ->
            runs

        Just (PageSelection _ pageStats) ->
            runs
                |> List.filterMap
                    (\r ->
                        case Dict.get r.run.id pageStats of
                            Nothing ->
                                Nothing

                            Just { stats } ->
                                -- Replace the run stats with the page stats
                                Just { r | stats = stats }
                    )


{-| Renders the right column where the graph selector and lists of pages reside
-}
rightmostPanel : Model -> Html Msg
rightmostPanel { graph, currentRunGroups, filteredGroups, runs, pages, concurrencyComparison, selectedPage, serverState, selectedGroupSet, availableGroupSets } =
    div []
        [ groupSetFilter selectedGroupSet availableGroupSets
        , graphTypeOptions graph
        , concurrencySelector concurrencyComparison runs
        , Lazy.lazy2 runGroupsList currentRunGroups filteredGroups
        , h4 [] [ text "Pages" ]
        , pagesList (extractSelectedPage selectedPage) pages
        , div [ class "scrollFader" ] []
        ]


groupSetFilter : Maybe GroupSet -> List GroupSet -> Html Msg
groupSetFilter selectedSet sets =
    select [ onChange (String.toInt >> Result.withDefault -1 >> ChangeGroupSet) ]
        ([ option [ value "" ] [ text "- All Sets -" ] ] ++ (List.map (groupSetOption selectedSet) sets))


groupSetOption : Maybe GroupSet -> GroupSet -> Html Msg
groupSetOption selectedGroupSet { id, name } =
    let
        isSelected =
            selectedGroupSet
                |> Maybe.map (\s -> s.id == id)
                |> Maybe.withDefault False
    in
        option [ value <| fromInt id, selected isSelected ] [ text name ]


graphTypeOptions : Title -> Html Msg
graphTypeOptions graph =
    select [ onChange ChangeGraphType ] (List.map (graphTypeOption graph) Graph.validGraphs)


graphTypeOption : Title -> ( Title, Graph.Graph ) -> Html msg
graphTypeOption current ( title, _ ) =
    option [ value title, selected (current == title) ] [ text title ]


concurrencySelector : Maybe Int -> List Run -> Html Msg
concurrencySelector current runs =
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


runGroupsList : List RunGroup -> List RunGroup -> Html Msg
runGroupsList allGroups filteredGroups =
    ul []
        (allGroups
            |> List.sortBy .id
            |> List.reverse
            |> List.map (groupItem filteredGroups)
        )


groupItem : List RunGroup -> RunGroup -> Html Msg
groupItem filtered runGroup =
    let
        isFiltered =
            case filtered of
                [] ->
                    False

                _ ->
                    filtered
                        |> List.filter ((==) runGroup)
                        |> List.isEmpty

        color =
            pickColor runGroup.id
    in
        li
            [ style [ ( "cursor", "pointer" ) ]
            , classList [ ( "grayed-out", isFiltered ) ]
            , onClick (ToggleFilterGroup runGroup.id)
            , onDoubleClick (ShowOnlyGroup runGroup.id)
            ]
            [ span
                [ style [ ( "color", colorToHex color ), ( "font-size", "25px" ) ] ]
                [ text "●", text " " ]
            , text runGroup.title
            ]


pagesList : String -> Dict String (List Int) -> Html Msg
pagesList selected pages =
    ul [ class "view-plot--right__pages" ] (List.map (pageItem selected) (Dict.keys pages))


pageItem : String -> String -> Html Msg
pageItem selected page =
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
