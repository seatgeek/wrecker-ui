module Scheduler exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import String.Extra exposing (fromInt)
import Util exposing (natSort)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Return exposing (command, andThen, mapCmd)
import Util exposing (return)


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


{-| It is possibible to parametrize test runs in the server. This type contains the fields
that can be parametrized for each run.
-}
type alias SchedulerOptions =
    { testTitle : String
    , annotationTitle : String
    , concurrencyStart : Int
    , concurrencyEnd : Int
    , stepSize : Int
    , runTime : Int
    , notes : String
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
    | RunTime
    | Notes


type Msg
    = TestTitleClicked String
    | LoadTestSchedule (Result Http.Error TestList)
    | SchedulerFieldChanged SchedulerField String
    | ScheduleTestClicked
    | ScheduleTestSent (Result Http.Error Decode.Value)


type alias Model =
    { testList : TestList
    , schedulerOptions : Maybe SchedulerOptions
    }


defaultModel : Model
defaultModel =
    { testList = TestList Dict.empty
    , schedulerOptions = Nothing
    }


init : ( Model, Cmd Msg )
init =
    defaultModel
        |> return
        |> command (Http.send LoadTestSchedule getTestList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadTestSchedule (Err error) ->
            -- If there was an error loading the test schedule, we just log it in the console
            ( Debug.log (toString error) model, Cmd.none )

        LoadTestSchedule (Ok tests) ->
            -- After getting the ajax results for the test schedule we need do nothing else but store them
            return { model | testList = tests }

        TestTitleClicked title ->
            -- In the Scheduling view, when the user clicks a test title, we populate the default
            -- scheduling options to show in the form.
            let
                schedulerOptions =
                    model.schedulerOptions
                        |> Maybe.map (\s -> { s | testTitle = title })
                        |> Maybe.withDefault (defaultOptions title)
            in
                return { model | schedulerOptions = Just schedulerOptions }

        SchedulerFieldChanged field value ->
            -- This is a catch-all for any changes done to the fields in the scheduling options form.
            -- depending on the value for the field parameter, we update a different field in the scheduling
            -- options record.
            let
                schedulerOptions =
                    model.schedulerOptions
                        |> Maybe.map (updateOptions field value)
            in
                return { model | schedulerOptions = schedulerOptions }

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
                return { model | schedulerOptions = Nothing }

        ScheduleTestSent _ ->
            -- If the test is created successfully in the scheduler, we re-load the scheduler information
            -- from the server in order to reflect the new status in the list of tests.
            { model | schedulerOptions = Nothing }
                |> return
                |> command (Http.send LoadTestSchedule getTestList)


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
                , Http.stringPart "notes" options.notes
                , Http.stringPart "groupName" options.annotationTitle
                , Http.stringPart "concurrencyStart" (fromInt options.concurrencyStart)
                , Http.stringPart "concurrencyEnd" (fromInt options.concurrencyEnd)
                , Http.stringPart "stepSize" (fromInt options.stepSize)
                , Http.stringPart "runTime" (options.runTime |> fromInt)
                ]
    in
        Http.post "/test-list" body Decode.value


defaultOptions : String -> SchedulerOptions
defaultOptions title =
    SchedulerOptions title "" 10 300 10 10 "No Notes"


updateOptions : SchedulerField -> String -> SchedulerOptions -> SchedulerOptions
updateOptions field value opts =
    case field of
        AnnotationTitle ->
            { opts | annotationTitle = value }

        Notes ->
            { opts | notes = value }

        ConcurrencyStart ->
            { opts | concurrencyStart = Result.withDefault opts.concurrencyStart (String.toInt value) }

        ConcurrencyEnd ->
            { opts | concurrencyEnd = Result.withDefault opts.concurrencyEnd (String.toInt value) }

        StepSize ->
            { opts | stepSize = Result.withDefault opts.stepSize (String.toInt value) }

        RunTime ->
            { opts | runTime = String.toInt value |> Result.withDefault opts.runTime }


view : Model -> Html Msg
view { testList, schedulerOptions } =
    let
        testNames (TestList list) =
            list
                |> Dict.toList
                |> List.sortWith (\( a, _ ) ( b, _ ) -> natSort a b)

        active =
            schedulerOptions
                |> Maybe.map .testTitle
                |> Maybe.withDefault ""
    in
        div [ class "view-plot view-plot__closed" ]
            [ div [ class "view-plot--left" ]
                [ ul []
                    (List.map (buildTestItems TestTitleClicked active) (testNames testList))
                ]
            , div [ class "view-plot--right view-plot--right__wide" ]
                [ case schedulerOptions of
                    Nothing ->
                        text ""

                    Just opts ->
                        renderOptions opts
                ]
            ]


buildTestItems : (String -> msg) -> String -> ( String, RunStatus ) -> Html msg
buildTestItems clickMsg selected ( title, status ) =
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
            [ a [ onClick (clickMsg title), classList classes ] [ text title ]
            , showStatus status
            ]


renderOptions : SchedulerOptions -> Html Msg
renderOptions options =
    let
        duration =
            options.runTime
                |> fromInt

        emptyAnnotation =
            String.trim options.annotationTitle == ""

        buttonOpts =
            if emptyAnnotation then
                []
            else
                [ onClick ScheduleTestClicked ]
    in
        div [ class "view-plot--right__concurrency" ]
            [ label [ classList [ ( "field-error", emptyAnnotation ) ] ]
                [ span [] [ text "Test Annotation *" ]
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
                [ span [] [ text "Duration (secs)" ]
                , input [ onInput (SchedulerFieldChanged RunTime), type_ "number", value duration ] []
                ]
            , label []
                [ span [ style [ ( "width", "100%" ) ] ] [ text "Test Notes (Markdown will be rendered)" ]
                , textarea
                    [ onInput (SchedulerFieldChanged Notes)
                    , value (options.notes)
                    , rows 20
                    , style
                        [ ( "width", "640px" )
                        , ( "margin-top", "10px" )
                        ]
                    ]
                    []
                ]
            , label []
                [ button buttonOpts [ text "Schedule Test" ]
                ]
            ]



--------------------
-- JSON
--------------------


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
