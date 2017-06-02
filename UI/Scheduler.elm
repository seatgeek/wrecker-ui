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
    , runTime : Maybe Int
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


type alias Config msg =
    { onSelectItem : String -> msg
    , onFieldChange : SchedulerField -> String -> msg
    , onSubmit : msg
    }


{-| Returns a Request object that can be used to load the list of tests titles. This is
used for displaying the list of runs that can be selected when scheduling a Run.
-}
getTestList : Http.Request TestList
getTestList =
    Http.get "/test-list" decodeTestList


defaultOptions : String -> SchedulerOptions
defaultOptions title =
    SchedulerOptions title "" 10 300 10 Nothing


updateOptions : SchedulerField -> String -> SchedulerOptions -> SchedulerOptions
updateOptions field value opts =
    case field of
        AnnotationTitle ->
            { opts | annotationTitle = value }

        ConcurrencyStart ->
            { opts | concurrencyStart = Result.withDefault opts.concurrencyStart (String.toInt value) }

        ConcurrencyEnd ->
            { opts | concurrencyEnd = Result.withDefault opts.concurrencyEnd (String.toInt value) }

        StepSize ->
            { opts | stepSize = Result.withDefault opts.stepSize (String.toInt value) }

        RunTime ->
            { opts | runTime = String.toInt value |> Result.toMaybe }


view : Config msg -> TestList -> Maybe SchedulerOptions -> Html msg
view config (TestList list) options =
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
            [ div [ class "view-plot--left" ]
                [ ul []
                    (List.map (buildTestItems config.onSelectItem active) testNames)
                ]
            , div [ class "view-plot--right" ]
                [ case options of
                    Nothing ->
                        text ""

                    Just opts ->
                        schedulerOptions config.onFieldChange config.onSubmit opts
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


schedulerOptions : (SchedulerField -> String -> msg) -> msg -> SchedulerOptions -> Html msg
schedulerOptions fieldMsg submitMsg options =
    let
        duration =
            options.runTime
                |> Maybe.map fromInt
                |> Maybe.withDefault ""
    in
        div [ class "view-plot--right__concurrency" ]
            [ label []
                [ span [] [ text "Test Annotation" ]
                , input [ onInput (fieldMsg AnnotationTitle), placeholder "E.g. Migrated to PHP 7" ] []
                ]
            , label []
                [ span [] [ text "Concurrency Start" ]
                , input [ onInput (fieldMsg ConcurrencyStart), type_ "number", value (fromInt options.concurrencyStart) ] []
                ]
            , label []
                [ span [] [ text "Concurrency Target" ]
                , input [ onInput (fieldMsg ConcurrencyEnd), type_ "number", value (fromInt options.concurrencyEnd) ] []
                ]
            , label []
                [ span [] [ text "Step Size" ]
                , input [ onInput (fieldMsg StepSize), type_ "number", value (fromInt options.stepSize) ] []
                ]
            , label []
                [ span [] [ text "Duration (secs)" ]
                , input [ onInput (fieldMsg RunTime), type_ "number", value duration ] []
                ]
            , label []
                [ button [ onClick submitMsg ] [ text "Schedule Test" ]
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
