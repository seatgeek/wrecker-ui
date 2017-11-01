module ServerState exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Data exposing (GroupSet, decodeGroupSet)
import Return exposing (command, andThen, mapCmd)
import Util exposing (return)
import Date


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


type alias State =
    { testList : TestList
    , groupSets : List GroupSet
    }


type Msg
    = LoadTestSchedule (Result Http.Error TestList)
    | LoadGroupSets (Result Http.Error (List GroupSet))


defaultState : State
defaultState =
    State (TestList Dict.empty) []


init : ( State, Cmd Msg )
init =
    defaultState
        |> return
        |> command (Http.send LoadTestSchedule getTestList)
        |> command (Http.send LoadGroupSets getGroupSets)


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        LoadTestSchedule (Err error) ->
            -- If there was an error loading the test schedule, we just log it in the console
            ( Debug.log (toString error) model, Cmd.none )

        LoadTestSchedule (Ok tests) ->
            -- After getting the ajax results for the test schedule we need do nothing else but store them
            return { model | testList = tests }

        LoadGroupSets (Err error) ->
            -- If there was an error loading the test schedule, we just log it in the console
            ( Debug.log (toString error) model, Cmd.none )

        LoadGroupSets (Ok sets) ->
            -- After getting the ajax results for the test schedule we need do nothing else but store them
            return { model | groupSets = sets |> List.sortBy (.created >> Date.toTime) }


{-| Returns a Request object that can be used to load the list of tests titles. This is
used for displaying the list of runs that can be selected when scheduling a Run.
-}
getTestList : Http.Request TestList
getTestList =
    Http.get "/test-list" decodeTestList


{-| Returns a Request object that can be used to load the list of group sets. This is
used for displaying the list of sets that can be used to group test runs.
-}
getGroupSets : Http.Request (List GroupSet)
getGroupSets =
    Http.get "/group-sets" (Decode.at [ "groupSets" ] (Decode.list decodeGroupSet))



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
