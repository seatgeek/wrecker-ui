module Scheduler exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import String.Extra exposing (fromInt)
import Util exposing (natSort)
import Http
import Json.Decode as Decode
import Data exposing (GroupSet, decodeGroupSet)
import ServerState exposing (TestList(..), RunStatus(..))


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
    , groupSetId : Maybe Int
    }


{-| We can create group sets form the interface. This type contains the params that need be
sent to the server to create a new one.
-}
type alias GroupSetOptions =
    { name : String
    , description : String
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
    | GroupSetButtonClicked
    | GroupSetNameChanged String
    | GroupSetDescriptionChanged String
    | GroupSetSaveClicked
    | GroupSetSent (Result Http.Error Decode.Value)
    | SchedulerFieldChanged SchedulerField String
    | SchedulerGroupSetIdSet Int
    | ScheduleTestClicked
    | ScheduleTestSent (Result Http.Error Decode.Value)


type alias Model =
    { schedulerOptions : Maybe SchedulerOptions
    , showGroupsetForm : Bool
    , groupSetOptions : GroupSetOptions
    , groupSetSaving : Bool
    }


defaultModel : Model
defaultModel =
    { schedulerOptions = Nothing
    , showGroupsetForm = False
    , groupSetOptions = GroupSetOptions "" ""
    , groupSetSaving = False
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


update : List GroupSet -> Msg -> Model -> ( Model, Cmd Msg, Bool )
update groupSets msg model =
    case msg of
        TestTitleClicked title ->
            -- In the Scheduling view, when the user clicks a test title, we populate the default
            -- scheduling options to show in the form.
            let
                schedulerOptions =
                    model.schedulerOptions
                        |> Maybe.map (\s -> { s | testTitle = title })
                        |> Maybe.withDefault (defaultOptions title groupSets)
            in
                ( { model | schedulerOptions = Just schedulerOptions, showGroupsetForm = False }, Cmd.none, False )

        SchedulerFieldChanged field value ->
            -- This is a catch-all for any changes done to the fields in the scheduling options form.
            -- depending on the value for the field parameter, we update a different field in the scheduling
            -- options record.
            let
                schedulerOptions =
                    model.schedulerOptions
                        |> Maybe.map (updateOptions field value)
            in
                ( { model | schedulerOptions = schedulerOptions }, Cmd.none, False )

        SchedulerGroupSetIdSet id ->
            let
                schedulerOptions =
                    model.schedulerOptions
                        |> Maybe.map (\opts -> { opts | groupSetId = Just id })
            in
                ( { model | schedulerOptions = schedulerOptions }, Cmd.none, False )

        ScheduleTestClicked ->
            -- Once the schedule test button is clicked, we take the scheduling options record and post it
            -- using a HTTP request, so that the test is run in the server.
            let
                effect =
                    model.schedulerOptions
                        |> Maybe.map (\o -> Http.send ScheduleTestSent (postTestSchedule o))
                        |> Maybe.withDefault Cmd.none
            in
                ( model, effect, False )

        ScheduleTestSent (Err err) ->
            -- If we got an error when sending the test to the scheduler, we log the error, but also
            -- hide the scheduler form.
            let
                _ =
                    Debug.log "ScheduleTestSent" err
            in
                ( { model | schedulerOptions = Nothing }, Cmd.none, False )

        ScheduleTestSent _ ->
            -- If the test is created successfully in the scheduler, we re-load the scheduler information
            -- from the server in order to reflect the new status in the list of tests.
            ( { model | schedulerOptions = Nothing }, Cmd.none, True )

        GroupSetButtonClicked ->
            ( { model | showGroupsetForm = True }, Cmd.none, False )

        GroupSetNameChanged name ->
            let
                groupSetOptions =
                    model.groupSetOptions
            in
                ( { model | groupSetOptions = { groupSetOptions | name = name } }, Cmd.none, False )

        GroupSetDescriptionChanged desc ->
            let
                groupSetOptions =
                    model.groupSetOptions
            in
                ( { model | groupSetOptions = { groupSetOptions | description = desc } }, Cmd.none, False )

        GroupSetSaveClicked ->
            -- Once the schedule save button is clicked, we take the group set options record and post it
            -- using a HTTP request, so that it gets saved.
            let
                effect =
                    if
                        String.trim model.groupSetOptions.name
                            /= ""
                            && String.trim model.groupSetOptions.description
                            /= ""
                            && not model.groupSetSaving
                    then
                        Http.send GroupSetSent (postGroupSet model.groupSetOptions)
                    else
                        Cmd.none
            in
                ( { model | groupSetSaving = True }, effect, False )

        GroupSetSent (Err err) ->
            -- If we got an error when sending the test to the scheduler, we log the error, but also
            -- hide the scheduler form.
            let
                _ =
                    Debug.log "GroupSetSent" err
            in
                ( { model | groupSetSaving = False }, Cmd.none, False )

        GroupSetSent _ ->
            -- If the group set is created successfully in the scheduler, we re-load the list of group sets
            -- from the server.
            ( { model | groupSetOptions = GroupSetOptions "" "", showGroupsetForm = False, groupSetSaving = False }
            , Cmd.none
            , True
            )


postTestSchedule : SchedulerOptions -> Http.Request Decode.Value
postTestSchedule options =
    let
        groupPart =
            options.groupSetId
                |> Maybe.map (fromInt >> Http.stringPart "groupSetId")
                |> Maybe.map (\p -> [ p ])
                |> Maybe.withDefault []

        body =
            Http.multipartBody
                ([ Http.stringPart "testTitle" options.testTitle
                 , Http.stringPart "notes" options.notes
                 , Http.stringPart "groupName" options.annotationTitle
                 , Http.stringPart "concurrencyStart" (fromInt options.concurrencyStart)
                 , Http.stringPart "concurrencyEnd" (fromInt options.concurrencyEnd)
                 , Http.stringPart "stepSize" (fromInt options.stepSize)
                 , Http.stringPart "runTime" (options.runTime |> fromInt)
                 ]
                    ++ groupPart
                )
    in
        Http.post "/test-list" body Decode.value


postGroupSet : GroupSetOptions -> Http.Request Decode.Value
postGroupSet options =
    let
        body =
            Http.multipartBody
                [ Http.stringPart "name" options.name
                , Http.stringPart "description" options.description
                ]
    in
        Http.post "/group-sets" body Decode.value


defaultOptions : String -> List GroupSet -> SchedulerOptions
defaultOptions title groupSets =
    SchedulerOptions title "" 10 300 10 10 "No Notes" (List.head groupSets |> Maybe.map .id)


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



--------------------
-- VIEW
--------------------


view : TestList -> List GroupSet -> Model -> Html Msg
view testList groupSets { schedulerOptions, showGroupsetForm, groupSetOptions } =
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
                [ div [ class "group-set-button" ] [ a [ onClick GroupSetButtonClicked ] [ text "⛱ Add GroupSet" ] ]
                , h4 [] [ text "...or schedule a test 👩\x200D🔬" ]
                , ul []
                    (List.map (buildTestItems TestTitleClicked active) (testNames testList))
                ]
            , div [ class "view-plot--right view-plot--right__wide" ]
                [ case ( schedulerOptions, showGroupsetForm ) of
                    ( Just opts, False ) ->
                        renderOptions opts groupSets

                    ( _, True ) ->
                        renderGroupSetForm groupSetOptions

                    _ ->
                        text ""
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


renderOptions : SchedulerOptions -> List GroupSet -> Html Msg
renderOptions options groupSets =
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
            [ label []
                [ span [] [ text "Test Set" ]
                , select [] (List.map groupSetOption groupSets)
                ]
            , label [ classList [ ( "field-error", emptyAnnotation ) ] ]
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


groupSetOption : GroupSet -> Html Msg
groupSetOption groupSet =
    option [ value (toString groupSet.id) ] [ text groupSet.name ]


renderGroupSetForm : GroupSetOptions -> Html Msg
renderGroupSetForm opts =
    let
        hasName =
            String.trim opts.name /= ""

        hasDesc =
            String.trim opts.description /= ""

        buttonOpts =
            if not (hasName && hasDesc) then
                []
            else
                [ onClick GroupSetSaveClicked ]
    in
        div [ class "view-plot--right__concurrency" ]
            [ label [ classList [ ( "field-error", not hasName ) ] ]
                [ span [] [ text "Name *" ]
                , input
                    [ onInput GroupSetNameChanged
                    , placeholder "E.g. Changing EC2 instance sizes"
                    , style
                        [ ( "width"
                          , "60%"
                          )
                        ]
                    ]
                    []
                ]
            , label [ classList [ ( "field-error", not hasDesc ) ] ]
                [ span [ style [ ( "width", "100%" ) ] ] [ text "Group Notes (Markdown will be rendered) *" ]
                , textarea
                    [ onInput GroupSetDescriptionChanged
                    , rows 20
                    , style
                        [ ( "width", "640px" )
                        , ( "margin-top", "10px" )
                        ]
                    ]
                    []
                ]
            , label []
                [ button buttonOpts [ text "Save Group" ]
                ]
            ]
