module RunGroupPanel exposing (view, Model, update, init, Msg)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String.Extra exposing (fromInt)
import Data exposing (RunGroup)
import Markdown
import Date.Format


type alias Model =
    { openPanels : List Int
    }


type alias FilteredGroups =
    List RunGroup


type Msg
    = Expand Int
    | Collapse Int


init : Model
init =
    Model []


update : Msg -> Model -> Model
update msg model =
    case msg of
        Expand panel ->
            { model | openPanels = panel :: model.openPanels }

        Collapse panel ->
            { model | openPanels = model.openPanels |> List.filter ((/=) panel) }


view : List ( String, RunGroup ) -> FilteredGroups -> Model -> Html Msg
view groups filteredGroups { openPanels } =
    div [ class "run-group-panel" ]
        [ h3 [] [ text "Test Runs" ]
        , div [] (List.map (panel openPanels) (onlyFiltered filteredGroups groups))
        ]


onlyFiltered : FilteredGroups -> List ( String, RunGroup ) -> List ( String, RunGroup )
onlyFiltered filtered allGroups =
    allGroups
        |> List.filter (\( _, group ) -> filtered |> List.member group)


panel : List Int -> ( String, RunGroup ) -> Html Msg
panel openPanels ( color, group ) =
    let
        expanded =
            List.member group.id openPanels

        msg =
            if expanded then
                Collapse
            else
                Expand
    in
        div [ class "run-group-panel--group" ]
            [ div [ class "run-group-panel--group--header", onClick (msg group.id) ]
                [ span
                    [ style [ ( "color", color ) ] ]
                    [ text "●", text " " ]
                , text group.title
                ]
            , if expanded then
                panelDetails group
              else
                text ""
            ]


panelDetails : RunGroup -> Html msg
panelDetails group =
    div []
        [ panelItem "Concurrency Start" (fromInt group.concurrencyStart)
        , panelItem "Concurrency Target" (fromInt group.concurrencyTarget)
        , panelItem "Step Size" (fromInt group.rampupStep)
        , panelItem "Step duration time" (fromInt group.runKeepTime ++ " secs")
        , panelItem "Run started" (Date.Format.format "%Y-%m-%d %H:%I:%S" group.created)
        , h5 [ class "run-group-panel--group--notes-title" ] [ text "Notes" ]
        , Markdown.toHtml [ class "run-group-panel--group--notes" ] group.notes
        ]


panelItem : String -> String -> Html msg
panelItem title value =
    div [ class "run-group-panel--group--item" ]
        [ div [ class "run-group-panel--group--title" ] [ text title ]
        , div [ class "run-group-panel--group--value" ] [ text value ]
        ]
