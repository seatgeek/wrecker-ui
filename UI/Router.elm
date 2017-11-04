module Router exposing (Route(..), modifyUrl, fromLocation)

import UrlParser as Url exposing (parseHash, s, (<?>), (</>), stringParam, intParam, int, oneOf, Parser)
import Navigation exposing (Location)
import Http
import String.Extra exposing (fromInt)


-- ROUTING --


type alias TestName =
    Maybe String


type alias PageName =
    Maybe String


type alias PlotType =
    Int


type alias ConcurrencyLevel =
    Int


type alias GroupSetId =
    Maybe Int


type Route
    = Home
    | ScheduleTest
    | SimplePlot PlotType TestName PageName GroupSetId
    | ComparisonPlot PlotType ConcurrencyLevel TestName PageName GroupSetId


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map ScheduleTest (s "schedule")
        , Url.map SimplePlot (s "plot" </> int <?> stringParam "test" <?> stringParam "page" <?> intParam "groupSet")
        , Url.map ComparisonPlot (s "comparison" </> int </> int <?> stringParam "test" <?> stringParam "page" <?> intParam "groupSet")
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        ( path, query ) =
            case page of
                Home ->
                    ( [], [] )

                ScheduleTest ->
                    ( [ "schedule" ], [] )

                SimplePlot plotType testName pageName groupSetId ->
                    ( [ "plot", toString plotType ]
                    , [ ( "test", testName )
                      , ( "page", pageName )
                      , ( "groupSet", groupSetId |> Maybe.map fromInt )
                      ]
                    )

                ComparisonPlot plotType concurrency testName pageName groupSetId ->
                    ( [ "plot", toString plotType, toString concurrency ]
                    , [ ( "test", testName )
                      , ( "page", pageName )
                      , ( "groupSet", groupSetId |> Maybe.map fromInt )
                      ]
                    )
    in
        toQueryString query ++ "#/" ++ (String.join "/" path)


toQueryString : List ( String, Maybe String ) -> String
toQueryString params =
    params
        |> List.filterMap (\( name, val ) -> val |> Maybe.map (\v -> name ++ "=" ++ Http.encodeUri v))
        |> List.foldl (\q acc -> acc ++ q ++ "&") "?"
        |> (\s ->
                if s == "?" then
                    ""
                else
                    s
           )



-- PUBLIC HELPERS --


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.newUrlWithoutNotify


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
