module Graph.Coloring exposing (pickColor)

import LineChart.Colors as Colors exposing (..)
import List.Extra exposing (getAt)


{-| Returns the list of colors that can be assigned to the groups.
Unfortunately I only found a reduced list of nice colors to use, so
we cannot display more runs than colors in this list!
-}
allColors =
    [ pink
    , blue
    , gold
    , red
    , green
    , cyan
    , teal
    , purple
    , pinkLight
    , blueLight
    , goldLight
    , redLight
    , greenLight
    , cyanLight
    , tealLight
    , purpleLight
    , black
    , gray
    , grayLight
    ]


totalColors : Int
totalColors =
    List.length allColors


pickColor num =
    let
        index =
            num % totalColors
    in
        allColors
            |> getAt index
            |> Maybe.withDefault blue
