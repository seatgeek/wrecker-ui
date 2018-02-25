module Graph.Types exposing (..)

import Data exposing (Run, RunGroup)


{-| Creates a Title alias to easier identify a string it refers to a graph title
-}
type alias Title =
    String


{-| A type with instructions for displaying the legend for the y axis
-}
type YLegend
    = YLegend String (Run -> String)


{-| A type with instructions for displaying the legend for the x axis
-}
type XLegend
    = XLegend String (Run -> String)


{-| Creates a Title alias to easier identify a function as a data getter
to be used for coordinates in the X axis.
-}
type XValueGetter
    = XValueGetter (Run -> Float)


{-| Creates a Title alias to easier identify a function as a data getter
to be used for coordinates in the Y axis.
-}
type YValueGetter
    = YValueGetter (Run -> Float)
