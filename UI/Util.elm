module Util exposing (..)

import String.Extra exposing (leftOf)


natSort : String -> String -> Order
natSort a b =
    case ( String.toInt (leftOf " -" a), String.toInt (leftOf " -" b) ) of
        ( Ok aOk, Ok bOk ) ->
            compare aOk bOk

        _ ->
            compare a b
