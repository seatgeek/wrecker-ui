module Util exposing (..)

import String.Extra exposing (leftOf)
import Return
import Html exposing (Attribute)
import Html.Events exposing (on, keyCode)
import Json.Decode as Decode


return : model -> Return.Return msg model
return =
    Return.singleton


natSort : String -> String -> Order
natSort a b =
    case ( String.toInt (leftOf " -" a), String.toInt (leftOf " -" b) ) of
        ( Ok aOk, Ok bOk ) ->
            compare aOk bOk

        _ ->
            compare a b



---------------------------------------------
--- View Utilities
---------------------------------------------


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        on "keydown" (keyCode |> Decode.andThen isEnter)


onChange : (String -> msg) -> Attribute msg
onChange msg =
    let
        action =
            Decode.at [ "target", "value" ] Decode.string
                |> Decode.map msg
    in
        on "change" action


onChangeInt : (Int -> msg) -> Attribute msg
onChangeInt msg =
    let
        action =
            Decode.at [ "target", "value" ] Decode.string
                |> Decode.andThen
                    (\txt ->
                        case String.toInt txt of
                            Ok int ->
                                Decode.succeed int

                            Err e ->
                                Decode.fail e
                    )
                |> Decode.map msg
    in
        on "change" action
