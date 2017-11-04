module Utils exposing (onEnter, onChange, isSmallScreen)

import Html
import Html.Events
import Json.Decode
import Window


onEnter : msg -> msg -> Html.Attribute msg
onEnter noop action =
    let
        matchOnlyEnter : Int -> Bool -> Bool -> Bool -> Bool
        matchOnlyEnter keycode shift ctrl alt =
            if keycode == 13 && not shift && not ctrl && not alt then
                True
            else
                False

        preventDefaultHack success =
            if success then
                Json.Decode.succeed action
            else
                Json.Decode.fail "This failure is a workaround to avoid prevent default in keycodes != 13"

        decoder =
            (Json.Decode.map4
                matchOnlyEnter
                (Json.Decode.field "keyCode" Json.Decode.int)
                (Json.Decode.field "shiftKey" Json.Decode.bool)
                (Json.Decode.field "ctrlKey" Json.Decode.bool)
                (Json.Decode.field "altKey" Json.Decode.bool)
            )
                |> Json.Decode.andThen preventDefaultHack
    in
        Html.Events.onWithOptions
            "keydown"
            { stopPropagation = False, preventDefault = True }
            decoder


onChange : (String -> msg) -> Html.Attribute msg
onChange toMsg =
    Html.Events.targetValue
        |> Json.Decode.map toMsg
        |> Html.Events.on "change"


isSmallScreen : Window.Size -> Bool
isSmallScreen windowSize =
    windowSize.width < 550
