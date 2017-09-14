module Utils exposing (onEnter, onChange)

import Html
import Html.Events
import Json.Decode


onEnter : msg -> msg -> Html.Attribute msg
onEnter noop action =
    let
        tagger code =
            if code == 13 then
                action
            else
                noop
    in
        Html.Events.on "keydown" <| Json.Decode.map tagger Html.Events.keyCode


onChange : (String -> msg) -> Html.Attribute msg
onChange toMsg =
    Html.Events.targetValue
        |> Json.Decode.map toMsg
        |> Html.Events.on "change"
