module Utils exposing (onEnter)

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
