port module DirtyHtml.Textarea exposing (view, config, Msg, update)

import Html
import Html.Events


type Config msg
    = Config
        { toMsg : Msg -> msg
        }


view : Config msg -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
view (Config { toMsg }) attributes children =
    let
        resizeOnFocus =
            Resize
                |> toMsg
                |> Html.Events.onFocus
    in
        Html.textarea
            (resizeOnFocus :: attributes)
            children


config : { toMsg : Msg -> msg } -> Config msg
config { toMsg } =
    Config
        { toMsg = toMsg
        }


type Msg
    = Resize


update : Msg -> Cmd Msg
update msg =
    case msg of
        Resize ->
            resizeFocused ()


port resizeFocused : () -> Cmd msg
