port module DirtyHtml.Textarea exposing (view, defaultConfig, config, Msg, update)

import Html
import Html.Events


type alias Tag msg =
    List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg


type Config msg
    = Config
        { toMsg : Msg -> msg
        , customTag : Maybe (Tag msg)
        }


view : Config msg -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
view (Config { toMsg, customTag }) attributes children =
    let
        textarea : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
        textarea =
            case customTag of
                Just tag ->
                    tag

                Nothing ->
                    Html.textarea

        resizeOnFocus =
            Resize
                |> toMsg
                |> Html.Events.onFocus
    in
        textarea
            (resizeOnFocus :: attributes)
            children


defaultConfig : { toMsg : Msg -> msg } -> Config msg
defaultConfig { toMsg } =
    Config
        { toMsg = toMsg
        , customTag = Nothing
        }


config : { toMsg : Msg -> msg, customTag : Tag msg } -> Config msg
config { toMsg, customTag } =
    Config
        { toMsg = toMsg
        , customTag = Just customTag
        }


type Msg
    = Resize


update : Msg -> Cmd Msg
update msg =
    case msg of
        Resize ->
            resizeFocused ()


port resizeFocused : () -> Cmd msg
