module EditableElement
    exposing
        ( htmlElement
        , config
        , State
        , initialState
        , triggerEditMode
        , view
        )

import Html
import Html.Attributes
import Html.Events
import Task
import Dom


type alias HtmlTag msg =
    List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg


type HtmlElement msg
    = HtmlElement String (HtmlTag msg)


htmlElement : String -> HtmlTag msg -> HtmlElement msg
htmlElement id tag =
    HtmlElement id tag


type Config msg
    = Config
        { readModeTag : HtmlElement msg
        , editModeTag : HtmlElement msg
        , editMsg : String -> msg
        , stateMsg : ( Cmd msg, State ) -> msg
        , editEnabled : Bool
        }


config :
    { readModeTag : HtmlElement msg
    , editModeTag : HtmlElement msg
    , editMsg : String -> msg
    , stateMsg : ( Cmd msg, State ) -> msg
    , editEnabled : Bool
    }
    -> Config msg
config { readModeTag, editModeTag, editMsg, stateMsg, editEnabled } =
    Config
        { readModeTag = readModeTag
        , editModeTag = editModeTag
        , editMsg = editMsg
        , stateMsg = stateMsg
        , editEnabled = editEnabled
        }


type State
    = Viewing
    | Editing FocusState


type FocusState
    = Unfocused
    | Focused (Result Dom.Error ())


initialState : State
initialState =
    Viewing


triggerEditMode : String -> (( Cmd msg, State ) -> msg) -> msg
triggerEditMode editTagId stateMsg =
    let
        focusCmd : String -> (( Cmd msg, State ) -> msg) -> Cmd msg
        focusCmd editTagId stateMsg =
            Task.attempt (Focused >> Editing >> (,) Cmd.none >> stateMsg) (Dom.focus editTagId)
    in
        (stateMsg ( focusCmd editTagId stateMsg, Editing Unfocused ))


view : Config msg -> State -> String -> Html.Html msg
view (Config { readModeTag, editModeTag, editMsg, stateMsg, editEnabled }) state content =
    let
        ( readTag, readTagId, editTag, editTagId ) =
            case ( readModeTag, editModeTag ) of
                ( HtmlElement readTagId readTag, HtmlElement editTagId editTag ) ->
                    ( readTag, readTagId, editTag, editTagId )

        readViewContent =
            if not (String.isEmpty content) then
                Html.text content
            else
                Html.em
                    [ Html.Attributes.style
                        [ ( "font-size", "0.9em" ) ]
                    ]
                    [ Html.text "Nothing much -- click to edit" ]
    in
        case ( editEnabled, state ) of
            ( True, Editing _ ) ->
                editTag
                    ([ Html.Attributes.id editTagId
                     , Html.Attributes.type_ "text"
                     , Html.Attributes.value content
                     , Html.Events.onInput editMsg
                     , Html.Events.onBlur (stateMsg ( Cmd.none, Viewing ))
                     ]
                    )
                    []

            ( True, Viewing ) ->
                readTag
                    [ Html.Attributes.id readTagId
                    , Html.Events.onClick (triggerEditMode editTagId stateMsg)
                    , Html.Attributes.style
                        [ ( "cursor", "pointer" )
                        ]
                    ]
                    [ readViewContent ]

            ( False, _ ) ->
                readTag
                    [ Html.Attributes.id readTagId
                    , Html.Attributes.style
                        [ ( "cursor", "default" )
                        ]
                    ]
                    [ readViewContent ]
