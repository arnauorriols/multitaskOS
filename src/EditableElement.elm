module EditableElement
    exposing
        ( config
        , State
        , Mode(ReadMode, EditMode)
        , initialState
        , triggerEditMode
        , getMode
        , textReadMode
        , markdownReadMode
        , defaultPlaceholder
        )

import Html
import Html.Attributes
import Html.Events
import Task
import Dom
import Markdown
import Utils


type Mode msg
    = ReadMode (List (Html.Attribute msg))
    | EditMode (List (Html.Attribute msg))


type Config msg
    = Config
        { stateMsg : ( Cmd msg, State ) -> msg
        , editEnabled : Bool
        }


config :
    { stateMsg : ( Cmd msg, State ) -> msg
    , editEnabled : Bool
    }
    -> Config msg
config { stateMsg, editEnabled } =
    Config
        { stateMsg = stateMsg
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


editTagId : String
editTagId =
    "editable-element-being-edited"


triggerEditMode : (( Cmd msg, State ) -> msg) -> msg
triggerEditMode stateMsg =
    let
        focusCmd : String -> (( Cmd msg, State ) -> msg) -> Cmd msg
        focusCmd editTagId stateMsg =
            Task.attempt (Focused >> Editing >> (,) Cmd.none >> stateMsg) (Dom.focus editTagId)
    in
        (stateMsg ( focusCmd editTagId stateMsg, Editing Unfocused ))


getMode : Config msg -> State -> Mode msg
getMode (Config { stateMsg, editEnabled }) state =
    case ( editEnabled, state ) of
        ( True, Editing _ ) ->
            EditMode
                [ Html.Attributes.id editTagId
                , Html.Events.onBlur (stateMsg ( Cmd.none, Viewing ))
                , Utils.onEnter (stateMsg ( Cmd.none, state )) (stateMsg ( Cmd.none, Viewing ))
                ]

        ( True, Viewing ) ->
            ReadMode
                [ Html.Events.onClick (triggerEditMode stateMsg)
                , Html.Attributes.style
                    [ ( "cursor", "pointer" )
                    ]
                ]

        ( False, _ ) ->
            ReadMode
                [ Html.Attributes.style
                    [ ( "cursor", "default" )
                    ]
                ]


textReadMode : String -> Html.Html msg
textReadMode content =
    if not (String.isEmpty content) then
        Html.text content
    else
        defaultPlaceholder


markdownOptions : Markdown.Options
markdownOptions =
    { githubFlavored = Just { tables = True, breaks = True }
    , defaultHighlighting = Just "javascript"
    , sanitize = True
    , smartypants = True
    }


markdownReadMode : String -> Html.Html msg
markdownReadMode content =
    if not (String.isEmpty content) then
        Markdown.toHtmlWith markdownOptions [ Html.Attributes.class "markdown" ] content
    else
        defaultPlaceholder


defaultPlaceholder : Html.Html msg
defaultPlaceholder =
    Html.em
        [ Html.Attributes.style
            [ ( "font-size", "0.9em" ) ]
        ]
        [ Html.text "Nothing much -- click to edit" ]
