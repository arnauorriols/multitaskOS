module EditableElement
    exposing
        ( config
        , State
        , Mode(ReadMode, EditMode)
        , initialState
        , triggerEditMode
        , editTagId
        , getMode
        , isInEditMode
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
        , submitOnEnter : Bool
        }


config :
    { stateMsg : ( Cmd msg, State ) -> msg
    , editEnabled : Bool
    , submitOnEnter : Bool
    }
    -> Config msg
config { stateMsg, editEnabled, submitOnEnter } =
    Config
        { stateMsg = stateMsg
        , editEnabled = editEnabled
        , submitOnEnter = submitOnEnter
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
getMode (Config { stateMsg, editEnabled, submitOnEnter }) state =
    case ( editEnabled, state ) of
        ( True, Editing _ ) ->
            EditMode
                ([ Html.Attributes.id editTagId
                 , Html.Events.onBlur (stateMsg ( Cmd.none, Viewing ))
                 ]
                    ++ if submitOnEnter then
                        [ Utils.onEnter (stateMsg ( Cmd.none, state )) (stateMsg ( Cmd.none, Viewing )) ]
                       else
                        []
                )

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


isInEditMode : State -> Bool
isInEditMode state =
    case state of
        Editing _ ->
            True

        Viewing ->
            False


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
