module Job
    exposing
        ( Model
        , Msg
        , init
        , encode
        , decoder
        , isValid
        , update
        , focusWorklogForm
        , triggerTitleEditMode
        , isEditingWorklogEntry
        , viewTitle
        , viewWorklog
        , viewWorklogForm
        )

import String
import Json.Decode
import Json.Encode
import Html exposing (..)
import Html.Keyed
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List.Extra
import Dom
import Task
import Window
import Update.Extra
import EditableElement
import Utils
import Helpcard
import DirtyHtml.Textarea


-- MODEL


{-| A job has a name, a journal, and the possibility to add new entries
to the journal by means of a worklog form.
-}
type alias Model =
    { title : String
    , titleWidgetState : EditableElement.State
    , worklog : Worklog
    }


type alias Worklog =
    List WorklogEntry


type alias WorklogEntry =
    ( String, EditableElement.State )


{-| Create a new empty Job
-}
init : Model
init =
    { title = ""
    , titleWidgetState = EditableElement.initialState
    , worklog = initWorklog
    }


{-| Encode a job model
-}
encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "title", Json.Encode.string model.title )
        , ( "worklog", Json.Encode.list (List.map (Tuple.first >> Json.Encode.string) model.worklog) )
        ]


{-| Decode a job model
-}
decoder : Json.Decode.Decoder Model
decoder =
    Json.Decode.map3
        Model
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.succeed EditableElement.initialState)
        (Json.Decode.field "worklog"
            (Json.Decode.list
                (Json.Decode.map2
                    (,)
                    (Json.Decode.string)
                    (Json.Decode.succeed EditableElement.initialState)
                )
            )
        )


{-| Assert the Job is valid and can be scheduled
-}
isValid : Model -> Bool
isValid model =
    not (String.isEmpty model.title)


initWorklog : Worklog
initWorklog =
    [initWorklogEntry]


initWorklogEntry : WorklogEntry
initWorklogEntry =
    ( "", EditableElement.initialState )


savedWorklog : Worklog -> Worklog
savedWorklog worklog =
    case List.tail worklog of
        Just w ->
            w

        Nothing ->
            []


unsavedWorklogEntry : Worklog -> WorklogEntry
unsavedWorklogEntry worklog =
    case List.head worklog of
        Just e ->
            e

        Nothing ->
            initWorklogEntry


updateWorklogEntry : Int -> WorklogEntry -> Worklog -> Worklog
updateWorklogEntry index worklogEntry worklog =
    case List.Extra.setAt index worklogEntry worklog of
        Just newWorklog ->
            newWorklog

        Nothing ->
            worklogEntry :: worklog


editWorklogEntryContent : Int -> String -> Worklog -> Worklog
editWorklogEntryContent index worklogEntryContent worklog =
    let
        worklogEntryWidgetState =
            case List.Extra.getAt index worklog of
                Just worklogEntry ->
                    Tuple.second worklogEntry

                Nothing ->
                    EditableElement.initialState

        worklogEntry =
            ( worklogEntryContent, worklogEntryWidgetState )
    in
        updateWorklogEntry index worklogEntry worklog


updateWorklogEntryWidgetState : Int -> EditableElement.State -> Worklog -> Worklog
updateWorklogEntryWidgetState index worklogEntryWidgetState worklog =
    let
        worklogEntryContent =
            case List.Extra.getAt index worklog of
                Just worklogEntry ->
                    Tuple.first worklogEntry

                Nothing ->
                    ""

        worklogEntry =
            ( worklogEntryContent, worklogEntryWidgetState )
    in
        updateWorklogEntry index worklogEntry worklog



-- UPDATE


{-| Possible actions that can occur on a job
-}
type Msg
    = NoOp
    | EditTitle String
    | TitleWidget ( Cmd Msg, EditableElement.State )
    | Worklog WorklogMsg
    | Focus DomActionMsg
    | Blur DomActionMsg
    | WorklogEntryTextareaMsg DirtyHtml.Textarea.Msg
    | WorklogInputTextareaMsg DirtyHtml.Textarea.Msg


type WorklogMsg
    = Add
    | Save Int String
    | Delete Int
    | WorklogEntryWidget Int ( Cmd Msg, EditableElement.State )


type DomActionMsg
    = Attempt String
    | Result (Result Dom.Error ())


{-| Handle incoming @docs Msg
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Focus (Attempt elementId) ->
            let
                focusTask =
                    Dom.focus elementId
            in
                ( model, Task.attempt (Result >> Focus) focusTask )

        Focus (Result result) ->
            case result of
                Err (Dom.NotFound id) ->
                    let
                        _ =
                            Debug.log "Element was not found, thus could not be focused" id
                    in
                        ( model, Cmd.none )

                Ok () ->
                    ( model, Cmd.none )

        Blur (Attempt elementId) ->
            let
                blurTask =
                    Dom.blur elementId
            in
                ( model, Task.attempt (Result >> Blur) blurTask )

        Blur (Result result) ->
            case result of
                Err (Dom.NotFound id) ->
                    let
                        _ =
                            Debug.log "Element was not found, thus could not be blurred" id
                    in
                        ( model, Cmd.none )

                Ok () ->
                    ( model, Cmd.none )

        EditTitle newTitle ->
            ( { model | title = newTitle }, Cmd.none )

        TitleWidget ( cmdNextTitleWidgetState, titleWidgetState ) ->
            ( { model | titleWidgetState = titleWidgetState }, cmdNextTitleWidgetState )

        Worklog Add ->
            ( { model | worklog = initWorklogEntry :: model.worklog }, Cmd.none )
                |> Update.Extra.andThen update focusWorklogForm

        Worklog (Save index worklogEntryContent) ->
            ( { model | worklog = editWorklogEntryContent index worklogEntryContent model.worklog }, Cmd.none )

        Worklog (Delete worklogEntryIndex) ->
            ( { model | worklog = List.Extra.removeAt worklogEntryIndex model.worklog }, Cmd.none )

        Worklog (WorklogEntryWidget index ( cmd, state )) ->
            ( { model | worklog = updateWorklogEntryWidgetState index state model.worklog }, cmd )

        WorklogEntryTextareaMsg dirtyTextareaMsg ->
            ( model, Cmd.map WorklogEntryTextareaMsg (DirtyHtml.Textarea.update dirtyTextareaMsg) )

        WorklogInputTextareaMsg dirtyTextareaMsg ->
            ( model, Cmd.map WorklogInputTextareaMsg (DirtyHtml.Textarea.update dirtyTextareaMsg) )


{-| Focus the input field to enter a new worklog entry
-}
focusWorklogForm : Msg
focusWorklogForm =
    Focus (Attempt "input-worklog")


triggerTitleEditMode : Msg
triggerTitleEditMode =
    EditableElement.triggerEditMode TitleWidget



-- VIEW


{-| Present the title of a job
-}
viewTitle : Model -> Html Msg
viewTitle model =
    let
        config =
            EditableElement.config
                { stateMsg = TitleWidget
                , editEnabled = True
                , submitOnEnter = True
                }

        readOrEditTag =
            EditableElement.getMode config model.titleWidgetState
    in
        h4
            [ class "grey-text text-darken-2" ]
            (case readOrEditTag of
                EditableElement.ReadMode attributes ->
                    [ span attributes [ EditableElement.textReadMode model.title ] ]

                EditableElement.EditMode attributes ->
                    [ input
                        (class "title-edit"
                            :: type_ "text"
                            :: value model.title
                            :: onInput EditTitle
                            :: attributes
                        )
                        []
                    ]
            )


{-| Present the list of journal entries of a job
-}
viewWorklog : Window.Size -> Bool -> Model -> Html Msg
viewWorklog windowSize editable model =
    case savedWorklog model.worklog of
        [] ->
            viewEmptyJobHelpCard

        worklogs ->
            let
                deleteIcon worklogEntryIndex =
                    i
                        [ id "delete-worklog-entry-icon"
                        , class "secondary-content material-icons"
                        , onWithOptions
                            "click"
                            { stopPropagation = True, preventDefault = False }
                            (Json.Decode.succeed (Worklog (Delete worklogEntryIndex)))
                        ]
                        [ text "delete" ]

                confirmEditIcon worklogEntryIndex =
                    i
                        [ id "confirm-worklog-entry-edit-icon"
                        , class "secondary-content material-icons"
                        , onWithOptions
                            -- This is a hack to disable the automatic blur of the EditableElement
                            "mousedown"
                            { stopPropagation = False, preventDefault = True }
                            (Json.Decode.succeed NoOp)

                        -- Explicitly blur the EditableElement on click
                        , onClick (Blur (Attempt EditableElement.editTagId))
                        ]
                        [ text "check" ]

                renderWorklogEntry : Int -> WorklogEntry -> Html Msg
                renderWorklogEntry index ( worklogEntryContent, worklogEntryWidgetState ) =
                    let
                        indexCountingUnsavedEntry =
                            index + 1

                        config =
                            EditableElement.config
                                { stateMsg = WorklogEntryWidget indexCountingUnsavedEntry >> Worklog
                                , editEnabled = editable
                                , submitOnEnter = not (Utils.isSmallScreen windowSize)
                                }
                    in
                        case EditableElement.getMode config worklogEntryWidgetState of
                            EditableElement.ReadMode attributes ->
                                li
                                    (class "collection-item worklog-entry"
                                        :: attributes
                                    )
                                    (if editable then
                                        -- Cannot prepend the delete icon as with the attrs
                                        [ EditableElement.markdownReadMode worklogEntryContent
                                        , deleteIcon indexCountingUnsavedEntry
                                        ]
                                     else
                                        [ EditableElement.markdownReadMode worklogEntryContent ]
                                    )

                            EditableElement.EditMode attributes ->
                                li
                                    [ class "collection-item worklog-entry" ]
                                    [ DirtyHtml.Textarea.view
                                        (DirtyHtml.Textarea.defaultConfig { toMsg = WorklogEntryTextareaMsg })
                                        (rows 1
                                            :: class "worklog-entry-edit materialize-textarea"
                                            :: defaultValue worklogEntryContent
                                            :: onInput (Save indexCountingUnsavedEntry >> Worklog)
                                            :: attributes
                                        )
                                        []
                                    , confirmEditIcon indexCountingUnsavedEntry
                                    ]
            in
                List.indexedMap renderWorklogEntry worklogs
                    |> ul [ class "grey-text collection with-header flex-scrollable z-depth-1" ]


viewEmptyJobHelpCard : Html Msg
viewEmptyJobHelpCard =
    Helpcard.view
        [ Helpcard.text "Looks like you haven't started working on this job yet. Let me give you a few hints to get started:"
        , Helpcard.bulletlist
            [ Helpcard.text "To skip this job and jump to the next job in the queue, click \"SKIP\" or use ALT+S hotkey"
            , Helpcard.text "To delete this job altogether, click \"DROP\" or use ALT+R hotkey"
            , Helpcard.text "To start working on this job, click \"LOAD\" or use ALT+L hotkey"
            , Helpcard.bulletlist
                [ Helpcard.text "When you are working on a job, you can add new entries to its journal. Add as many details as needed, so that you can quickly remember all the context of the job when you get back to it later on. But be careful, remember that you'll need to read it all back, do not clog it with unnecessary stuff!"
                , Helpcard.markdown "**New!** Now you can write multiple lines in a journal entry, by using shift+enter"
                , Helpcard.markdown "**New!** Now you can write using Markdown syntax, check out what you can do [in this cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)"
                , Helpcard.text "When you have to stop working on the job, click \"YIELD\" or use ALT+Y. When a job is yielded, it is put back to the end of the queue, and the next job is shown instead"
                , Helpcard.text
                    "When you have finished working on the job, click \"FINISH\" or use ALT+C"
                ]
            , Helpcard.text "Don't worry if you have a typo. The title and journal entries are editable, just click on them"
            , Helpcard.text "An experimental metrics report is available just by clicking on the graph button at the right. Click it again to switch back to the journal"
            ]
        ]


isEditingWorklogEntry : Model -> Bool
isEditingWorklogEntry { worklog } =
    List.foldl
        (\( entry, widgetState ) editing ->
            case ( editing, EditableElement.isInEditMode widgetState ) of
                ( True, _ ) ->
                    True

                ( False, True ) ->
                    True

                ( False, False ) ->
                    False
        )
        False
        worklog


{-| Present the form to add new entried to the job's journal
-}
viewWorklogForm : Window.Size -> String -> Model -> Html Msg
viewWorklogForm windowSize buttonText { worklog } =
    let
        textareaLabel : Html.Html Msg
        textareaLabel =
            label [ for "input-worklog" ]
                [ text
                    (if not (Utils.isSmallScreen windowSize) then
                        "tips: shift+enter, Markdown..."
                     else
                        "tips: multi-line, Markdown..."
                    )
                ]

        keyedTextarea : String -> Html.Html Msg -> List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html.Html Msg
        keyedTextarea key label attributes children =
            Html.Keyed.node "span" [] [ ( key, textarea attributes children ), ( "1", label ) ]

        key : String
        key =
            List.length worklog |> toString
    in
        div [ class "row" ]
            [ div [ class "input-field col s9 m10" ]
                [ DirtyHtml.Textarea.view
                    (DirtyHtml.Textarea.config
                        { toMsg = WorklogInputTextareaMsg
                        , customTag = keyedTextarea key textareaLabel
                        }
                    )
                    ([ id "input-worklog"
                     , class "materialize-textarea"
                     , rows 1
                     , defaultValue (Tuple.first (unsavedWorklogEntry worklog))
                     , onInput (Save 0 >> Worklog)
                     ]
                        ++ (if not (Utils.isSmallScreen windowSize) then
                                [ Utils.onEnter NoOp (Worklog Add) ]
                            else
                                []
                           )
                    )
                    []
                ]
            , div [ class "input-field col s3 m2" ]
                [ button
                    [ class "right waves-effect waves-light btn"
                    , type_ "submit"
                    , onClick (Worklog Add)
                    ]
                    [ text buttonText ]
                ]
            ]
