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
        , viewTitle
        , viewWorklog
        , viewWorklogForm
        )

import String
import Json.Decode
import Json.Encode
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List.Extra
import Dom
import Task
import EditableElement
import Utils
import Helpcard


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
    []


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
    | Focus FocusMsg


type WorklogMsg
    = Add
    | Save Int String
    | Delete Int
    | WorklogEntryWidget Int ( Cmd Msg, EditableElement.State )


type FocusMsg
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

        EditTitle newTitle ->
            ( { model | title = newTitle }, Cmd.none )

        TitleWidget ( cmdNextTitleWidgetState, titleWidgetState ) ->
            ( { model | titleWidgetState = titleWidgetState }, cmdNextTitleWidgetState )

        Worklog Add ->
            ( { model | worklog = initWorklogEntry :: model.worklog }, Cmd.none )

        Worklog (Save index worklogEntryContent) ->
            ( { model | worklog = editWorklogEntryContent index worklogEntryContent model.worklog }, Cmd.none )

        Worklog (Delete worklogEntryIndex) ->
            ( { model | worklog = List.Extra.removeAt worklogEntryIndex model.worklog }, Cmd.none )

        Worklog (WorklogEntryWidget index ( cmd, state )) ->
            ( { model | worklog = updateWorklogEntryWidgetState index state model.worklog }, cmd )


{-| Focus the input field to enter a new worklog entry
-}
focusWorklogForm : Msg
focusWorklogForm =
    Focus (Attempt "input-worklog")


triggerTitleEditMode : Msg
triggerTitleEditMode =
    EditableElement.triggerEditMode "title-edit" TitleWidget



-- VIEW


{-| Present the title of a job
-}
viewTitle : Model -> Html Msg
viewTitle model =
    let
        config =
            EditableElement.config
                { readModeTag = EditableElement.htmlElement "title" span
                , editModeTag = EditableElement.htmlElement "title-edit" input
                , editMsg = EditTitle
                , stateMsg = TitleWidget
                , editEnabled = True
                }
    in
        h4 [ class "grey-text text-darken-2" ] [ EditableElement.view config model.titleWidgetState model.title ]


{-| Present the list of journal entries of a job
-}
viewWorklog : Bool -> Model -> Html Msg
viewWorklog editable model =
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

                readModeElement worklogEntryIndex =
                    EditableElement.htmlElement
                        "worklog-entry"
                        (\attributes children ->
                            let
                                finalAttributes =
                                    attributes ++ [ class "collection-item" ]

                                finalChildren =
                                    if editable then
                                        children ++ [ deleteIcon worklogEntryIndex ]
                                    else
                                        children
                            in
                                li finalAttributes finalChildren
                        )

                editModeElement =
                    EditableElement.htmlElement
                        "worklog-entry-edit"
                        (\attributes children ->
                            li [ class "collection-item" ] [ input attributes children ]
                        )
            in
                ul [ class "grey-text collection with-header flex-scrollable z-depth-1" ] <|
                    List.indexedMap
                        (\index ( worklogEntryContent, worklogEntryWidgetState ) ->
                            let
                                indexCountingUnsavedEntry =
                                    index + 1

                                config =
                                    EditableElement.config
                                        { readModeTag = readModeElement indexCountingUnsavedEntry
                                        , editModeTag = editModeElement
                                        , editMsg = Save indexCountingUnsavedEntry >> Worklog
                                        , stateMsg = WorklogEntryWidget indexCountingUnsavedEntry >> Worklog
                                        , editEnabled = editable
                                        }
                            in
                                EditableElement.view config worklogEntryWidgetState worklogEntryContent
                        )
                        worklogs


viewEmptyJobHelpCard : Html Msg
viewEmptyJobHelpCard =
    Helpcard.view
        [ Helpcard.text "Looks like you haven't started working on this job yet. Let me give you a few hints to get started:"
        , Helpcard.bulletlist
            [ Helpcard.bullet "To skip this job and jump to the next job in the queue, click \"SKIP\" or use ALT+S hotkey"
            , Helpcard.bullet "To delete this job altogether, click \"DROP\" or use ALT+R hotkey"
            , Helpcard.bullet "To start working on this job, click \"GO!\" or use ALT+G hotkey"
            , Helpcard.nbulletlist
                [ Helpcard.bullet "When you are working on the job, you can add new entries to the job journal. Add as many as possible, so that you can quickly remember all the context of the job when you get back to it at another time!"
                , Helpcard.bullet "When you have to stop working on the job, click \"YIELD\" or use ALT+Y. When a job is yielded, it is put back to the end of the queue, and you can start working on the next one"
                , Helpcard.bullet
                    "When you have finished working on the job, click \"FINISH\" or use ALT+C"
                ]
            ]
        ]


{-| Present the form to add new entried to the job's journal
-}
viewWorklogForm : String -> Model -> Html Msg
viewWorklogForm buttonText { worklog } =
    div [ class "row" ]
        [ div [ class "input-field col s8 m10" ]
            [ input
                [ id "input-worklog"
                , value (Tuple.first (unsavedWorklogEntry worklog))
                , type_ "text"
                , onInput (Save 0 >> Worklog)
                , Utils.onEnter NoOp (Worklog Add)
                ]
                []
            , label [ for "input-worklog" ]
                [ text "New journal entry" ]
            ]
        , div [ class "input-field col s4 m2" ]
            [ button
                [ class "right waves-effect waves-light btn"
                , type_ "submit"
                , onClick (Worklog Add)
                ]
                [ text buttonText ]
            ]
        ]
