module Job
    exposing
        ( Model
        , Msg
        , init
        , encode
        , decoder
        , isValid
        , update
        , focusTitleForm
        , focusWorklogForm
        , viewTitle
        , viewTitleForm
        , viewWorklog
        , viewWorklogForm
        )

{-| A Job is an specific task or goal scheduled to be accomplished.


# Model

@docs Model, init, encode, decoder, isValid


# Business Logic

@docs Msg, update, focusTitleForm, focusWorklogForm


# Presentation

@docs viewTitle, viewTitleForm, viewWorklog, viewWorklogForm

-}

import String
import Json.Decode
import Json.Encode
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List.Extra
import Update.Extra
import Dom
import Task


-- MODEL


{-| A job has a name, a journal, and the possibility to add new entries
to the journal by means of a worklog form.
-}
type alias Model =
    { title : String
    , worklog : Worklog
    , editingWorklogEntryIndex : Maybe Int
    }


type alias Worklog =
    List WorklogEntry


type alias WorklogEntry =
    String


{-| Create a new empty Job
-}
init : Model
init =
    { title = ""
    , worklog = initWorklog
    , editingWorklogEntryIndex = Nothing
    }


{-| Encode a job model
-}
encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "title", Json.Encode.string model.title )
        , ( "worklog", Json.Encode.list (List.map Json.Encode.string model.worklog) )
        ]


{-| Decode a job model
-}
decoder : Json.Decode.Decoder Model
decoder =
    Json.Decode.map3
        Model
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "worklog" (Json.Decode.list Json.Decode.string))
        (Json.Decode.succeed Nothing)


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
    ""


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


editWorklogEntry : Int -> WorklogEntry -> Worklog -> Worklog
editWorklogEntry index worklogEntry worklog =
    case List.Extra.setAt index worklogEntry worklog of
        Just newWorklog ->
            newWorklog

        Nothing ->
            worklogEntry :: worklog



-- UPDATE


{-| Possible actions that can occur on a job
-}
type Msg
    = NoOp
    | EditTitle String
    | Worklog WorklogMsg
    | Focus FocusMsg


type WorklogMsg
    = Add
    | StartEditing Int
    | StopEditing Int
    | Save Int WorklogEntry
    | Delete Int


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

        Worklog Add ->
            ( { model | worklog = initWorklogEntry :: model.worklog }, Cmd.none )

        Worklog (Save index worklogEntry) ->
            ( { model | worklog = editWorklogEntry index worklogEntry model.worklog }, Cmd.none )

        Worklog (StartEditing worklogEntryIndex) ->
            ( { model | editingWorklogEntryIndex = Just worklogEntryIndex }, Cmd.none )
                |> (Update.Extra.andThen update (Focus (Attempt "editing-worklog-entry")))

        Worklog (StopEditing worklogEntryIndex) ->
            ( { model | editingWorklogEntryIndex = Nothing }, Cmd.none )

        Worklog (Delete worklogEntryIndex) ->
            ( { model | worklog = List.Extra.removeAt worklogEntryIndex model.worklog }, Cmd.none )


{-| Focus the input field to enter the title of a new job
-}
focusTitleForm : Msg
focusTitleForm =
    Focus (Attempt "input-title")


{-| Focus the input field to enter a new worklog entry
-}
focusWorklogForm : Msg
focusWorklogForm =
    Focus (Attempt "input-worklog")



-- VIEW


{-| Present the form for creating new jobs
-}
viewTitleForm : String -> Model -> Html Msg
viewTitleForm placeholder model =
    div [ class "input-field" ]
        [ input
            [ id "input-title"
            , class "validate"
            , type_ "text"
            , value model.title
            , onInput EditTitle
            ]
            []
        , label [ for "input-title" ]
            [ text placeholder ]
        ]


{-| Present the title of a job
-}
viewTitle : Model -> Html Msg
viewTitle model =
    h4 [ class "grey-text text-darken-2" ] [ text model.title ]


{-| Present the list of journal entries of a job
-}
viewWorklog : Bool -> Model -> Html Msg
viewWorklog editable model =
    case savedWorklog model.worklog of
        [] ->
            let
                bulletList =
                    ul [ class "browser-default" ]

                bullet content =
                    li
                        [ class "browser-default"
                        , style
                            [ ( "list-style-type", "disc" ) ]
                        ]
                        [ text content ]
            in
                p [ class "card-panel flex-scrollable teal lighten-4 grey-text text-darken-3" ]
                    [ text "Looks like you haven't started working on this job yet. Let me give you a few hints to get started:"
                    , bulletList
                        [ bullet "To skip this job and jump to the next job in the queue, click \"SKIP\" or use ALT+S hotkey"
                        , bullet "To delete this job altogether, click \"DROP\" or use ALT+R hotkey"
                        , bullet "To start working on this job, click \"GO!\" or use ALT+G hotkey"
                        , bulletList
                            [ bullet "When you are working on the job, you can add new entries to the job journal. Add as many as possible, so that you can quickly remember all the context of the job when you get back to it at another time!"
                            , bullet "When you have to stop working on the job, click \"YIELD\" or use ALT+Y. When a job is yielded, it is put back to the end of the queue, and you can start working on the next one"
                            , bullet
                                "When you have finished working on the job, click \"FINISH\" or use ALT+C"
                            ]
                        ]
                    ]

        worklogs ->
            let
                readView worklogEntryIndex worklogEntry =
                    let
                        parentAttributes =
                            if editable then
                                [ onClick (Worklog (StartEditing worklogEntryIndex))
                                , style
                                    [ ( "cursor", "pointer" )
                                    ]
                                ]
                            else
                                [ style
                                    [ ( "cursor", "default" )
                                    ]
                                ]

                        children =
                            [ (if (not (String.isEmpty worklogEntry)) then
                                text worklogEntry
                               else
                                em
                                    [ style
                                        [ ( "font-size", "0.9em" ) ]
                                    ]
                                    [ text "Nothing much -- click to edit" ]
                              )
                            , if editable then
                                i
                                    [ id "delete-worklog-entry-icon"
                                    , class "secondary-content material-icons"
                                    , onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.Decode.succeed (Worklog (Delete worklogEntryIndex)))
                                    ]
                                    [ text "delete" ]
                              else
                                text ""
                            ]
                    in
                        ( parentAttributes, children )

                editView worklogEntryIndex worklogEntry =
                    let
                        parentAttributes =
                            []

                        children =
                            [ input
                                ([ id "editing-worklog-entry"
                                 , style
                                    [ ( "border-bottom", "none" )
                                    , ( "height", "1.5em" )
                                    , ( "margin-bottom", "0" )
                                    ]
                                 , type_ "text"
                                 , value worklogEntry
                                 , onInput (Save worklogEntryIndex >> Worklog)
                                 , onBlur (Worklog (StopEditing worklogEntryIndex))
                                 , onEnter (Worklog (StopEditing worklogEntryIndex))
                                 ]
                                )
                                []
                            ]
                    in
                        ( parentAttributes, children )

                readOrEditView worklogEntryIndex worklogEntry =
                    case ( editable, model.editingWorklogEntryIndex ) of
                        ( True, Just editedIndex ) ->
                            if worklogEntryIndex == editedIndex then
                                editView worklogEntryIndex worklogEntry
                            else
                                readView worklogEntryIndex worklogEntry

                        _ ->
                            readView worklogEntryIndex worklogEntry
            in
                ul [ class "grey-text collection with-header flex-scrollable z-depth-1" ] <|
                    List.indexedMap
                        (\index worklogEntry ->
                            let
                                indexCountingUnsavedEntry =
                                    index + 1

                                ( parentAttributes, children ) =
                                    readOrEditView indexCountingUnsavedEntry worklogEntry
                            in
                                li
                                    ([ class "collection-item" ] ++ parentAttributes)
                                    children
                        )
                        worklogs


{-| Present the form to add new entried to the job's journal
-}
viewWorklogForm : String -> Model -> Html Msg
viewWorklogForm buttonText { worklog } =
    div []
        [ div [ class "input-field col s9" ]
            [ input
                [ id "input-worklog"
                , value (unsavedWorklogEntry worklog)
                , type_ "text"
                , onInput (Save 0 >> Worklog)
                , onEnter (Worklog Add)
                ]
                []
            , label [ for "input-worklog" ]
                [ text "New journal entry" ]
            ]
        , div [ class "input-field col s3" ]
            [ button
                [ class "waves-effect waves-light btn"
                , type_ "submit"
                , onClick (Worklog Add)
                ]
                [ text buttonText ]
            ]
        ]


onEnter : Msg -> Attribute Msg
onEnter action =
    let
        tagger code =
            if code == 13 then
                action
            else
                NoOp
    in
        on "keydown" <| Json.Decode.map tagger keyCode
