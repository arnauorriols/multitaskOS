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
import Dom
import Task


-- MODEL


{-| A job has a name, a journal, and the possibility to add new entries
to the journal by means of a worklog form.
-}
type alias Model =
    { title : String
    , worklog : Worklog
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
    Json.Decode.map2
        Model
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "worklog" (Json.Decode.list Json.Decode.string))


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
    | Edit Int WorklogEntry


type FocusMsg
    = Attempt String
    | Result (Result Dom.Error ())


{-| Handle incoming @docs Msg
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
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

        Worklog (Edit index worklogEntry) ->
            ( { model | worklog = editWorklogEntry index worklogEntry model.worklog }, Cmd.none )


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
viewTitleForm : Model -> Html Msg
viewTitleForm model =
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
            [ text "Job title" ]
        ]


{-| Present the title of a job
-}
viewTitle : Model -> Html Msg
viewTitle model =
    h3 [ class "grey-text text-darken-2" ] [ text model.title ]


{-| Present the list of journal entries of a job
-}
viewWorklog : Model -> Html Msg
viewWorklog model =
    ul [ class "grey-text collection with-header flex-scrollable z-depth-1" ] <|
        case savedWorklog model.worklog of
            [] ->
                [ li [ class "collection-item" ] [ text "Nothing logged yet for this job" ] ]

            worklogs ->
                List.map (\worklogEntry -> li [ class "collection-item" ] [ text worklogEntry ]) worklogs


{-| Present the form to add new entried to the job's journal
-}
viewWorklogForm : Model -> Html Msg
viewWorklogForm { worklog } =
    div [ class "row" ]
        [ div [ class "input-field col s9" ]
            [ input
                [ id "input-worklog"
                , value (unsavedWorklogEntry worklog)
                , type_ "text"
                , onInput (Edit 0 >> Worklog)
                , onEnter (Worklog Add)
                ]
                []
            , label [ for "input-worklog" ]
                [ text "Journal entry" ]
            ]
        , div [ class "input-field col s3" ]
            [ button
                [ class "waves-effect waves-light btn"
                , type_ "submit"
                , onClick (Worklog Add)
                ]
                [ text "Log" ]
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
