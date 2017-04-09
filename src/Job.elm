module Job
    exposing
        ( Model
        , Action
        , init
        , isValid
        , update
        , showJobForm
        , showJobTitle
        , showJournalList
        , showJournalForm
        )

{-| A Job is an specific task or goal scheduled to be accomplished. 

# Model
@docs Model, init, isValid

# Business Logic
@docs Action, update

# Presentation
@docs showJobTitle, showJobForm, showJournalList, showJournalForm
-}

import String
import Json.Decode
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


-- MODEL

{-| A job has a name, a journal, and the possibility to add new entries
    to the journal by means of a worklog form.
-}
type alias Model =
    { threadName : String
    , worklog : String
    , journal : List String
    }


{-| Create a new empty Job -}
init : Model
init =
    { threadName = ""
    , worklog = ""
    , journal = []
    }


updateName : String -> Model -> Model
updateName newName model =
    { model | threadName = newName }


updateWorklog : String -> Model -> Model
updateWorklog newWorklog model =
    { model | worklog = newWorklog }


saveWorklogToJournal : Model -> Model
saveWorklogToJournal model =
    if String.isEmpty model.worklog then
        model
    else
        { model | journal = model.journal ++ [ model.worklog ] }


flushWorklog : Model -> Model
flushWorklog model =
    { model | worklog = "" }


{-| Assert the Job is valid and can be scheduled -}
isValid : Model -> Bool
isValid model =
    not (String.isEmpty model.threadName)



-- UPDATE


{-| Possible actions that can occur on a job -}
type Action
    = NoOp
    | UpdateName String
    | UpdateWorklog String
    | SaveWorklogToJournal


{-| Handle incoming @docs Action -}
update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        NoOp ->
            model ! []

        UpdateName newName ->
            updateName newName model ! []

        UpdateWorklog newWorklog ->
            updateWorklog newWorklog model ! []

        SaveWorklogToJournal ->
            (model
                |> saveWorklogToJournal
                |> flushWorklog
            )
                ! []



-- VIEW


{-| Present the form for creating new jobs -}
showJobForm : Model -> Html Action
showJobForm model =
    div [ class "input-field" ]
        [ input
            [ id "input-thread-name"
            , class "validate"
            , type_ "text"
            , value model.threadName
            , onInput UpdateName
            ]
            []
        , label [ for "input-thread-name" ]
            [ text "Job title" ]
        ]


{-| Present the title of a job -}
showJobTitle : Model -> Html Action
showJobTitle model =
    h3 [ class "grey-text text-darken-2" ] [ text model.threadName ]


{-| Present the list of journal entries of a job -}
showJournalList : Model -> Html Action
showJournalList model =
    ul [ class "grey-text collection with-header flex-scrollable z-depth-1" ]
        <| case model.journal of
            [] ->
                [ li [ class "collection-item" ] [ text "Nothing logged yet for this job" ] ]

            journal ->
                List.map (\journalEntry -> li [ class "collection-item" ] [ text journalEntry ]) journal

{-| Present the form to add new entried to the job's journal -}
showJournalForm : Model -> Html Action
showJournalForm model =
    div [ class "row" ]
        [ div [ class "input-field col s9" ]
            [ input
                [ id "input-worklog"
                , value model.worklog
                , type_ "text"
                , onInput <| UpdateWorklog
                , onEnter <| SaveWorklogToJournal
                ]
                []
            , label [ for "input-worklog" ]
                [ text "Journal entry" ]
            ]
        , div [ class "input-field col s3" ]
            [ button
                [ class "waves-effect waves-light btn"
                , type_ "submit"
                , onClick <| SaveWorklogToJournal
                ]
                [ text "Log" ]
            ]
        ]


onEnter : Action -> Attribute Action
onEnter action =
    let
        tagger code =
            if code == 13 then
                action
            else
                NoOp
    in
        on "keydown" <| Json.Decode.map tagger keyCode
