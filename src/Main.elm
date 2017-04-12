port module Main exposing (Model, main)

{-| Multitask OS.

@docs Model, main

-}

import Json.Decode
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Update.Extra

import Job
import Hotkey


-- MODEL


{-| Our model is conformed by the following elements:
    * job: Job on current execution stack
    * jobQueue: List of queued jobs, waiting for some execution time
    * newJob: Form data to build a new job.

The usage flow is roughly summarized as follows:
    1. At the start of the world, there is no job executing or jobs waiting on the jobQueue
    2. An empty job sits in newJob.
    3. The user creates at least one new job. Creating a job means adding a newJob to the jobQueue
    4. When the jobQueue holds at least one job, the OS selects the first job in the queue and prompts the user to start working on it
    5. When the user starts working on a job, this job is stored in the field 'job' of the Model.
    6. The user cannot work on another job until she yields or finishes the current job under execution.
    7. Finishing a job removes it from the world.
    8. Yielding a job puts it at the end of the jobQueue.
    9. When the user yields or finishes a job, OS jumps to OP 4.

-}
type alias Model =
    { job : Maybe Job.Model
    , jobQueue : JobQueue
    , newJob : Job.Model
    , hotkeysPressed: Hotkey.Model
    }


type alias JobQueue =
    List Job.Model


init : Model
init =
    { job = Nothing
    , jobQueue = []
    , newJob = Job.init
    , hotkeysPressed = Hotkey.init
    }


flushNewJob : Model -> Model
flushNewJob model =
    { model | newJob = Job.init }


updateNewJob : Model -> Job.Model -> Model
updateNewJob model newJob =
    { model | newJob = newJob }


getNewJob : Model -> Job.Model
getNewJob model =
    model.newJob


executeJob : Job.Model -> Model -> Model
executeJob job model =
    { model | job = Just job }


updateExecutingJob : Model -> Job.Model -> Model
updateExecutingJob model job =
    { model | job = Just job }


stopExecutingJob : Model -> Model
stopExecutingJob model =
    { model | job = Nothing }


getExecutingJob : Model -> Maybe Job.Model
getExecutingJob model =
    model.job


getJobQueue : Model -> JobQueue
getJobQueue model =
    model.jobQueue


getNextScheduledJob : Model -> Maybe Job.Model
getNextScheduledJob model =
    case getExecutingJob model of
        Nothing ->
            case getJobQueue model of
                [] ->
                    Nothing

                nextQueuedJob :: _ ->
                    Just nextQueuedJob

        Just job ->
            Just job


enqueueJob : Job.Model -> JobQueue -> JobQueue
enqueueJob job jobQueue =
    jobQueue ++ [ job ]


updateJobQueue : JobQueue -> Model -> Model
updateJobQueue jobQueue model =
    { model | jobQueue = jobQueue }


getHotkeysPressed : Model -> Hotkey.Model
getHotkeysPressed model =
    model.hotkeysPressed


updateHotkeysPressed : Hotkey.Model -> Model -> Model
updateHotkeysPressed hotkeysPressed model =
    { model | hotkeysPressed = hotkeysPressed }



-- UPDATE


type Action
    = NoOp
    | ScheduleJob
    | YieldJob
    | FinishJob
    | ExecuteNextJob
    | SkipNextJob
    | DropNextJob
    | CreateNewJob Job.Action
    | WorkOnJob Job.Action
    | ShowJobDetails Job.Action
    | HotkeyAction Hotkey.Action
    | HotkeyTriggered Hotkey.Hotkey


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        NoOp ->
            model ! []

        ScheduleJob ->
            if Job.isValid model.newJob then
                (model
                    |> updateJobQueue (enqueueJob model.newJob model.jobQueue)
                    |> flushNewJob
                )
                    ! []
            else
                model ! []

        YieldJob ->
            case getExecutingJob model of
                Nothing ->
                    model ! []

                Just job ->
                    (model
                        |> stopExecutingJob
                        |> updateJobQueue (enqueueJob job model.jobQueue)
                    )
                        ! []

        FinishJob ->
            (stopExecutingJob model) ! []

        ExecuteNextJob ->
            case getJobQueue model of
                [] ->
                    model ! []

                nextJob :: restQueue ->
                    (model
                        |> executeJob nextJob
                        |> updateJobQueue restQueue
                    ) ! []

        SkipNextJob ->
            case getJobQueue model of
                [] ->
                    model ! []

                nextJob :: restQueue ->
                    let 
                        jobQueue =
                            enqueueJob nextJob restQueue
                    in
                        (model
                            |> updateJobQueue jobQueue
                        ) ! []

        DropNextJob ->
            case getJobQueue model of
                [] ->
                    model ! []

                _ :: restQueue ->
                    (model
                        |> updateJobQueue restQueue
                    ) ! []

        CreateNewJob action ->
            let
                ( newJob, cmds ) =
                    Job.update action <| getNewJob model
            in
                updateNewJob model newJob ! [ Cmd.map CreateNewJob cmds ]

        ShowJobDetails action ->
            case getNextScheduledJob model of
                Nothing ->
                    model ! []

                Just nextScheduledJob ->
                    let
                        ( updatedNextScheduledJob, cmds ) =
                            Job.update action nextScheduledJob
                    in
                        model ! [ Cmd.map ShowJobDetails cmds ]

        WorkOnJob action ->
            case getExecutingJob model of
                Nothing ->
                    model ! []

                Just executingJob ->
                    let
                        ( updatedExecutingJob, cmds ) =
                            Job.update action executingJob
                    in
                        updateExecutingJob model updatedExecutingJob ! [ Cmd.map WorkOnJob cmds ]

        HotkeyTriggered hotkey ->
            let
                newJobHotkey =
                    Job.newJobHotkey

                nextAction =
                    case hotkey of
                        newJobHotkey ->
                            CreateNewJob Job.newJobHotkeyAction
            in
                model ! []
                    |> Update.Extra.andThen update nextAction

        HotkeyAction action ->
            let
                (hotkeysPressed, hotkeyTriggered) =
                        Hotkey.update action <| getHotkeysPressed model

                modelUpdated =
                    updateHotkeysPressed hotkeysPressed model
            in
                case hotkeyTriggered of
                    Just hotkey ->
                        modelUpdated ! []
                            |> Update.Extra.andThen update (HotkeyTriggered hotkey)

                    Nothing ->
                        modelUpdated ! []




-- VIEW


view : Model -> Html Action
view model =
    viewPort
        [ sideSection
            [ jobScheduleForm model ]
        , mainSection
            [ nextScheduledJobTitle model
            , contextSwitchingControls model
            , nextScheduledJobJournal model
            , nextScheduledJobWorklogForm model
            ]
        ]


viewPort : List (Html Action) -> Html Action
viewPort elements =
    div [ class "container flex-container flex-contained" ]
        [ div [class "row flex-container flex-contained"] elements ]


sideSection : List (Html Action) -> Html Action
sideSection elements =
    div [ class "col s4 flex-container flex-contained" ] elements


mainSection : List (Html Action) -> Html Action
mainSection elements =
    div [ class "col s8 flex-container flex-contained" ] elements


nextScheduledJobTitle : Model -> Html Action
nextScheduledJobTitle model =
    let
        jobTitle =
            case getNextScheduledJob model of
                Nothing ->
                    h5 [ class "section grey-text text-lighten-2" ] [ text "Nothing to work on" ]
                Just nextScheduledJob ->
                    Html.map ShowJobDetails <| Job.showJobTitle nextScheduledJob
     in
         div [] [ jobTitle ]


nextScheduledJobJournal : Model -> Html Action
nextScheduledJobJournal model =
    case getNextScheduledJob model of
        Nothing ->
            Html.text ""

        Just nextScheduledJob ->
            Html.map ShowJobDetails <| Job.showJournalList nextScheduledJob


nextScheduledJobWorklogForm : Model -> Html Action
nextScheduledJobWorklogForm model =
    case getExecutingJob model of
        Nothing ->
            Html.text ""

        Just executingJob ->
            Html.map WorkOnJob <| Job.showJournalForm executingJob



jobScheduleForm : Model -> Html Action
jobScheduleForm model =
    div
        [ class "section"
        , onEnter ScheduleJob
        ]
        [ Html.map CreateNewJob <| Job.showJobForm <| getNewJob model
        , button
            [ class "waves-effect waves-light btn"
            , onClick ScheduleJob
            ]
            [ text "Schedule" ]
        ]


contextSwitchingControls : Model -> Html Action
contextSwitchingControls model =
    div []
        <| case getExecutingJob model of
            Nothing ->
                case getJobQueue model of
                    [] ->
                        []

                    jobQueue ->
                        [ button
                            [ class "waves-effect waves-light btn"
                            , onClick ExecuteNextJob
                            ]
                            [ text "Go!" ]
                        , button
                            ([ classList
                                [ ( "waves-effect waves-light btn", True )
                                , ( "disabled", List.length jobQueue < 2 )
                                ]
                             , onClick SkipNextJob
                             ]
                            )
                            [ text "Skip" ]
                        , button
                            [ class "waves-effect waves-light btn"
                            , onClick DropNextJob
                            ]
                            [ text "Drop" ]
                        ]

            Just job ->
                [ button
                    [ class "waves-effect waves-light btn"
                    , onClick YieldJob
                    ]
                    [ text "Yield" ]
                , button
                    [ class "waves-effect waves-light btn"
                    , onClick FinishJob
                    ]
                    [ text "Finished" ]
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


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.map HotkeyAction Hotkey.subscriptions



-- WIRING


{-| Simple Signal Wiring using an Actions tagged union
-}
main : Program (Maybe Model) Model Action
main =
    Html.programWithFlags
        { init =
            \maybeModel ->
                case maybeModel of
                    Just model ->
                        model ! []

                    Nothing ->
                        init ! []
        , view = view
        , update =
            \action model ->
                let
                    ( newModel, cmds ) =
                        update action model
                in
                    newModel ! [ persistModel newModel, cmds ]
        , subscriptions = subscriptions
        }


port persistModel : Model -> Cmd msg
