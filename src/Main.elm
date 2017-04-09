port module Main exposing (Model, main)

{-| Multitask OS.

@docs Model, main

-}

import Json.Decode
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Job


-- MODEL


{-| There are 3 main entities in our model:
    * job: Job on current execution stack
    * jobQueue: List of queued jobs, waiting for some execution time
    * newJob: Form data to build a new job.

The use flow is roughly summarized as follows:
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
    , jobQueue : List Job.Model
    , newJob : Job.Model
    }


init : Model
init =
    { job = Nothing
    , jobQueue = []
    , newJob = Job.init
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


isExecutingJob : Model -> Job.Model -> Bool
isExecutingJob model candidateJob =
    case model.job of
        Nothing ->
            False

        Just executingJob ->
            executingJob == candidateJob


getNextScheduledJob : Model -> Maybe Job.Model
getNextScheduledJob model =
    case getExecutingJob model of
        Nothing ->
            case getNextJobFromQueue model.jobQueue of
                Nothing ->
                    Nothing

                Just ( nextJob, _ ) ->
                    Just nextJob

        Just job ->
            Just job


getNextJobFromQueue : List Job.Model -> Maybe ( Job.Model, List Job.Model )
getNextJobFromQueue jobQueue =
    case jobQueue of
        [] ->
            Nothing

        nextJob :: restQueue ->
            Just ( nextJob, restQueue )


enqueueJob : Job.Model -> List Job.Model -> List Job.Model
enqueueJob job jobQueue =
    jobQueue ++ [ job ]


updateJobQueue : List Job.Model -> Model -> Model
updateJobQueue jobQueue model =
    { model | jobQueue = jobQueue }



-- UPDATE


type Action
    = NoOp
    | ScheduleJob
    | YieldJob
    | FinishJob
    | ExecuteNextJob
    | SkipNextJob
    | DropNextJob
    | NewJob Job.Action
    | ExecutingJob Job.Action
    | NextScheduledJob Job.Action


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
            case getNextJobFromQueue model.jobQueue of
                Nothing ->
                    model ! []

                Just ( nextJob, restQueue ) ->
                    (model
                        |> executeJob nextJob
                        |> updateJobQueue restQueue
                    )
                        ! []

        SkipNextJob ->
            case getNextJobFromQueue model.jobQueue of
                Nothing ->
                    model ! []

                Just ( nextJob, restQueue ) ->
                    (updateJobQueue (enqueueJob nextJob restQueue) model) ! []

        DropNextJob ->
            case getNextJobFromQueue model.jobQueue of
                Nothing ->
                    model ! []

                Just ( _, restQueue ) ->
                    (updateJobQueue restQueue model) ! []

        NewJob action ->
            let
                ( newJob, cmds ) =
                    Job.update action <| getNewJob model
            in
                updateNewJob model newJob ! [ Cmd.map NewJob cmds ]

        NextScheduledJob action ->
            case getNextScheduledJob model of
                Nothing ->
                    model ! []

                Just nextScheduledJob ->
                    let
                        ( updatedNextScheduledJob, cmds ) =
                            Job.update action nextScheduledJob
                    in
                        model ! [ Cmd.map NextScheduledJob cmds ]

        ExecutingJob action ->
            case getExecutingJob model of
                Nothing ->
                    model ! []

                Just executingJob ->
                    let
                        ( updatedExecutingJob, cmds ) =
                            Job.update action executingJob
                    in
                        updateExecutingJob model updatedExecutingJob ! [ Cmd.map ExecutingJob cmds ]



-- VIEW


view : Model -> Html Action
view model =
    div [ class "container flex-container flex-contained" ]
        [ div [ class "row flex-container flex-contained" ]
            [ div [ class "col s4 flex-container flex-contained" ]
                [ jobScheduleForm model ]
            , div [ class "col s8 flex-container flex-contained" ]
                <| [ div [ class "row" ]
                        [ case getNextScheduledJob model of
                            Nothing ->
                                h5 [ class "section grey-text text-lighten-2" ] [ text "Nothing to work on" ]

                            Just nextScheduledJob ->
                                Html.map NextScheduledJob <| Job.showJobTitle nextScheduledJob
                        , contextSwitchingControls model
                        ]
                   ]
                ++ case getNextScheduledJob model of
                    Nothing ->
                        []

                    Just nextScheduledJob ->
                        [ Html.map NextScheduledJob <| Job.showJournalList nextScheduledJob ]
                            ++ if isExecutingJob model nextScheduledJob then
                                [ Html.map ExecutingJob <| Job.showJournalForm nextScheduledJob ]
                               else
                                []
            ]
        ]


jobScheduleForm : Model -> Html Action
jobScheduleForm model =
    div
        [ class "section"
        , onEnter ScheduleJob
        ]
        [ Html.map NewJob <| Job.showJobForm <| getNewJob model
        , button
            [ class "waves-effect waves-light btn"
            , onClick ScheduleJob
            ]
            [ text "Schedule" ]
        ]


contextSwitchingControls : Model -> Html Action
contextSwitchingControls model =
    div [ class "col s12" ]
        <| case getExecutingJob model of
            Nothing ->
                case model.jobQueue of
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
        , subscriptions = \_ -> Sub.none
        }


port persistModel : Model -> Cmd msg
