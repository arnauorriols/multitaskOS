port module Main exposing (main)

import Debug
import Task
import Json.Decode
import Json.Encode
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Update.Extra
import List.Extra
import Utils
import Hotkey
import Job
import Metrics
import Graph


-- MODEL


type alias Model =
    { jobQueue : List JobQueueEntry
    , nextJobStatus : JobStatus
    , hotkeysPressed : Hotkey.Model
    , hintsStatus : HotkeyHintStatus
    , viewType : ViewType
    , graphState : Graph.State
    , graphConfig : GraphConfigState
    , msgBeingTracked : Msg
    }


type ViewType
    = WorklogView
    | GraphView


type JobStatus
    = Active
    | Queued


type alias JobQueueEntry =
    { data : Job.Model
    , history : Metrics.State Msg
    }


type alias GraphConfigState =
    { resolutionUnit : Graph.DateUnit
    , offsetAmmount : Int
    , offsetUnit : Graph.DateUnit
    }


graphConfigStateInit : GraphConfigState
graphConfigStateInit =
    { resolutionUnit = Graph.Minutes
    , offsetAmmount = 7
    , offsetUnit = Graph.Days
    }


graphConfig : GraphConfigState -> Graph.Config Msg
graphConfig configState =
    let
        ensureMax10Columns : Int -> Int -> Int
        ensureMax10Columns offsetAmmount groupAmmount =
            if offsetAmmount // groupAmmount > 7 then
                ensureMax10Columns offsetAmmount (groupAmmount + 1)
            else
                groupAmmount
    in
        Graph.config
            { toMsg = SetGraphState
            , from = Graph.offset configState.offsetUnit -configState.offsetAmmount
            , groupBy = Graph.groupBy configState.offsetUnit (ensureMax10Columns configState.offsetAmmount 1)
            , resolution = Graph.resolution configState.resolutionUnit
            , reducer =
                \( from, to ) ( msg, timestamp ) accumulated ->
                    case msg of
                        NextJob Execute ->
                            if accumulated == 0 then
                                to - timestamp
                            else
                                accumulated - (timestamp - from)

                        ActiveJob Yield ->
                            accumulated + (timestamp - from)

                        _ ->
                            Debug.crash ("There's an intruder msg in the metrics!")
            }


type HotkeyHintStatus
    = Shown
    | Hidden


hotkeyHintOrReal : HotkeyHintStatus -> String -> String -> String
hotkeyHintOrReal hotkeyHintStatus hotkeyHint realText =
    case hotkeyHintStatus of
        Shown ->
            hotkeyHint

        Hidden ->
            realText


init : Model
init =
    { jobQueue = []
    , hotkeysPressed = Hotkey.init
    , nextJobStatus = Queued
    , hintsStatus = Hidden
    , viewType = WorklogView
    , graphState = Graph.init
    , graphConfig = graphConfigStateInit
    , msgBeingTracked = NoOp
    }


encode : Model -> Json.Encode.Value
encode model =
    let
        encodeStatus status =
            Json.Encode.string
                (case status of
                    Active ->
                        "Active"

                    Queued ->
                        "Queued"
                )

        encodeJob job =
            Json.Encode.object
                [ ( "data", Job.encode job.data )
                , ( "history", Metrics.encode metricsConfig job.history )
                ]
    in
        Json.Encode.object
            [ ( "jobQueue", Json.Encode.list (List.map encodeJob model.jobQueue) )
            , ( "nextJobStatus", encodeStatus model.nextJobStatus )
            ]


decoder : Json.Decode.Decoder Model
decoder =
    let
        jobStatusDeserializer jobStatus =
            case jobStatus of
                "Active" ->
                    Active

                "Queued" ->
                    Queued

                _ ->
                    Debug.crash "A Job loaded from the storage has an impossible status!"

        jobStatusDecoder =
            (Json.Decode.map jobStatusDeserializer Json.Decode.string)

        jobQueueDecoder =
            Json.Decode.list
                (Json.Decode.map2
                    JobQueueEntry
                    (Json.Decode.field "data" Job.decoder)
                    (Json.Decode.field "history" (Metrics.decoder metricsConfig))
                )
    in
        Json.Decode.map8
            Model
            (Json.Decode.field "jobQueue" jobQueueDecoder)
            (Json.Decode.field "nextJobStatus" jobStatusDecoder)
            (Json.Decode.succeed Hotkey.init)
            (Json.Decode.succeed Hidden)
            (Json.Decode.succeed WorklogView)
            (Json.Decode.succeed Graph.init)
            (Json.Decode.succeed graphConfigStateInit)
            (Json.Decode.succeed NoOp)


decodeValue : Json.Encode.Value -> Model
decodeValue value =
    case Json.Decode.decodeValue decoder value of
        Ok decodedModel ->
            decodedModel

        Err error ->
            Debug.crash error



-- UPDATE


type Msg
    = NoOp
    | SyncModel Model
    | NewJob
    | NextJob NextJobMsg
    | ActiveJob ActiveJobMsg
    | Hotkey HotkeyMsg
    | ToggleViewType
    | SetGraphState Graph.State
    | GraphControls GraphControlsMsg


type NextJobMsg
    = Execute
    | Skip
    | Drop
    | MetricsMsg (Metrics.State Msg)
    | NextJobMsg Job.Msg


type ActiveJobMsg
    = Yield
    | Finish
    | ActiveJobMsg Job.Msg


type HotkeyMsg
    = ShowHints
    | HideHints
    | HotkeyMsg Hotkey.Msg
    | Triggered Hotkey.Hotkey


type GraphControlsMsg
    = ChangeResolution String
    | ChangeOffsetAmmount String
    | ChangeOffsetUnit String


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NoOp ->
            ( model, Cmd.none )

        SyncModel model ->
            ( model, Cmd.none )

        NewJob ->
            case model.nextJobStatus of
                Active ->
                    ( model, Cmd.none )

                Queued ->
                    let
                        newJob =
                            { data = Job.init, history = Metrics.init }
                    in
                        ( { model | jobQueue = newJob :: model.jobQueue }, Cmd.none )
                            |> Update.Extra.andThen update (NextJob (NextJobMsg Job.triggerTitleEditMode))

        NextJob Execute ->
            ( { model | nextJobStatus = Active }, Cmd.none )
                |> Update.Extra.andThen update (ActiveJob (ActiveJobMsg Job.focusWorklogForm))

        NextJob Skip ->
            case ( model.nextJobStatus, List.Extra.uncons model.jobQueue ) of
                ( Queued, Just ( job, restQueue ) ) ->
                    ( { model | jobQueue = restQueue ++ [ job ] }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NextJob Drop ->
            case ( model.nextJobStatus, List.Extra.uncons model.jobQueue ) of
                ( Queued, Just ( job, restQueue ) ) ->
                    ( { model | jobQueue = restQueue }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NextJob (NextJobMsg jobMsg) ->
            case List.Extra.uncons model.jobQueue of
                Just ( job, restQueue ) ->
                    let
                        ( job2Data, cmd ) =
                            Job.update jobMsg job.data
                    in
                        ( { model | jobQueue = { job | data = job2Data } :: restQueue }
                        , Cmd.map (NextJobMsg >> NextJob) cmd
                        )

                Nothing ->
                    ( model, Cmd.none )

        NextJob (MetricsMsg newHistory) ->
            case model.msgBeingTracked of
                NextJob Execute ->
                    case List.Extra.uncons model.jobQueue of
                        Just ( job, restQueue ) ->
                            ( { model | jobQueue = { job | history = newHistory } :: restQueue }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                ActiveJob Yield ->
                    case
                        (List.Extra.updateAt
                            ((List.length model.jobQueue) - 1)
                            (\job -> { job | history = newHistory })
                            model.jobQueue
                        )
                    of
                        Just newJobQueue ->
                            ( { model | jobQueue = newJobQueue }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ActiveJob Yield ->
            case model.nextJobStatus of
                Active ->
                    ( { model | nextJobStatus = Queued }, Cmd.none )
                        |> Update.Extra.andThen update (NextJob Skip)

                _ ->
                    ( model, Cmd.none )

        ActiveJob Finish ->
            case model.nextJobStatus of
                Active ->
                    ( { model | nextJobStatus = Queued }, Cmd.none )
                        |> Update.Extra.andThen update (NextJob Drop)

                _ ->
                    ( model, Cmd.none )

        ActiveJob (ActiveJobMsg jobMsg) ->
            case ( model.nextJobStatus, List.Extra.uncons model.jobQueue ) of
                ( Active, Just ( job, restQueue ) ) ->
                    let
                        ( job2Data, cmd ) =
                            Job.update jobMsg job.data
                    in
                        ( { model | jobQueue = { job | data = job2Data } :: restQueue }
                        , Cmd.map (ActiveJobMsg >> ActiveJob) cmd
                        )

                _ ->
                    ( model, Cmd.none )

        Hotkey ShowHints ->
            ( { model | hintsStatus = Shown }, Cmd.none )

        Hotkey HideHints ->
            ( { model | hintsStatus = Hidden }, Cmd.none )

        Hotkey (Triggered hotkey) ->
            let
                nextMsg =
                    case hotkey of
                        Hotkey.N ->
                            NewJob

                        Hotkey.G ->
                            NextJob Execute

                        Hotkey.Y ->
                            ActiveJob Yield

                        Hotkey.S ->
                            NextJob Skip

                        Hotkey.R ->
                            NextJob Drop

                        Hotkey.C ->
                            ActiveJob Finish

                        Hotkey.H ->
                            case model.hintsStatus of
                                Shown ->
                                    Hotkey HideHints

                                Hidden ->
                                    Hotkey ShowHints
            in
                ( model, Task.perform (\x -> nextMsg) (Task.succeed never) )

        Hotkey (HotkeyMsg action) ->
            let
                ( hotkeysPressed, hotkeyTriggered ) =
                    Hotkey.update action model.hotkeysPressed

                modelUpdated =
                    { model | hotkeysPressed = hotkeysPressed }
            in
                case hotkeyTriggered of
                    Just hotkey ->
                        ( modelUpdated, Cmd.none )
                            |> Update.Extra.andThen update (Hotkey (Triggered hotkey))

                    Nothing ->
                        ( modelUpdated, Cmd.none )

        ToggleViewType ->
            case model.viewType of
                WorklogView ->
                    ( { model | viewType = GraphView }, Graph.load (graphConfig model.graphConfig) )

                GraphView ->
                    ( { model | viewType = WorklogView }, Cmd.none )

        SetGraphState graphState ->
            ( { model | graphState = graphState }, Cmd.none )

        GraphControls (ChangeResolution resolution) ->
            let
                graphConfig =
                    model.graphConfig
            in
                case Graph.dateUnitFromString resolution of
                    Ok unit ->
                        ( { model | graphConfig = { graphConfig | resolutionUnit = unit } }, Cmd.none )

                    Err reason ->
                        Debug.crash reason

        GraphControls (ChangeOffsetUnit offsetUnit) ->
            let
                graphConfig =
                    model.graphConfig
            in
                case Graph.dateUnitFromString offsetUnit of
                    Ok unit ->
                        ( { model | graphConfig = { graphConfig | offsetUnit = unit } }, Cmd.none )

                    Err reason ->
                        Debug.crash reason

        GraphControls (ChangeOffsetAmmount ammount) ->
            let
                graphConfig =
                    model.graphConfig

                offsetAmmount =
                    Result.withDefault ((graphConfigStateInit).offsetAmmount) (String.toInt ammount)
            in
                ( { model | graphConfig = { graphConfig | offsetAmmount = offsetAmmount } }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    viewPort
        [ mainSection
            (case model.viewType of
                WorklogView ->
                    [ viewNextScheduledJobTitle model
                    , viewContextSwitchingControls model
                    , viewNextScheduledJobWorklog model
                    , viewActiveJobWorklogForm model
                    , viewHotkeyHintsToggle model
                    ]

                GraphView ->
                    [ viewNextScheduledJobTitle model
                    , viewGraphControls model
                    , viewNextScheduledJobGraph model
                    , viewHotkeyHintsToggle model
                    ]
            )
        ]


viewPort : List (Html Msg) -> Html Msg
viewPort elements =
    div [ class "container flex-container flex-contained" ]
        [ div [ class "row flex-container flex-contained" ] elements ]


mainSection : List (Html Msg) -> Html Msg
mainSection elements =
    div [ class "col s12 flex-container flex-contained" ]
        [ div [ class "section flex-container flex-contained" ] elements
        ]


viewNextScheduledJobTitle : Model -> Html Msg
viewNextScheduledJobTitle model =
    let
        viewJobTitle job =
            Job.viewTitle job.data |> Html.map (NextJobMsg >> NextJob)

        newJobButton =
            span
                []
                [ a
                    [ class "right btn-floating waves-effect waves-light"
                    , onClick NewJob
                    ]
                    [ i
                        [ class "material-icons" ]
                        [ text "add" ]
                    ]
                , span
                    [ class "right"
                    , style
                        [ ( "position", "relative" )
                        , ( "top", "0.5rem" )
                        , ( "left", "-0.22rem" )
                        ]
                    ]
                    [ text (hotkeyHintOrReal model.hintsStatus "Alt+N" "") ]
                ]
    in
        div [ class "row valign-wrapper" ]
            (case ( model.nextJobStatus, List.head model.jobQueue ) of
                ( Queued, Just job ) ->
                    [ span
                        [ class "col s10 m11" ]
                        [ viewJobTitle job ]
                    , span
                        [ class "col s2 m1" ]
                        [ newJobButton ]
                    ]

                ( Active, Just job ) ->
                    [ span
                        [ class "col s12" ]
                        [ viewJobTitle job ]
                    ]

                ( _, Nothing ) ->
                    [ button
                        [ class "col s12 m4 offset-m4  btn btn-large waves-effect waves-light"
                        , onClick NewJob
                        ]
                        [ text "Create your first job" ]
                    ]
            )


viewNextScheduledJobWorklog : Model -> Html Msg
viewNextScheduledJobWorklog model =
    let
        help =
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
                p [ class "flex-scrollable left-align card-panel teal lighten-4 grey-text text-darken-3" ]
                    [ text "Looks like you are new around here! Let me give you a few hints to get started:"
                    , bulletList
                        [ bullet "The goal of this tool is to help you manage the overhead of doing multiple tasks at the same time"
                        , bullet "Tasks in MultitaskOS are called \"jobs\". Click on the button above to create your first job"
                        , bullet "New jobs are scheduled to a queue. MultitaskOS will take care of deciding for you which is the next job you have to work on"
                        , bullet "Each job has a journal to keep a detailed log of any relevant information you might need in the future when coming back to it"
                        , bullet "You can yield a job at any time, and resume working on the next one"
                        , bullet "Thanks to the journal, you can dump or load the context of a job at any time, so that you don't need to keep it in your head!"
                        , bullet "Find out about the hotkeys available by clicking on the help icon on the left-bottom corner, or using the ALT+H hotkey"
                        ]
                    ]
    in
        case List.head model.jobQueue of
            Just job ->
                Job.viewWorklog (model.nextJobStatus == Active) job.data |> Html.map (NextJobMsg >> NextJob)

            Nothing ->
                help


viewActiveJobWorklogForm : Model -> Html Msg
viewActiveJobWorklogForm model =
    case ( model.nextJobStatus, List.head model.jobQueue ) of
        ( Active, Just job ) ->
            let
                submitButtonText =
                    hotkeyHintOrReal model.hintsStatus "Enter" "Save"
            in
                Job.viewWorklogForm submitButtonText job.data |> Html.map (ActiveJobMsg >> ActiveJob)

        _ ->
            Html.text ""


viewGraphControls : Model -> Html Msg
viewGraphControls model =
    div []
        [ div
            [ class "col s2 m1 graph-control" ]
            [ label
                []
                [ text "Since"
                , input
                    [ id "offset-ammount-input"
                    , type_ "number"
                    , Html.Attributes.min "0"
                    , onInput (ChangeOffsetAmmount >> GraphControls)
                    , value (toString model.graphConfig.offsetAmmount)
                    ]
                    []
                ]
            ]
        , div
            [ class "col graph-control graph-control-bundled" ]
            [ label
                []
                [ span [ style [ ( "visibility", "hidden" ) ] ] [ text "unit" ]
                , select
                    [ id "offset-unit-select"
                    , class "browser-default"
                    , Utils.onChange (ChangeOffsetUnit >> GraphControls)
                    ]
                    [ option
                        [ value "Months"
                        , selected (model.graphConfig.offsetUnit == Graph.Months)
                        ]
                        [ text "Months ago" ]
                    , option
                        [ value "Days"
                        , selected (model.graphConfig.offsetUnit == Graph.Days)
                        ]
                        [ text "Days ago" ]
                    , option
                        [ value "Hours"
                        , selected (model.graphConfig.offsetUnit == Graph.Hours)
                        ]
                        [ text "Hours ago" ]
                    , option
                        [ value "Minutes"
                        , selected (model.graphConfig.offsetUnit == Graph.Minutes)
                        ]
                        [ text "Minutes ago" ]
                    ]
                ]
            ]
        , div
            [ class "col graph-control graph-control-next" ]
            [ label
                []
                [ text "Resolution"
                , select
                    [ id "resolution-select"
                    , class "browser-default"
                    , Utils.onChange (ChangeResolution >> GraphControls)
                    ]
                    [ option
                        [ value "Minutes"
                        , selected (model.graphConfig.resolutionUnit == Graph.Minutes)
                        ]
                        [ text "Minutes" ]
                    , option
                        [ value "Hours"
                        , selected (model.graphConfig.resolutionUnit == Graph.Hours)
                        ]
                        [ text "Hours" ]
                    , option
                        [ value "Days"
                        , selected (model.graphConfig.resolutionUnit == Graph.Days)
                        ]
                        [ text "Days" ]
                    , option
                        [ value "Months"
                        , selected (model.graphConfig.resolutionUnit == Graph.Months)
                        ]
                        [ text "Months" ]
                    ]
                ]
            ]
        , viewViewTypeToggle model
        ]


viewNextScheduledJobGraph : Model -> Html Msg
viewNextScheduledJobGraph model =
    case List.head model.jobQueue of
        Just job ->
            Graph.view (graphConfig model.graphConfig) model.graphState (Metrics.getEvents job.history)

        Nothing ->
            Html.text ""


viewHotkeyHintsToggle : Model -> Html Msg
viewHotkeyHintsToggle model =
    div
        [ class "fixed-action-btn hide-on-small-only"
        , style
            [ ( "left", "23px" )
            , ( "right", "auto" )
            ]
        ]
        [ i
            [ class "material-icons"
            , style [ ( "cursor", "pointer" ) ]
            , onMouseDown (Hotkey ShowHints)
            , onMouseUp (Hotkey HideHints)
            ]
            [ text "info_outline" ]
        , span
            [ style
                [ ( "font-size", "0.8em" )
                , ( "vertical-align", "super" )
                , ( "margin-left", "3px" )
                ]
            ]
            [ text (hotkeyHintOrReal model.hintsStatus "Alt+H" "") ]
        ]


viewContextSwitchingControls : Model -> Html Msg
viewContextSwitchingControls model =
    if not (List.isEmpty model.jobQueue) then
        div [] <|
            (case model.nextJobStatus of
                Active ->
                    [ button
                        [ class "waves-effect waves-light btn"
                        , onClick (ActiveJob Yield)
                        ]
                        [ text (hotkeyHintOrReal model.hintsStatus "Alt+Y" "Yield") ]
                    , button
                        [ class "waves-effect waves-light btn"
                        , onClick (ActiveJob Finish)
                        ]
                        [ text (hotkeyHintOrReal model.hintsStatus "Alt+C" "Finish") ]
                    ]

                Queued ->
                    [ button
                        [ class "waves-effect waves-light btn"
                        , onClick (NextJob Execute)
                        ]
                        [ text (hotkeyHintOrReal model.hintsStatus "Alt+G" "Go!") ]
                    , button
                        ([ classList
                            [ ( "waves-effect waves-light btn", True )
                            , ( "disabled", List.length model.jobQueue < 2 )
                            ]
                         , onClick (NextJob Skip)
                         ]
                        )
                        [ text (hotkeyHintOrReal model.hintsStatus "Alt+S" "Skip") ]
                    , button
                        [ class "waves-effect waves-light btn"
                        , onClick (NextJob Drop)
                        ]
                        [ text (hotkeyHintOrReal model.hintsStatus "Alt+R" "Drop") ]
                    ]
            )
                ++ [ viewViewTypeToggle model ]
    else
        text ""


viewViewTypeToggle : Model -> Html Msg
viewViewTypeToggle model =
    let
        icon =
            case model.viewType of
                WorklogView ->
                    "assessment"

                GraphView ->
                    "assignment"
    in
        button
            [ class "waves-effect waves-light btn right icon-btn without-margin-btn"
            , onClick ToggleViewType
            ]
            [ i
                [ class "material-icons" ]
                [ Html.text icon ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ syncModelFromDatabase (decodeValue >> SyncModel)
        , Sub.map (HotkeyMsg >> Hotkey) Hotkey.subscriptions
        ]


metricsConfig : Metrics.Config Msg Model
metricsConfig =
    Metrics.config
        { trackedMsgs = [ NextJob Execute, ActiveJob Yield ]
        , modelGetter =
            (\model ->
                case model.msgBeingTracked of
                    NextJob Execute ->
                        case List.head model.jobQueue of
                            Just job ->
                                job.history

                            Nothing ->
                                Metrics.init

                    ActiveJob Yield ->
                        case
                            (List.Extra.getAt
                                ((List.length model.jobQueue) - 1)
                                model.jobQueue
                            )
                        of
                            Just job ->
                                job.history

                            Nothing ->
                                Metrics.init

                    _ ->
                        Metrics.init
            )
        , msgEncoder =
            (\msg ->
                case msg of
                    NextJob Execute ->
                        Json.Encode.string "NextJob Execute"

                    ActiveJob Yield ->
                        Json.Encode.string "ActiveJob Yield"

                    _ ->
                        Json.Encode.null
            )
        , msgDecoder =
            Json.Decode.string
                |> Json.Decode.andThen
                    (\msg ->
                        case msg of
                            "NextJob Execute" ->
                                Json.Decode.succeed (NextJob Execute)

                            "ActiveJob Yield" ->
                                Json.Decode.succeed (ActiveJob Yield)

                            _ ->
                                Json.Decode.fail ("The msg " ++ msg ++ " is not being tracked!")
                    )
        , toMsg = MetricsMsg >> NextJob
        }


main : Program (Maybe Json.Encode.Value) Model Msg
main =
    Html.programWithFlags
        { init =
            \maybeModel ->
                case maybeModel of
                    Just model ->
                        ( decodeValue model, Cmd.none )

                    Nothing ->
                        ( init, Cmd.none )
        , view = view
        , update =
            \msg model ->
                let
                    ( model2, cmd1 ) =
                        update msg model

                    model3 =
                        case msg of
                            NextJob Execute ->
                                { model2 | msgBeingTracked = msg }

                            ActiveJob Yield ->
                                { model2 | msgBeingTracked = msg }

                            _ ->
                                model2

                    cmd2 =
                        Metrics.track metricsConfig msg model3

                    model2persist =
                        let
                            oldModelEncoded =
                                encode model

                            newModelEncoded =
                                encode model3
                        in
                            if oldModelEncoded /= newModelEncoded then
                                Just newModelEncoded
                            else
                                Nothing
                in
                    ( model3
                    , Cmd.batch
                        [ case model2persist of
                            Just model ->
                                persistModel model

                            Nothing ->
                                Cmd.none
                        , cmd1
                        , cmd2
                        ]
                    )
        , subscriptions = subscriptions
        }


port persistModel : Json.Encode.Value -> Cmd msg


port syncModelFromDatabase : (Json.Encode.Value -> msg) -> Sub msg
