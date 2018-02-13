port module Main exposing (main)

import Debug
import Task
import Json.Decode
import Json.Encode
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Update.Extra
import List.Extra
import Window
import Utils
import Hotkey
import Job
import Metrics
import Graph
import Helpcard
import DirtyHtml.Dropdown


-- MODEL


type alias Model =
    { jobQueue : List (QueuePosition Job.Model)
    , nextJobStatus : JobStatus
    , hotkeysPressed : Hotkey.Model
    , hintsStatus : HotkeyHintStatus
    , viewType : ViewType
    , graphState : Graph.State
    , graphConfig : GraphConfigState
    , windowSize : Window.Size
    , importExportDropdownState : DirtyHtml.Dropdown.Model
    }


type alias ModelPersisted =
    { jobQueue : List (QueuePosition Job.Model)
    , nextJobStatus : JobStatus
    }


modelPersisted : Model -> ModelPersisted
modelPersisted model =
    { jobQueue = model.jobQueue
    , nextJobStatus = model.nextJobStatus
    }


init : Model
init =
    { jobQueue = []
    , hotkeysPressed = Hotkey.init
    , nextJobStatus = Queued
    , hintsStatus = Hidden
    , viewType = WorklogView
    , graphState = Graph.init
    , graphConfig = graphConfigStateInit
    , windowSize = windowSizeInit
    , importExportDropdownState = DirtyHtml.Dropdown.init
    }


type alias QueuePosition dataModel =
    { data : dataModel
    , history : Metrics.State ActivityMetric
    }


type JobStatus
    = Active
    | Queued


type HotkeyHintStatus
    = Shown
    | Hidden


windowSizeInit : Window.Size
windowSizeInit =
    { width = 800, height = 600 }


hotkeyHintOrReal : HotkeyHintStatus -> String -> String -> String
hotkeyHintOrReal hotkeyHintStatus hotkeyHint realText =
    case hotkeyHintStatus of
        Shown ->
            hotkeyHint

        Hidden ->
            realText


type ViewType
    = WorklogView
    | GraphView


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


type ActivityMetric
    = StartActivity
    | StopActivity
    | PauseActivity


graphConfig : GraphConfigState -> Graph.Config ActivityMetric Msg
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
                \( from, to ) ( data, timestamp ) accumulated ->
                    case data of
                        StartActivity ->
                            if accumulated == 0 then
                                to - timestamp
                            else
                                accumulated - (timestamp - from)

                        StopActivity ->
                            accumulated + (timestamp - from)

                        PauseActivity ->
                            accumulated + (timestamp - from)
            }


encode : ModelPersisted -> Json.Encode.Value
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
    in
        Json.Encode.object
            [ ( "jobQueue", Json.Encode.list (List.map encodeQueuePosition model.jobQueue) )
            , ( "nextJobStatus", encodeStatus model.nextJobStatus )
            ]


encodeQueuePosition : QueuePosition Job.Model -> Json.Encode.Value
encodeQueuePosition job =
    Json.Encode.object
        [ ( "data", Job.encode job.data )
        , ( "history", Metrics.encode metricsConfig job.history )
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
            Json.Decode.list queuePositionDecoder
    in
        Json.Decode.map2
            (\jobQueue nextJobStates ->
                Model
                    jobQueue
                    nextJobStates
                    Hotkey.init
                    Hidden
                    WorklogView
                    Graph.init
                    graphConfigStateInit
                    windowSizeInit
                    DirtyHtml.Dropdown.init
            )
            (Json.Decode.field "jobQueue" jobQueueDecoder)
            (Json.Decode.field "nextJobStatus" jobStatusDecoder)


queuePositionDecoder : Json.Decode.Decoder (QueuePosition Job.Model)
queuePositionDecoder =
    (Json.Decode.map2
        QueuePosition
        (Json.Decode.field "data" Job.decoder)
        (Json.Decode.field "history" (Metrics.decoder metricsConfig))
    )


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
    | ReplaceNextJob ReplaceNextJobMsg
    | NewJob
    | NextJob NextJobMsg
    | ActiveJob ActiveJobMsg
    | Hotkey HotkeyMsg
    | ToggleViewType
    | SetGraphState Graph.State
    | GraphControls GraphControlsMsg
    | WindowResize Window.Size
    | InputOutputDropdownMsg DirtyHtml.Dropdown.Msg


type ReplaceNextJobMsg
    = Attempt String
    | Result Json.Decode.Value


type NextJobMsg
    = Execute
    | Skip
    | Drop
    | MetricsMsg (Metrics.State ActivityMetric)
    | NextJobMsg Job.Msg


type ActiveJobMsg
    = Pause
    | Yield
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

        ReplaceNextJob (Result jobJson) ->
            case Json.Decode.decodeValue queuePositionDecoder jobJson of
                Ok job ->
                    case model.jobQueue of
                        nextJob :: rest ->
                            ( { model | jobQueue = job :: rest }, Cmd.none )

                        [] ->
                            ( { model | jobQueue = [ job ] }, Cmd.none )

                Err error ->
                    let
                        log =
                            Debug.log "Error decoding" jobJson
                    in
                        ( model, Cmd.none )

        ReplaceNextJob (Attempt inputId) ->
            ( model, readFile inputId )

        NewJob ->
            case model.nextJobStatus of
                Active ->
                    ( model, Cmd.none )

                Queued ->
                    let
                        newJob =
                            { data = Job.init, history = Metrics.init }
                    in
                        ( { model | jobQueue = newJob :: model.jobQueue, importExportDropdownState = DirtyHtml.Dropdown.init }, Cmd.none )
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
            case Metrics.lastEvent newHistory of
                Just ( StartActivity, ts_ ) ->
                    case List.Extra.uncons model.jobQueue of
                        Just ( job, restQueue ) ->
                            ( { model | jobQueue = { job | history = newHistory } :: restQueue }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Just ( StopActivity, ts_ ) ->
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

                Just ( PauseActivity, ts_ ) ->
                    case List.Extra.uncons model.jobQueue of
                        Just ( job, restQueue ) ->
                            ( { model | jobQueue = { job | history = newHistory } :: restQueue }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ActiveJob Pause ->
            case model.nextJobStatus of
                Active ->
                    ( { model | nextJobStatus = Queued }, Cmd.none )

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

                        Hotkey.L ->
                            NextJob Execute

                        Hotkey.P ->
                            ActiveJob Pause

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

        WindowResize windowSize ->
            ( { model | windowSize = windowSize }, Cmd.none )

        InputOutputDropdownMsg dropdownMsg ->
            let
                ( newDropdownState, newDropdownCmd ) =
                    DirtyHtml.Dropdown.update dropdownMsg model.importExportDropdownState

                newModel =
                    { model | importExportDropdownState = newDropdownState }

                newCmd =
                    Cmd.map InputOutputDropdownMsg newDropdownCmd
            in
                ( newModel, newCmd )



-- VIEW


view : Model -> Html Msg
view model =
    viewPort
        [ mainSection
            (case model.viewType of
                WorklogView ->
                    [ viewNextScheduledJobTitle model
                    , viewContextSwitchingControls model |> ifNotEditingWorklogEntry model
                    , viewNextScheduledJobWorklog model
                    , viewActiveJobWorklogForm model |> ifNotEditingWorklogEntry model
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


ifNotEditingWorklogEntry : Model -> Html msg -> Html msg
ifNotEditingWorklogEntry model view =
    case List.head model.jobQueue of
        Just job ->
            if Job.isEditingWorklogEntry job.data then
                Html.text ""
            else
                view

        Nothing ->
            view


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
        div [ class "row valign-wrapper flex-inflexible" ]
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


viewContextSwitchingControls : Model -> Html Msg
viewContextSwitchingControls model =
    case List.head model.jobQueue of
        Just queuePosition ->
            div [ id "controls-row" ] <|
                (case model.nextJobStatus of
                    Active ->
                        let
                            pauseButton =
                                button
                                    [ class "waves-effect waves-light btn"
                                    , onClick (ActiveJob Pause)
                                    ]
                                    [ text (hotkeyHintOrReal model.hintsStatus "Alt+Y" "Pause") ]

                            yieldButton =
                                button
                                    [ class "waves-effect waves-light btn"
                                    , onClick (ActiveJob Yield)
                                    ]
                                    [ text (hotkeyHintOrReal model.hintsStatus "Alt+Y" "Yield") ]

                            finishButton =
                                button
                                    [ class "waves-effect waves-light btn"
                                    , onClick (ActiveJob Finish)
                                    ]
                                    [ text (hotkeyHintOrReal model.hintsStatus "Alt+C" "Finish") ]
                        in
                            [ pauseButton
                            , yieldButton
                            , finishButton
                            ]

                    Queued ->
                        let
                            loadButton =
                                button
                                    [ class "waves-effect waves-light btn"
                                    , onClick (NextJob Execute)
                                    ]
                                    [ text (hotkeyHintOrReal model.hintsStatus "Alt+L" "Load") ]

                            skipButton =
                                button
                                    ([ classList
                                        [ ( "waves-effect waves-light btn", True )
                                        , ( "disabled", List.length model.jobQueue < 2 )
                                        ]
                                     , onClick (NextJob Skip)
                                     ]
                                    )
                                    [ text (hotkeyHintOrReal model.hintsStatus "Alt+S" "Skip") ]

                            dropButton =
                                button
                                    [ class "waves-effect waves-light btn"
                                    , onClick (NextJob Drop)
                                    ]
                                    [ text (hotkeyHintOrReal model.hintsStatus "Alt+R" "Drop") ]
                        in
                            [ loadButton
                            , skipButton
                            , dropButton
                            ]
                )
                    ++ [ viewImportExportMenu queuePosition
                       , viewViewTypeToggle model
                       ]

        Nothing ->
            text ""


viewNextScheduledJobWorklog : Model -> Html Msg
viewNextScheduledJobWorklog model =
    case List.head model.jobQueue of
        Just job ->
            Job.viewWorklog model.windowSize (model.nextJobStatus == Active) job.data |> Html.map (NextJobMsg >> NextJob)

        Nothing ->
            viewInitialHelpCard


viewInitialHelpCard : Html Msg
viewInitialHelpCard =
    Helpcard.view
        [ Helpcard.text "Looks like you are new around here! Let me give you a few hints to get started:"
        , (Helpcard.bulletlist
            [ Helpcard.text "The goal of this tool is to help you manage the overhead of doing multiple tasks at the same time"
            , Helpcard.text "Tasks in MultitaskOS are called \"jobs\". Click on the button above to create your first job"
            , Helpcard.text "New jobs are scheduled to a queue. MultitaskOS will take care of deciding for you which is the next job you have to work on"
            , Helpcard.text "Each job has a journal to keep a detailed log of any relevant information you might need when resuming the job later on"
            , Helpcard.text "Using the journal, you can dump or load the context of a job at any time. No need to keep it in your head any more!"
            , Helpcard.text "Log in to enable cloud synchronization. You will have your data available everywhere!"
            , Helpcard.text "Find out about the hotkeys available by clicking on the help icon on the left-bottom corner, or using the ALT+H hotkey"
            , Helpcard.text "MultitaskOS is available in the following formats:"
            , Helpcard.bulletlist
                [ Helpcard.markdown "Chrome extension: [Chrome Web Store link](https://chrome.google.com/webstore/detail/multitaskos/ocdlpdmejajjjfcmhggnhbeacmgnabad)"
                , Helpcard.markdown "Website: [Website link](https://arnauorriols.github.io/multitaskOS/)"
                , Helpcard.text "When using Android, make sure to try the \"install to homescreen\" option in Chrome for the best native-like experience"
                ]
            ]
          )
        ]


viewActiveJobWorklogForm : Model -> Html Msg
viewActiveJobWorklogForm model =
    case ( model.nextJobStatus, List.head model.jobQueue ) of
        ( Active, Just job ) ->
            let
                submitButtonText =
                    hotkeyHintOrReal model.hintsStatus "Enter" "Log"
            in
                Job.viewWorklogForm model.windowSize submitButtonText job.data |> Html.map (ActiveJobMsg >> ActiveJob)

        _ ->
            Html.text ""


viewGraphControls : Model -> Html Msg
viewGraphControls model =
    let
        offsetAmmountControl =
            div
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

        offsetUnitControl =
            div
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

        offsetResolutionControl =
            div
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
    in
        case (List.head model.jobQueue) of
            Just queuePosition ->
                div []
                    [ offsetAmmountControl
                    , offsetUnitControl
                    , offsetResolutionControl
                    , viewImportExportMenu queuePosition
                    , viewViewTypeToggle model
                    ]

            Nothing ->
                div [] []


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
            [ class "waves-effect waves-light btn right first-right icon-btn"
            , onClick ToggleViewType
            ]
            [ i
                [ class "material-icons" ]
                [ Html.text icon ]
            ]


viewImportExportMenu : QueuePosition Job.Model -> Html Msg
viewImportExportMenu job =
    let
        urlEncodedJob =
            "data:text/json;charset=utf-8," ++ Http.encodeUri (Json.Encode.encode 4 (encodeQueuePosition job))

        importButton =
            li
                [ class "file-field" ]
                [ span
                    []
                    [ text "Import" ]
                , input
                    [ type_ "file"
                    , id "import-file-input"
                    , value ""
                    , Utils.onChange (\filename -> ReplaceNextJob (Attempt "import-file-input"))
                    ]
                    []
                ]

        exportButton =
            li
                []
                [ a
                    [ downloadAs (job.data.title ++ ".json")
                    , href urlEncodedJob
                    ]
                    [ text "Export" ]
                ]
    in
        DirtyHtml.Dropdown.view
            (\attr children ->
                button
                    (class "waves-effect waves-light btn right icon-btn" :: attr)
                    (i [ class "material-icons" ] [ Html.text "import_export" ] :: children)
            )
            (\attr children ->
                ul
                    (style [ ( "margin-top", "0.5rem" ), ( "overflow-x", "hidden" ) ] :: attr)
                    (importButton :: exportButton :: children)
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ syncModelFromDatabase (decodeValue >> SyncModel)
        , Sub.map (HotkeyMsg >> Hotkey) Hotkey.subscriptions
        , Window.resizes WindowResize
        , Sub.map InputOutputDropdownMsg (DirtyHtml.Dropdown.subscriptions model.importExportDropdownState)
        , fileRead (Result >> ReplaceNextJob)
        ]


metricsConfig : Metrics.Config ActivityMetric Msg
metricsConfig =
    Metrics.config
        { dataEncoder =
            (\data ->
                case data of
                    StartActivity ->
                        Json.Encode.string "NextJob Execute"

                    StopActivity ->
                        Json.Encode.string "ActiveJob Yield"

                    PauseActivity ->
                        Json.Encode.string "ActiveJob Pause"
            )
        , dataDecoder =
            Json.Decode.string
                |> Json.Decode.andThen
                    (\data ->
                        case data of
                            "NextJob Execute" ->
                                Json.Decode.succeed (StartActivity)

                            "ActiveJob Yield" ->
                                Json.Decode.succeed (StopActivity)

                            "ActiveJob Pause" ->
                                Json.Decode.succeed (PauseActivity)

                            _ ->
                                Json.Decode.fail ("Cannot decode " ++ data)
                    )
        , toMsg = MetricsMsg >> NextJob
        }


main : Program (Maybe Json.Encode.Value) Model Msg
main =
    Html.programWithFlags
        { init =
            \maybeModel ->
                let
                    initialModel =
                        case maybeModel of
                            Just model ->
                                decodeValue model

                            Nothing ->
                                init

                    initialCmd =
                        Task.perform WindowResize Window.size
                in
                    ( initialModel, initialCmd )
        , view = view
        , update =
            \msg oldModel ->
                let
                    ( newModel, businessCmd ) =
                        update msg oldModel

                    jobTracked : Maybe (QueuePosition Job.Model)
                    jobTracked =
                        List.head oldModel.jobQueue

                    trackMetricsCmd =
                        case ( jobTracked, msg ) of
                            ( Just job, NextJob Execute ) ->
                                Metrics.track metricsConfig job.history (StartActivity)

                            ( Just job, ActiveJob Yield ) ->
                                Metrics.track metricsConfig job.history (StopActivity)

                            ( Just job, ActiveJob Pause ) ->
                                Metrics.track metricsConfig job.history (PauseActivity)

                            _ ->
                                Cmd.none

                    model2persist =
                        let
                            oldModelPersisted =
                                modelPersisted oldModel

                            newModelPersisted =
                                modelPersisted newModel
                        in
                            if oldModelPersisted /= newModelPersisted then
                                Just newModelPersisted
                            else
                                Nothing
                in
                    ( newModel
                    , Cmd.batch
                        [ case model2persist of
                            Just model ->
                                encode model |> persistModel

                            Nothing ->
                                Cmd.none
                        , businessCmd
                        , trackMetricsCmd
                        ]
                    )
        , subscriptions = subscriptions
        }


port persistModel : Json.Encode.Value -> Cmd msg


port syncModelFromDatabase : (Json.Encode.Value -> msg) -> Sub msg


port readFile : String -> Cmd msg


port fileRead : (Json.Encode.Value -> msg) -> Sub msg
