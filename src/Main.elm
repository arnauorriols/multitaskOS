port module Main exposing (Model, main)

{-| Multitask OS.

@docs Model, main

-}

import Debug
import Json.Decode
import Json.Encode
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Update.Extra
import List.Extra
import Hotkey
import Job


-- MODEL


{-| -}
type alias Model =
    { jobQueue : JobQueue
    , unsavedJob : Job.Model
    , hotkeysPressed : Hotkey.Model
    , hintsStatus : HotkeyHintStatus
    }


type JobStatus
    = Active
    | Queued


type alias JobQueue =
    List ( JobStatus, Job.Model )


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
    , unsavedJob = Job.init
    , hotkeysPressed = Hotkey.init
    , hintsStatus = Hidden
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

        encodeJobTuple ( jobStatus, job ) =
            Json.Encode.list
                [ encodeStatus jobStatus
                , Job.encode job
                ]

        jobQueue =
            List.map encodeJobTuple model.jobQueue
    in
        Json.Encode.object
            [ ( "jobQueue", Json.Encode.list jobQueue )
            , ( "unsavedJob", Job.encode model.unsavedJob )
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
                    (,)
                    (Json.Decode.index 0 jobStatusDecoder)
                    (Json.Decode.index 1 Job.decoder)
                )
    in
        Json.Decode.map4
            Model
            (Json.Decode.field "jobQueue" jobQueueDecoder)
            (Json.Decode.field "unsavedJob" Job.decoder)
            (Json.Decode.succeed Hotkey.init)
            (Json.Decode.succeed Hidden)


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
    | UnsavedJob UnsavedJobMsg
    | NextJob NextJobMsg
    | ActiveJob ActiveJobMsg
    | Hotkey HotkeyMsg


type UnsavedJobMsg
    = Save
    | UnsavedJobMsg Job.Msg


type NextJobMsg
    = Execute
    | Skip
    | Drop
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


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NoOp ->
            ( model, Cmd.none )

        SyncModel model ->
            ( model, Cmd.none )

        UnsavedJob Save ->
            let
                saveUnsavedIntoQueue model =
                    case List.Extra.uncons model.jobQueue of
                        Just ( ( Active, activeJob ), restQueue ) ->
                            { model | jobQueue = ( Active, activeJob ) :: ( Queued, model.unsavedJob ) :: restQueue }

                        _ ->
                            { model | jobQueue = ( Queued, model.unsavedJob ) :: model.jobQueue }

                flushUnsaved model =
                    { model | unsavedJob = Job.init }
            in
                if Job.isValid model.unsavedJob then
                    ( model
                        |> saveUnsavedIntoQueue
                        |> flushUnsaved
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )

        UnsavedJob (UnsavedJobMsg jobMsg) ->
            let
                ( newUnsavedJob, newJobCmd ) =
                    Job.update jobMsg model.unsavedJob
            in
                ( { model | unsavedJob = newUnsavedJob }
                , Cmd.map (UnsavedJobMsg >> UnsavedJob) newJobCmd
                )

        NextJob Execute ->
            case List.Extra.uncons model.jobQueue of
                Just ( ( Queued, job ), restQueue ) ->
                    ( { model | jobQueue = ( Active, job ) :: restQueue }, Cmd.none )
                        |> Update.Extra.andThen update (ActiveJob (ActiveJobMsg Job.focusWorklogForm))

                _ ->
                    ( model, Cmd.none )

        NextJob Skip ->
            case List.Extra.uncons model.jobQueue of
                Just ( ( Queued, job ), restQueue ) ->
                    ( { model | jobQueue = restQueue ++ [ ( Queued, job ) ] }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NextJob Drop ->
            case List.Extra.uncons model.jobQueue of
                Just ( ( Queued, job ), restQueue ) ->
                    ( { model | jobQueue = restQueue }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NextJob (NextJobMsg jobMsg) ->
            case List.Extra.uncons model.jobQueue of
                Just ( ( jobStatus, job ), restQueue ) ->
                    let
                        ( job2, cmd ) =
                            Job.update jobMsg job
                    in
                        ( { model | jobQueue = ( jobStatus, job2 ) :: restQueue }
                        , Cmd.map (NextJobMsg >> NextJob) cmd
                        )

                _ ->
                    ( model, Cmd.none )

        ActiveJob Yield ->
            case List.Extra.uncons model.jobQueue of
                Just ( ( Active, job ), restQueue ) ->
                    ( { model | jobQueue = restQueue ++ [ ( Queued, job ) ] }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ActiveJob Finish ->
            case List.Extra.uncons model.jobQueue of
                Just ( ( Active, job ), restQueue ) ->
                    ( { model | jobQueue = restQueue }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ActiveJob (ActiveJobMsg jobMsg) ->
            case List.Extra.uncons model.jobQueue of
                Just ( ( Active, job ), restQueue ) ->
                    let
                        ( job2, cmd ) =
                            Job.update jobMsg job
                    in
                        ( { model | jobQueue = ( Active, job2 ) :: restQueue }
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
                            UnsavedJob (UnsavedJobMsg Job.focusTitleForm)

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
                ( model, Cmd.none ) |> Update.Extra.andThen update nextMsg

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



-- VIEW


view : Model -> Html Msg
view model =
    viewPort
        [ sideSection
            [ viewNewJobForm model
            , viewHotkeyHintsToggle model
            ]
        , mainSection
            [ viewNextScheduledJobTitle model
            , viewContextSwitchingControls model
            , viewNextScheduledJobWorklog model
            , viewActiveJobWorklogForm model
            ]
        ]


viewPort : List (Html Msg) -> Html Msg
viewPort elements =
    div [ class "container flex-container flex-contained" ]
        [ div [ class "row flex-container flex-contained" ] elements ]


sideSection : List (Html Msg) -> Html Msg
sideSection elements =
    div [ class "col s4 flex-container flex-contained" ]
        [ div [ class "section flex-container flex-contained" ] elements
        ]


mainSection : List (Html Msg) -> Html Msg
mainSection elements =
    div [ class "col s8 flex-container flex-contained" ]
        [ div [ class "section flex-container flex-contained" ] elements
        ]


viewNextScheduledJobTitle : Model -> Html Msg
viewNextScheduledJobTitle model =
    let
        jobTitle =
            case List.head model.jobQueue of
                Just ( _, job ) ->
                    Job.viewTitle job |> Html.map (NextJobMsg >> NextJob)

                Nothing ->
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
                        p [ class "card-panel teal lighten-4 grey-text text-darken-3" ]
                            [ text "Looks like you are new around here! Let me give you a few hints to get started:"
                            , bulletList
                                [ bullet "The goal of this tool is to help you manage the overhead of doing multiple tasks at the same time"
                                , bullet "Tasks in MultitaskOS are called \"jobs\". Add a new job using the form at the left"
                                , bullet "New jobs are scheduled to a queue. MultitaskOS will take care of deciding for you which is the next job you have to work on"
                                , bullet "Each job has a journal to keep a detailed log of any relevant information you might need in the future when coming back to it"
                                , bullet "You can yield a job at any time, and resume working on the next one"
                                , bullet "Thanks to the journal, you can dump or load the context of a job at any time, so that you don't need to keep it in your head!"
                                , bullet "Find out about the hotkeys available by clicking on the help icon on the left-bottom corner, or using the ALT+H hotkey"
                                ]
                            ]
    in
        jobTitle


viewNextScheduledJobWorklog : Model -> Html Msg
viewNextScheduledJobWorklog model =
    case List.head model.jobQueue of
        Just ( jobStatus, job ) ->
            Job.viewWorklog (jobStatus == Active) job |> Html.map (NextJobMsg >> NextJob)

        Nothing ->
            Html.text ""


viewActiveJobWorklogForm : Model -> Html Msg
viewActiveJobWorklogForm model =
    case List.head model.jobQueue of
        Just ( Active, job ) ->
            let
                submitButtonText =
                    hotkeyHintOrReal model.hintsStatus "Enter" "Save"
            in
                Job.viewWorklogForm submitButtonText job |> Html.map (ActiveJobMsg >> ActiveJob)

        _ ->
            Html.text ""


viewNewJobForm : Model -> Html Msg
viewNewJobForm model =
    let
        placeholder =
            hotkeyHintOrReal model.hintsStatus "Alt+N" "New job"
    in
        div
            [ onEnter (UnsavedJob Save)
            ]
            [ Job.viewTitleForm placeholder model.unsavedJob |> Html.map (UnsavedJobMsg >> UnsavedJob)
            , button
                [ class "waves-effect waves-light btn"
                , onClick (UnsavedJob Save)
                ]
                [ text (hotkeyHintOrReal model.hintsStatus "Enter" "Schedule") ]
            ]


viewHotkeyHintsToggle : Model -> Html Msg
viewHotkeyHintsToggle model =
    div
        [ class "fixed-action-btn"
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
    div [] <|
        case List.head model.jobQueue of
            Just ( jobStatus, job ) ->
                case jobStatus of
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

            Nothing ->
                []


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ syncModelFromDatabase (decodeValue >> SyncModel)
        , Sub.map (HotkeyMsg >> Hotkey) Hotkey.subscriptions
        ]



-- WIRING


{-| Simple Signal Wiring using an Msgs tagged union
-}
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
            \action model ->
                let
                    ( newModel, cmd ) =
                        update action model
                in
                    ( newModel, Cmd.batch [ persistModel (encode newModel), cmd ] )
        , subscriptions = subscriptions
        }


port persistModel : Json.Encode.Value -> Cmd msg


port syncModelFromDatabase : (Json.Encode.Value -> msg) -> Sub msg
