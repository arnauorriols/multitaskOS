module Metrics
    exposing
        ( State
        , init
        , getEvents
        , Config
        , config
        , encode
        , decoder
        , track
        )

import Json.Encode
import Json.Decode
import Time
import Task


type State msg
    = State (List (Event msg)) (Msg msg)


init : State msg
init =
    State [] NoOp


getEvents : State msg -> List ( msg, Float )
getEvents (State events _) =
    List.map (\(Event msg timestamp) -> ( msg, timestamp )) events


type alias Timestamp =
    Float


type Event msg
    = Event msg Timestamp


type Config msg model
    = Config
        { trackedMsgs : List msg
        , modelGetter : model -> State msg
        , msgEncoder : msg -> Json.Encode.Value
        , msgDecoder : Json.Decode.Decoder msg
        , toMsg : State msg -> msg
        }


config :
    { trackedMsgs : List msg
    , modelGetter : model -> State msg
    , msgEncoder : msg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , toMsg : State msg -> msg
    }
    -> Config msg model
config c =
    Config c


type Msg msg
    = Collect msg Time.Time
    | NoOp


track : Config msg model -> msg -> model -> Cmd msg
track (Config { trackedMsgs, modelGetter, toMsg }) msg model =
    let
        storeTask msg timestamp history =
            Task.succeed (State ((Event msg (Time.inMilliseconds timestamp)) :: history) NoOp)

        storeCmd =
            case modelGetter model of
                State history (Collect msg timestamp) ->
                    Task.perform toMsg (storeTask msg timestamp history)

                state ->
                    Cmd.none

        collectCmd =
            if List.member msg trackedMsgs then
                case modelGetter model of
                    State history _ ->
                        Task.perform (Collect msg >> State history >> toMsg) Time.now
            else
                Cmd.none
    in
        Cmd.batch [ storeCmd, collectCmd ]


encode : Config msg model -> State msg -> Json.Encode.Value
encode (Config { msgEncoder }) state =
    let
        encodeMsgRecord msg timestamp =
            Json.Encode.object
                [ ( "msg", msgEncoder msg )
                , ( "timestamp", Json.Encode.float timestamp )
                ]

        encodeEvents events =
            Json.Encode.list
                (List.map
                    (\event ->
                        case event of
                            Event msg timestamp ->
                                encodeMsgRecord msg timestamp
                    )
                    events
                )

        encodeInternalMsg internalMsg =
            case internalMsg of
                Collect msg timestamp ->
                    Json.Encode.object
                        [ ( "msg", Json.Encode.string "Collect" )
                        , ( "data", encodeMsgRecord msg timestamp )
                        ]

                NoOp ->
                    Json.Encode.object
                        [ ( "msg", Json.Encode.string "NoOp" )
                        , ( "data", Json.Encode.null )
                        ]
    in
        case state of
            State events msg ->
                Json.Encode.object
                    [ ( "events", encodeEvents events )
                    , ( "metricsMsg", encodeInternalMsg msg )
                    ]


decoder : Config msg model -> Json.Decode.Decoder (State msg)
decoder (Config { msgDecoder }) =
    let
        eventsDecoder =
            Json.Decode.list
                (Json.Decode.map2
                    Event
                    (Json.Decode.field "msg" msgDecoder)
                    (Json.Decode.field "timestamp" Json.Decode.float)
                )

        internalMsgDecoder =
            Json.Decode.field "msg" Json.Decode.string
                |> Json.Decode.andThen
                    (\msg ->
                        case msg of
                            "Collect" ->
                                Json.Decode.field "data"
                                    (Json.Decode.map2
                                        Collect
                                        (Json.Decode.field "msg" msgDecoder)
                                        (Json.Decode.field "timestamp" Json.Decode.float)
                                    )

                            "NoOp" ->
                                Json.Decode.succeed NoOp

                            other ->
                                Json.Decode.fail ("Don't know how to decode " ++ other)
                    )
    in
        Json.Decode.map2
            State
            (Json.Decode.field "events" eventsDecoder)
            (Json.Decode.field "metricsMsg" internalMsgDecoder)
