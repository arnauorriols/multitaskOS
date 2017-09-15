module Metrics
    exposing
        ( State
        , init
        , getEvents
        , lastEvent
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
    = State (List (Event msg))


init : State msg
init =
    State []


getEvents : State msg -> List ( msg, Float )
getEvents (State events) =
    List.map (\(Event msg timestamp) -> ( msg, timestamp )) events


lastEvent : State msg -> Maybe ( msg, Float )
lastEvent history =
    getEvents history |> List.head


type alias Timestamp =
    Float


type Event msg
    = Event msg Timestamp


type Config msg
    = Config
        { msgEncoder : msg -> Json.Encode.Value
        , msgDecoder : Json.Decode.Decoder msg
        , toMsg : State msg -> msg
        }


config :
    { msgEncoder : msg -> Json.Encode.Value
    , msgDecoder : Json.Decode.Decoder msg
    , toMsg : State msg -> msg
    }
    -> Config msg
config c =
    Config c


track : Config msg -> State msg -> msg -> Cmd msg
track (Config { toMsg }) (State history) msg =
    let
        storeTask msg history timestamp =
            Task.succeed (State ((Event msg (Time.inMilliseconds timestamp)) :: history))

        getTimestampTask =
            Time.now

        getTimestampAndStoreTask =
            getTimestampTask |> Task.andThen (storeTask msg history)
    in
        Task.perform toMsg getTimestampAndStoreTask


encode : Config msg -> State msg -> Json.Encode.Value
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
    in
        case state of
            State events ->
                Json.Encode.object
                    [ ( "events", encodeEvents events ) ]


decoder : Config msg -> Json.Decode.Decoder (State msg)
decoder (Config { msgDecoder }) =
    let
        eventsDecoder =
            Json.Decode.list
                (Json.Decode.map2
                    Event
                    (Json.Decode.field "msg" msgDecoder)
                    (Json.Decode.field "timestamp" Json.Decode.float)
                )
    in
        Json.Decode.map State (Json.Decode.field "events" eventsDecoder)
