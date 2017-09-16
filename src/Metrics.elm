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


type State data
    = State (List (Event data))


init : State data
init =
    State []


getEvents : State data -> List ( data, Float )
getEvents (State events) =
    List.map (\(Event data timestamp) -> ( data, timestamp )) events


lastEvent : State data -> Maybe ( data, Float )
lastEvent history =
    getEvents history |> List.head


type alias Timestamp =
    Float


type Event data
    = Event data Timestamp


type Config data msg
    = Config
        { dataEncoder : data -> Json.Encode.Value
        , dataDecoder : Json.Decode.Decoder data
        , toMsg : State data -> msg
        }


config :
    { dataEncoder : data -> Json.Encode.Value
    , dataDecoder : Json.Decode.Decoder data
    , toMsg : State data -> msg
    }
    -> Config data msg
config c =
    Config c


track : Config data msg -> State data -> data -> Cmd msg
track (Config { toMsg }) (State history) data =
    let
        storeTask data history timestamp =
            Task.succeed (State ((Event data (Time.inMilliseconds timestamp)) :: history))

        getTimestampTask =
            Time.now

        getTimestampAndStoreTask =
            getTimestampTask |> Task.andThen (storeTask data history)
    in
        Task.perform toMsg getTimestampAndStoreTask


encode : Config data msg -> State data -> Json.Encode.Value
encode (Config { dataEncoder }) state =
    let
        encodeMetricRecord data timestamp =
            Json.Encode.object
                [ ( "msg", dataEncoder data )
                , ( "timestamp", Json.Encode.float timestamp )
                ]

        encodeEvents events =
            Json.Encode.list
                (List.map
                    (\event ->
                        case event of
                            Event data timestamp ->
                                encodeMetricRecord data timestamp
                    )
                    events
                )
    in
        case state of
            State events ->
                Json.Encode.object
                    [ ( "events", encodeEvents events ) ]


decoder : Config data msg -> Json.Decode.Decoder (State data)
decoder (Config { dataDecoder }) =
    let
        eventsDecoder =
            Json.Decode.list
                (Json.Decode.map2
                    Event
                    (Json.Decode.field "msg" dataDecoder)
                    (Json.Decode.field "timestamp" Json.Decode.float)
                )
    in
        Json.Decode.map State (Json.Decode.field "events" eventsDecoder)
