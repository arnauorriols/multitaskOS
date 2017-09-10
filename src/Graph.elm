module Graph
    exposing
        ( State
        , init
        , Config
        , config
        , load
        , view
        , offset
        , groupBy
        , resolution
        , DateUnit(..)
        )

import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra
import Time
import Date
import Date.Extra.Format
import Date.Extra.TimeUnit
import Date.Extra.Duration
import Date.Extra.Compare
import Date.Extra.Config
import Date.Extra.Config.Config_en_us
import Plot


type State
    = Loaded Date.Date
    | Loading


type Config msg
    = Config
        { toMsg : State -> msg
        , reducer : Time.Time -> ( msg, Time.Time ) -> Time.Time -> Time.Time
        , from : Offset
        , groupBy : GroupBy
        , resolution : Resolution
        }


config :
    { toMsg : State -> msg
    , reducer : Time.Time -> ( msg, Time.Time ) -> Time.Time -> Time.Time
    , from : Offset
    , groupBy : GroupBy
    , resolution : Resolution
    }
    -> Config msg
config c =
    Config c


load : Config msg -> Cmd msg
load (Config { toMsg }) =
    Task.perform (Loaded >> toMsg) Date.now


init : State
init =
    Loading


view : Config msg -> State -> List ( msg, Time.Time ) -> Html msg
view config state data =
    case state of
        Loaded now ->
            viewGraph config now data

        Loading ->
            Html.text "loading"


type DateUnit
    = Months
    | Days
    | Hours
    | Minutes


type Offset
    = Offset DateUnit Int


offset : DateUnit -> Int -> Offset
offset unit ammount =
    Offset unit ammount


type GroupBy
    = GroupBy DateUnit Int


groupBy : DateUnit -> Int -> GroupBy
groupBy unit ammount =
    GroupBy unit ammount


type Resolution
    = Resolution DateUnit


resolution : DateUnit -> Resolution
resolution unit =
    Resolution unit


fromAsDate : Offset -> Date.Date -> Date.Date
fromAsDate offsetConfig now =
    case offsetConfig of
        Offset Months offset ->
            Date.Extra.Duration.add Date.Extra.Duration.Month offset now

        Offset Days offset ->
            Date.Extra.Duration.add Date.Extra.Duration.Day offset now

        Offset Hours offset ->
            Date.Extra.Duration.add Date.Extra.Duration.Hour offset now

        Offset Minutes offset ->
            Date.Extra.Duration.add Date.Extra.Duration.Minute offset now


nextDate : GroupBy -> Date.Date -> Date.Date
nextDate groupby currentDate =
    case groupby of
        GroupBy Months ammount ->
            Date.Extra.Duration.add Date.Extra.Duration.Month ammount currentDate

        GroupBy Days ammount ->
            Date.Extra.Duration.add Date.Extra.Duration.Day ammount currentDate

        GroupBy Hours ammount ->
            Date.Extra.Duration.add Date.Extra.Duration.Hour ammount currentDate

        GroupBy Minutes ammount ->
            Date.Extra.Duration.add Date.Extra.Duration.Minute ammount currentDate


groupDates : GroupBy -> Date.Date -> Date.Date -> List Date.Date
groupDates groupby from to =
    List.Extra.unfoldr
        (\date ->
            if Date.Extra.Compare.is Date.Extra.Compare.After date to then
                Nothing
            else
                Just ( date, nextDate groupby date )
        )
        from


groupData : GroupBy -> Date.Date -> List ( msg, Time.Time ) -> List ( msg, Time.Time )
groupData groupby groupDate data =
    let
        ( from, to ) =
            groupTimeframe groupby groupDate
    in
        List.filter (\( _, timestamp ) -> Date.Extra.Compare.is3 Date.Extra.Compare.BetweenOpen (Date.fromTime timestamp) to from) data


groupTimeframe : GroupBy -> Date.Date -> ( Date.Date, Date.Date )
groupTimeframe groupby groupDate =
    case groupby of
        GroupBy Months ammount ->
            ( Date.Extra.TimeUnit.startOfTime Date.Extra.TimeUnit.Month groupDate
            , Date.Extra.TimeUnit.endOfTime Date.Extra.TimeUnit.Month groupDate
            )

        GroupBy Days ammount ->
            ( Date.Extra.TimeUnit.startOfTime Date.Extra.TimeUnit.Day groupDate
            , Date.Extra.TimeUnit.endOfTime Date.Extra.TimeUnit.Day groupDate |> Date.Extra.Duration.add Date.Extra.Duration.Day (ammount - 1)
            )

        GroupBy Hours ammount ->
            ( Date.Extra.TimeUnit.startOfTime Date.Extra.TimeUnit.Hour groupDate
            , Date.Extra.TimeUnit.endOfTime Date.Extra.TimeUnit.Hour groupDate
            )

        GroupBy Minutes ammount ->
            ( Date.Extra.TimeUnit.startOfTime Date.Extra.TimeUnit.Minute groupDate
            , Date.Extra.TimeUnit.endOfTime Date.Extra.TimeUnit.Minute groupDate
            )


dateFormatConfig : Date.Extra.Config.Config
dateFormatConfig =
    Date.Extra.Config.Config_en_us.config


formatDate : Date.Extra.Config.Config -> Date.Date -> String
formatDate dateFormatConfig date =
    Date.Extra.Format.format dateFormatConfig dateFormatConfig.format.date date


inResolution : Resolution -> Time.Time -> Float
inResolution resolutionConfigured totalSeconds =
    case resolutionConfigured of
        Resolution Months ->
            inResolution (resolution Days) totalSeconds / 30

        Resolution Days ->
            inResolution (resolution Hours) totalSeconds / 24

        Resolution Hours ->
            Time.inHours totalSeconds

        Resolution Minutes ->
            Time.inMinutes totalSeconds


viewGraph : Config msg -> Date.Date -> List ( msg, Time.Time ) -> Html msg
viewGraph (Config config) now data =
    let
        groupDataReduced : List ( msg, Time.Time ) -> Time.Time
        groupDataReduced msgs =
            List.foldl (config.reducer (Date.toTime now)) 0 msgs

        from : Date.Date
        from =
            fromAsDate config.from now

        graphData =
            List.map
                (\groupDate ->
                    ( formatDate dateFormatConfig groupDate
                    , groupData config.groupBy groupDate data |> groupDataReduced |> inResolution config.resolution
                    )
                )
                (groupDates config.groupBy from now)
    in
        div
            [ class "flex-container flex-contained flex-contained-force-grow" ]
            [ Plot.viewBars
                (Plot.histogram
                    (List.map
                        (\( label, ammount ) -> Plot.group label [ ammount ])
                    )
                )
                graphData
            ]
