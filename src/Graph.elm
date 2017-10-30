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
        , dateUnitFromString
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
import Helpcard


type State
    = Loaded Date.Date
    | Loading


type Config data msg
    = Config
        { toMsg : State -> msg
        , reducer : ( Time.Time, Time.Time ) -> ( data, Time.Time ) -> Time.Time -> Time.Time
        , from : Offset
        , groupBy : GroupBy
        , resolution : Resolution
        }


config :
    { toMsg : State -> msg
    , reducer : ( Time.Time, Time.Time ) -> ( data, Time.Time ) -> Time.Time -> Time.Time
    , from : Offset
    , groupBy : GroupBy
    , resolution : Resolution
    }
    -> Config data msg
config c =
    Config c


load : Config data msg -> Cmd msg
load (Config { toMsg }) =
    Task.perform (Loaded >> toMsg) Date.now


init : State
init =
    Loading


view : Config data msg -> State -> List ( data, Time.Time ) -> Html msg
view config state data =
    case ( data, state ) of
        ( [], _ ) ->
            Helpcard.view
                [ Helpcard.text "You haven't started with this Job yet. This is what you'll see when you start:"
                , Helpcard.bulletlist
                    [ Helpcard.text "A histogram of all the time worked on this card each day"
                    , Helpcard.text "You can change the resolution and time range using the control inputs above the graph"
                    ]
                , Helpcard.text "This is an experimental feature. More functionality is expected in the next releases"
                ]

        ( data, Loaded now ) ->
            viewGraph config now data

        ( data, Loading ) ->
            Html.text "loading"


type DateUnit
    = Months
    | Days
    | Hours
    | Minutes


dateUnitFromString : String -> Result String DateUnit
dateUnitFromString unit =
    case unit of
        "Minutes" ->
            Ok Minutes

        "Hours" ->
            Ok Hours

        "Days" ->
            Ok Days

        "Months" ->
            Ok Months

        _ ->
            Err ("Unsupported date unit => " ++ unit)


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


groupData : GroupBy -> Date.Date -> Date.Date -> List ( data, Time.Time ) -> List ( data, Time.Time )
groupData groupby now groupDate data =
    let
        ( from, to ) =
            groupTimeframe groupby now groupDate
    in
        List.filter (\( _, timestamp ) -> Date.Extra.Compare.is3 Date.Extra.Compare.BetweenOpen (Date.fromTime timestamp) to from) data


groupTimeframe : GroupBy -> Date.Date -> Date.Date -> ( Date.Date, Date.Date )
groupTimeframe groupby now groupDate =
    let
        ensureNotGraterThanNow date =
            if Date.Extra.Compare.is Date.Extra.Compare.After date now then
                now
            else
                date
    in
        case groupby of
            GroupBy Months ammount ->
                ( Date.Extra.TimeUnit.startOfTime Date.Extra.TimeUnit.Month groupDate
                , Date.Extra.TimeUnit.endOfTime Date.Extra.TimeUnit.Month groupDate |> Date.Extra.Duration.add Date.Extra.Duration.Month (ammount - 1) |> ensureNotGraterThanNow
                )

            GroupBy Days ammount ->
                ( Date.Extra.TimeUnit.startOfTime Date.Extra.TimeUnit.Day groupDate
                , Date.Extra.TimeUnit.endOfTime Date.Extra.TimeUnit.Day groupDate |> Date.Extra.Duration.add Date.Extra.Duration.Day (ammount - 1) |> ensureNotGraterThanNow
                )

            GroupBy Hours ammount ->
                ( Date.Extra.TimeUnit.startOfTime Date.Extra.TimeUnit.Hour groupDate
                , Date.Extra.TimeUnit.endOfTime Date.Extra.TimeUnit.Hour groupDate |> Date.Extra.Duration.add Date.Extra.Duration.Hour (ammount - 1) |> ensureNotGraterThanNow
                )

            GroupBy Minutes ammount ->
                ( Date.Extra.TimeUnit.startOfTime Date.Extra.TimeUnit.Minute groupDate
                , Date.Extra.TimeUnit.endOfTime Date.Extra.TimeUnit.Minute groupDate |> Date.Extra.Duration.add Date.Extra.Duration.Minute (ammount - 1) |> ensureNotGraterThanNow
                )


dateFormatConfig : Date.Extra.Config.Config
dateFormatConfig =
    Date.Extra.Config.Config_en_us.config


formatDate : Date.Extra.Config.Config -> Date.Date -> String
formatDate dateFormatConfig date =
    Date.Extra.Format.format dateFormatConfig "%-d/%-m %-H:%M" date


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


viewGraph : Config data msg -> Date.Date -> List ( data, Time.Time ) -> Html msg
viewGraph (Config config) now data =
    let
        groupDataReduced : Date.Date -> List ( data, Time.Time ) -> Time.Time
        groupDataReduced groupDate data =
            List.foldl (config.reducer (groupTimeframe config.groupBy now groupDate |> Tuple.mapFirst Date.toTime |> Tuple.mapSecond Date.toTime)) 0 data

        from : Date.Date
        from =
            fromAsDate config.from now

        graphData =
            List.map
                (\groupDate ->
                    ( formatDate dateFormatConfig groupDate
                    , groupData config.groupBy now groupDate data |> groupDataReduced groupDate |> inResolution config.resolution
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
