module Main (Model, main) where

{-| Multitask OS.

@docs Model, main

-}

import String
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Native.Main


-- MODEL


{-| There are 3 main entities in our model:
    * thread: Thread on current execution stack
    * threadQueue: List of queued threads, waiting for some execution time
    * newThread: Form data to build a new thread.

The use flow is roughly summarized as follows:
    1. At the start of the world, there is no thread executing or threads waiting on the threadQueue
    2. An empty thread sits in newThread.
    3. The user creates at least one new thread. Creating a thread means adding a newThread to the threadQueue
    4. When the threadQueue holds at least one thread, the OS selects the first thread in the queue and prompts the user to start working on it
    5. When the user starts working on a thread, this thread is stored in the field 'thread' of the Model.
    6. The user cannot work on another thread until she yields or finishes the current thread under execution.
    7. Finishing a thread removes it from the world.
    8. Yielding a thread puts it at the end of the threadQueue.
    9. When the user yields or finishes a task, OS jumps to OP 4.

-}
type alias Model =
  { thread : Maybe Thread
  , threadQueue : List Thread
  , newThread : Thread
  }


type alias Thread =
  { threadName : String
  , worklog : String
  , journal : List String
  }


buildNewModel : Model
buildNewModel =
  case Native.Main.retrievePersistedModel () of
    Just model ->
      model

    Nothing ->
      { thread = Nothing
      , threadQueue = []
      , newThread = buildNewThread
      }


buildNewThread : Thread
buildNewThread =
  { threadName = ""
  , worklog = ""
  , journal = []
  }


flushNewThread : Model -> Model
flushNewThread model =
  { model | newThread = buildNewThread }


executeThread : Thread -> Model -> Model
executeThread thread model =
  { model | thread = Just thread }


stopExecutingThread : Model -> Model
stopExecutingThread model =
  { model | thread = Nothing }


getNextThread : List Thread -> ( Thread, List Thread )
getNextThread threadQueue =
  case threadQueue of
    [] ->
      Debug.crash "Cannot get the next thread of an empty queue!"

    nextThread :: restQueue ->
      ( nextThread, restQueue )


enqueueThread : Thread -> List Thread -> List Thread
enqueueThread thread threadQueue =
  threadQueue ++ [ thread ]


updateThreadQueue : List Thread -> Model -> Model
updateThreadQueue threadQueue model =
  { model | threadQueue = threadQueue }


updateNewThread : Model -> Thread -> Model
updateNewThread model newThread =
  { model | newThread = newThread }


updateCurrentOp : String -> Thread -> Thread
updateCurrentOp newOp thread =
  { thread | threadName = newOp }


updateWorklog : String -> Thread -> Thread
updateWorklog worklog thread =
  { thread | worklog = worklog }


saveWorklogToJournal : String -> Thread -> Thread
saveWorklogToJournal worklog thread =
  if String.isEmpty worklog then
    thread
  else
    { thread | journal = thread.journal ++ [ worklog ] }


flushWorklog : Thread -> Thread
flushWorklog thread =
  { thread | worklog = "" }


updateExecutingThread : Model -> Thread -> Model
updateExecutingThread model thread =
  { model | thread = Just thread }



-- UPDATE


type Action
  = ScheduleTask
  | NoOp
  | YieldTask
  | FinishTask
  | ExecuteNextTask
  | SkipNextTask
  | DropNextTask
  | UpdateOpNewThread String
  | UpdateWorklog Thread String
  | SaveWorklogToJournal Thread


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    ScheduleTask ->
      if String.isEmpty model.newThread.threadName then
        model
      else
        model
          |> updateThreadQueue (enqueueThread model.newThread model.threadQueue)
          |> flushNewThread

    YieldTask ->
      case model.thread of
        Nothing ->
          Debug.crash "Cannot yield an unexisting task!"

        Just thread ->
          model
            |> stopExecutingThread
            |> updateThreadQueue (enqueueThread thread model.threadQueue)

    FinishTask ->
      stopExecutingThread model

    ExecuteNextTask ->
      let
        ( nextThread, restQueue ) =
          getNextThread model.threadQueue
      in
        model
          |> executeThread nextThread
          |> updateThreadQueue restQueue

    SkipNextTask ->
      let
        ( nextThread, restQueue ) =
          getNextThread model.threadQueue
      in
        updateThreadQueue (enqueueThread nextThread restQueue) model

    DropNextTask ->
      let
        ( _, restQueue ) =
          getNextThread model.threadQueue
      in
        updateThreadQueue restQueue model

    UpdateOpNewThread newOp ->
      model.newThread
        |> updateCurrentOp newOp
        |> updateNewThread model

    UpdateWorklog thread worklog ->
      thread
        |> updateWorklog worklog
        |> updateExecutingThread model

    SaveWorklogToJournal thread ->
      thread
        |> saveWorklogToJournal thread.worklog
        |> flushWorklog
        |> updateExecutingThread model



-- VIEW


onEnter : Signal.Address Action -> Action -> Attribute
onEnter address action =
  onKeyDown address
    <| \key ->
        case key of
          13 ->
            action

          _ ->
            NoOp


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "container flex-container flex-contained" ]
    [ div
        [ class "row flex-container flex-contained" ]
        [ div
            [ class "col s4" ]
            [ div
                [ class "section" ]
                [ div
                    [ class "input-field" ]
                    [ input
                        [ id "input-thread-name"
                        , class "validate"
                        , type' "text"
                        , value model.newThread.threadName
                        , on "input" targetValue <| Signal.message address << UpdateOpNewThread
                        , onEnter address ScheduleTask
                        ]
                        []
                    , label
                        [ for "input-thread-name" ]
                        [ text "Task title" ]
                    ]
                , button
                    [ class "waves-effect waves-light btn"
                    , onClick address ScheduleTask
                    ]
                    [ text "Schedule" ]
                ]
            ]
        , div
            [ class "col s8 flex-container flex-contained" ]
            ([ div
                [ class "row" ]
                [ div
                    [ class "col s12" ]
                    <| let
                        thread =
                          case model.thread of
                            Nothing ->
                              case model.threadQueue of
                                [] ->
                                  Nothing

                                threadQueue ->
                                  let
                                    ( nextThread, _ ) =
                                      getNextThread threadQueue
                                  in
                                    Just nextThread

                            Just thread ->
                              Just thread
                       in
                        case thread of
                          Nothing ->
                            [ h5 [ class "section grey-text text-lighten-2" ] [ text "Nothing to work on" ] ]

                          Just thread ->
                            [ h3
                                [ class "grey-text text-darken-2" ]
                                [ text thread.threadName ]
                            ]
                , div
                    [ class "col s12" ]
                    <| case model.thread of
                        Nothing ->
                          case model.threadQueue of
                            [] ->
                              []

                            threadQueue ->
                              [ button
                                  [ class "waves-effect waves-light btn"
                                  , onClick address ExecuteNextTask
                                  ]
                                  [ text "Go!" ]
                              , button
                                  ([ classList
                                      [ ( "waves-effect waves-light btn", True )
                                      , ( "disabled", List.length threadQueue < 2 )
                                      ]
                                   , onClick address SkipNextTask
                                   ]
                                  )
                                  [ text "Skip" ]
                              , button
                                  [ class "waves-effect waves-light btn"
                                  , onClick address DropNextTask
                                  ]
                                  [ text "Drop" ]
                              ]

                        Just thread ->
                          [ button
                              [ class "waves-effect waves-light btn"
                              , onClick address YieldTask
                              ]
                              [ text "Yield" ]
                          , button
                              [ class "waves-effect waves-light btn"
                              , onClick address FinishTask
                              ]
                              [ text "Finished" ]
                          ]
                ]
             ]
              ++ let
                  thread =
                    case model.thread of
                      Nothing ->
                        case model.threadQueue of
                          [] ->
                            Nothing

                          threadQueue ->
                            let
                              ( nextThread, _ ) =
                                getNextThread threadQueue
                            in
                              Just nextThread

                      Just thread ->
                        Just thread
                 in
                  case thread of
                    Nothing ->
                      []

                    Just thread ->
                      [ div
                          [ class "row flex-container flex-contained" ]
                          [ div
                              [ class "flex-container flex-contained col s12" ]
                              [ ul
                                  [ class "grey-text collection with-header flex-scrollable z-depth-1" ]
                                  <| case thread.journal of
                                      [] ->
                                        [ li [ class "collection-item" ] [ text "Nothing logged yet for this task" ] ]

                                      journal ->
                                        List.map (\journalEntry -> li [ class "collection-item" ] [ text journalEntry ]) journal
                              ]
                          ]
                      ]
                        ++ case model.thread of
                            Nothing ->
                              []

                            Just thread ->
                              [ div
                                  [ class "row" ]
                                  [ div
                                      [ class "input-field col s9" ]
                                      [ input
                                          [ id "input-worklog"
                                          , value thread.worklog
                                          , type' "text"
                                          , on "input" targetValue <| Signal.message address << UpdateWorklog thread
                                          , onEnter address <| SaveWorklogToJournal thread
                                          ]
                                          []
                                      , label
                                          [ for "input-worklog" ]
                                          [ text "Journal entry" ]
                                      ]
                                  , div
                                      [ class "input-field col s3" ]
                                      [ button
                                          [ class "waves-effect waves-light btn"
                                          , type' "submit"
                                          , onClick address <| SaveWorklogToJournal thread
                                          ]
                                          [ text "Log" ]
                                      ]
                                  ]
                              ]
            )
        ]
    ]



-- WIRING


{-| Simple Signal Wiring using an Actions tagged union
-}
main : Signal Html
main =
  Signal.map (view actions.address) model


port persistModel : Signal Model
port persistModel =
  Signal.map Native.Main.persistModel model


model : Signal Model
model =
  Signal.foldp update buildNewModel actions.signal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox <| UpdateOpNewThread ""
