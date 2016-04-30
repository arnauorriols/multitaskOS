module Main (Model, main) where

{-| Multitask OS.

@docs Model, main

-}

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


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
  { currentOp : String }


buildNewModel : Model
buildNewModel =
  { thread = Nothing
  , threadQueue = []
  , newThread = buildNewThread
  }


buildNewThread : Thread
buildNewThread =
  { currentOp = "" }


scheduleNewThread : Model -> Thread -> Model
scheduleNewThread model thread =
  { model
    | threadQueue = model.threadQueue ++ [ thread ]
    , newThread = buildNewThread
  }


executeThread : Thread -> Model -> Model
executeThread thread model =
  { model | thread = Just thread }


stopExecutingThread : Model -> Model
stopExecutingThread model =
  { model | thread = Nothing }


updateThreadQueue : List Thread -> Model -> Model
updateThreadQueue threadQueue model =
  { model | threadQueue = threadQueue }


updateNewThread : Model -> Thread -> Model
updateNewThread model newThread =
  { model | newThread = newThread }


updateCurrentOp : String -> Thread -> Thread
updateCurrentOp newOp thread =
  { thread | currentOp = newOp }



-- UPDATE


type Action
  = ScheduleTask
  | YieldTask
  | ExecuteTask Thread (List Thread)
  | FinishTask
  | UpdateOpNewThread String


update : Action -> Model -> Model
update action model =
  case action of
    ScheduleTask ->
      scheduleNewThread model model.newThread

    YieldTask ->
      case model.thread of
        Nothing ->
          Debug.crash "Cannot yield an unexisting task!"

        Just thread ->
          model
            |> stopExecutingThread
            |> updateThreadQueue (model.threadQueue ++ [ thread ])

    ExecuteTask thread threadQueue ->
      model
        |> updateThreadQueue threadQueue
        |> executeThread thread

    FinishTask ->
      stopExecutingThread model

    UpdateOpNewThread newOp ->
      model.newThread
        |> updateCurrentOp newOp
        |> updateNewThread model



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ div
        []
        [ text "Schedule a new task: "
        , input
            [ value model.newThread.currentOp
            , on "input" targetValue (Signal.message address << UpdateOpNewThread)
            ]
            []
        , button
            [ onClick address ScheduleTask ]
            [ text "Schedule" ]
        ]
    , div
        []
        <| case model.thread of
            Nothing ->
              case model.threadQueue of
                [] ->
                  [ text "Nothing to work on" ]

                nextThread :: threadQueue ->
                  [ text ("Next task: " ++ nextThread.currentOp)
                  , button
                      [ onClick address (ExecuteTask nextThread threadQueue) ]
                      [ text "Go!" ]
                  ]

            Just thread ->
              [ text ("You are currently working on '" ++ thread.currentOp ++ "'")
              , button
                  [ onClick address YieldTask ]
                  [ text "Yield" ]
              , button
                  [ onClick address FinishTask ]
                  [ text "Finished" ]
              ]
    ]



-- WIRING


{-| Simple Signal Wiring using an Actions tagged union
-}
main : Signal Html
main =
  Signal.map (view actions.address) model


model : Signal Model
model =
  Signal.foldp update buildNewModel actions.signal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox <| UpdateOpNewThread ""
