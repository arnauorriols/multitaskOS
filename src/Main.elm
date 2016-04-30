module Main (..) where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


-- MODEL


type alias Model =
  { thread : Maybe Thread
  , newThread : Thread
  }


type alias Thread =
  { currentOp : String
  , newOp : String
  , executing : Bool
  }



-- Do not operate directly on the object, build an API for it


newModel : Model
newModel =
  { thread = Nothing
  , newThread = buildNewThread ""
  }


buildNewThread : String -> Thread
buildNewThread currentOp =
  { currentOp = currentOp
  , newOp = ""
  , executing = False
  }


startNewThread : Model -> Thread -> Model
startNewThread model thread =
  { model
    | thread = Just { thread | executing = True }
    , newThread = buildNewThread ""
  }


pauseThread : Thread -> Thread
pauseThread thread =
  { thread | executing = False }


resumeThread : Thread -> Thread
resumeThread thread =
  { thread | executing = True }


finishThread : Model -> Thread -> Model
finishThread model thread =
  { model | thread = Nothing }


updateThread : Model -> Thread -> Model
updateThread model thread =
  { model | thread = Just thread }


updateNewThread : Model -> Thread -> Model
updateNewThread model newThread =
  { model | newThread = newThread }


updateNewOp : String -> Thread -> Thread
updateNewOp newOp thread =
  { thread | newOp = newOp }


saveNewOp : Thread -> Thread
saveNewOp thread =
  { thread
    | currentOp = thread.newOp
    , newOp = ""
  }



-- UPDATE


type Action
  = StartTask
  | PauseTask
  | ResumeTask
  | FinishTask
  | UpdateNewOpInput String


update : Action -> Model -> Model
update action model =
  case action of
    StartTask ->
      model.newThread
        |> saveNewOp
        |> startNewThread model

    PauseTask ->
      case model.thread of
        Nothing ->
          Debug.crash "Cannot pause an unexisting task!"

        Just thread ->
          thread
            |> pauseThread
            |> updateThread model

    ResumeTask ->
      case model.thread of
        Nothing ->
          Debug.crash "Cannot pause an unexisting task!"

        Just thread ->
          thread
            |> resumeThread
            |> updateThread model

    FinishTask ->
      case model.thread of
        Nothing ->
          Debug.crash "Cannot stop an unexisting task!"

        Just thread ->
          finishThread model thread

    UpdateNewOpInput newOp ->
      model.newThread
        |> updateNewOp newOp
        |> updateNewThread model



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  case model.thread of
    Nothing ->
      div
        []
        [ text "On what will you start working? "
        , input
            [ value model.newThread.newOp
            , on "input" targetValue (Signal.message address << UpdateNewOpInput)
            ]
            []
        , button
            [ onClick address StartTask ]
            [ text "Start" ]
        ]

    Just thread ->
      case thread.executing of
        True ->
          div
            []
            [ text ("You are currently working on '" ++ thread.currentOp ++ "'")
            , button
                [ onClick address PauseTask ]
                [ text "Pause" ]
            , button
                [ onClick address FinishTask ]
                [ text "Stop" ]
            ]

        False ->
          div
            []
            [ text ("You have paused while working on '" ++ thread.currentOp ++ "'")
            , button
                [ onClick address ResumeTask ]
                [ text "Resume" ]
            , button
                [ onClick address FinishTask ]
                [ text "Stop" ]
            ]



-- WIRING


main : Signal Html
main =
  Signal.map (view actions.address) model


model : Signal Model
model =
  Signal.foldp update newModel actions.signal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox <| UpdateNewOpInput ""
