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
  }


startThread : Model -> Thread -> Model
startThread model thread =
  { model
    | thread = Just thread
    , newThread = buildNewThread ""
  }


stopThread : Model -> Thread -> Model
stopThread model thread =
  { model | thread = Nothing }


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
  | StopTask
  | UpdateNewOpInput String


update : Action -> Model -> Model
update action model =
  case action of
    StartTask ->
      model.newThread
        |> saveNewOp
        |> startThread model

    StopTask ->
      case model.thread of
        Nothing ->
          Debug.crash "Cannot stop an unexisting task!"

        Just thread ->
          stopThread model thread

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
      div
        []
        [ text ("You are currently working on '" ++ thread.currentOp ++ "'")
        , button
            [ onClick address StopTask ]
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
