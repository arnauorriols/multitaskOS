module Main (..) where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


-- MODEL


type alias Model =
  { thread : Thread }


type alias Thread =
  { currentOp : String
  , newOp : String
  }



-- Do not operate directly on the object, build an API for it


newModel : Model
newModel =
  { thread = newThread "" }


changeThread : Model -> Thread -> Model
changeThread model thread =
  { model | thread = thread }


newThread : String -> Thread
newThread currentOp =
  { currentOp = currentOp
  , newOp = ""
  }


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
  = ChangeTask
  | UpdateNewOpInput String


update : Action -> Model -> Model
update action model =
  case action of
    ChangeTask ->
      model.thread
        |> saveNewOp
        |> changeThread model

    UpdateNewOpInput newOp ->
      model.thread
        |> updateNewOp newOp
        |> changeThread model



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ text ("You are currently working on: " ++ model.thread.currentOp)
    , input
        [ value model.thread.newOp
        , on "input" targetValue (Signal.message address << UpdateNewOpInput)
        ]
        []
    , button
        [ onClick address ChangeTask ]
        [ text "Save" ]
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
  Signal.mailbox ChangeTask
