module Hotkey
    exposing
        ( Hotkey(..)
        , Model
        , init
        , Action(..)
        , update
        , subscriptions
        )

import Char
import Keyboard


--  MODEL


type alias Model =
    List Int


init : Model
init =
    []


type Hotkey
    = N
    | S
    | G


fromChar : Char -> Maybe Hotkey
fromChar char =
    case char |> Char.toLower of
        'n' ->
            Just N

        's' ->
            Just S

        'g' ->
            Just G

        _ ->
            Nothing



-- UPDATE


type Action
    = HotkeyDown Int
    | HotkeyUp Int


update : Action -> Model -> ( Model, Maybe Hotkey )
update action model =
    case action of
        HotkeyDown keycode ->
            let
                modelUpdated =
                    keycode :: model

                alt =
                    18

                armingKey =
                    alt

                isArmed =
                    List.member armingKey modelUpdated

                lastKeyDown =
                    modelUpdated
                        |> List.filter ((/=) armingKey)
                        |> List.head

                hotkey =
                    if isArmed then
                        lastKeyDown |> Maybe.andThen (Char.fromCode >> fromChar)
                    else
                        Nothing
            in
                ( modelUpdated, hotkey )

        HotkeyUp keycode ->
            ( List.filter ((/=) keycode) model, Nothing )



-- VIEW
-- SUBSCRIPTIONS


subscriptions : Sub Action
subscriptions =
    Sub.batch
        [ Keyboard.downs HotkeyDown
        , Keyboard.ups HotkeyUp
        ]
