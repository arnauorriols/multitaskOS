port module DirtyHtml.Dropdown exposing (view, init, Model, Msg, update, subscriptions)

import Html
import Html.Attributes
import Defer


type alias Model =
    { deferState : Defer.Model }


type alias Tag msg =
    List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg


view : Tag msg -> Tag msg -> Html.Html msg
view button dropdown =
    Html.span
        []
        [ button
            [ Html.Attributes.class "dropdown-button dirty-dropdown"
            , Html.Attributes.attribute "data-activates" "import-export-menu"
            ]
            []
        , dropdown
            [ Html.Attributes.id "import-export-menu"
            , Html.Attributes.class "dropdown-content"
            ]
            []
        ]


init : Model
init =
    { deferState = Defer.init [ initDirtyDropdown () ] }


type Msg
    = DeferMsg Defer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeferMsg deferMsg ->
            let
                ( newDeferState, newDeferCmd ) =
                    Defer.update deferMsg model.deferState

                newModel =
                    { model | deferState = newDeferState }
            in
                ( newModel, Cmd.map DeferMsg newDeferCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map DeferMsg (Defer.subscriptions model.deferState)


port initDirtyDropdown : () -> Cmd msg
