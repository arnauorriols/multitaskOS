module Helpcard exposing (view, bulletlist, text)

import Html
import Html.Attributes


view : List Element -> Html.Html msg
view elements =
    Html.p
        [ Html.Attributes.class "flex-scrollable left-align card-panel teal lighten-4 grey-text text-darken-3" ]
        (List.map
            (\element ->
                case element of
                    Text content ->
                        Html.text content

                    Bulletlist content ->
                        viewBulletlist content
            )
            elements
        )


viewBulletlist : List Element -> Html.Html msg
viewBulletlist bullets =
    Html.ul [ Html.Attributes.class "browser-default" ]
        (List.map
            (\bullet ->
                case bullet of
                    Text content ->
                        Html.li
                            [ Html.Attributes.class "browser-default"
                            , Html.Attributes.style
                                [ ( "list-style-type", "disc" ) ]
                            ]
                            [ Html.text content ]

                    Bulletlist nestedBullets ->
                        viewBulletlist nestedBullets
            )
            bullets
        )


type Element
    = Text String
    | Bulletlist (List Element)


bulletlist : List Element -> Element
bulletlist bullets =
    Bulletlist bullets


text : String -> Element
text content =
    Text content
