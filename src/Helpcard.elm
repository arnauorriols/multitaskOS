module Helpcard exposing (view, bullet, bulletlist, nbulletlist, text)

import Html
import Html.Attributes


view : List HelpcardElement -> Html.Html msg
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


viewBulletlist : List Bullet -> Html.Html msg
viewBulletlist bullets =
    Html.ul [ Html.Attributes.class "browser-default" ]
        (List.map
            (\bullet ->
                case bullet of
                    Bullet content ->
                        Html.li
                            [ Html.Attributes.class "browser-default"
                            , Html.Attributes.style
                                [ ( "list-style-type", "disc" ) ]
                            ]
                            [ Html.text content ]

                    Nested nestedBullets ->
                        viewBulletlist nestedBullets
            )
            bullets
        )


type Bullet
    = Bullet String
    | Nested (List Bullet)


type HelpcardElement
    = Text String
    | Bulletlist (List Bullet)


bullet : String -> Bullet
bullet content =
    Bullet content


nbulletlist : List Bullet -> Bullet
nbulletlist bulletlist =
    Nested bulletlist


bulletlist : List Bullet -> HelpcardElement
bulletlist bullets =
    Bulletlist bullets


text : String -> HelpcardElement
text content =
    Text content
