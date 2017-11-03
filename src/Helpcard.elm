module Helpcard exposing (view, bulletlist, text, markdown)

import Html
import Html.Attributes
import Markdown


view : List Element -> Html.Html msg
view elements =
    Html.p
        [ Html.Attributes.class "flex-scrollable left-align card-panel teal lighten-4 grey-text text-darken-3 helpcard" ]
        (List.map
            (\element ->
                case element of
                    Text content ->
                        Html.text content

                    Markdown content ->
                        parseMarkdown content

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
                        Html.text content
                            |> asBullet

                    Markdown content ->
                        parseMarkdown content
                            |> asBullet

                    Bulletlist nestedBullets ->
                        viewBulletlist nestedBullets
            )
            bullets
        )


asBullet : Html.Html msg -> Html.Html msg
asBullet content =
    Html.li
        [ Html.Attributes.class "browser-default"
        , Html.Attributes.style
            [ ( "list-style-type", "disc" ) ]
        ]
        [ content ]


type Element
    = Text String
    | Markdown String
    | Bulletlist (List Element)


bulletlist : List Element -> Element
bulletlist bullets =
    Bulletlist bullets


text : String -> Element
text content =
    Text content


markdown : String -> Element
markdown content =
    Markdown content


parseMarkdown : String -> Html.Html msg
parseMarkdown markdown =
    Markdown.toHtml [ Html.Attributes.class "inline-markdown" ] markdown
