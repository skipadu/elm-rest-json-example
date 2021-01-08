module RestJson exposing (main)

import Html exposing (Html, div, h1, text)


view : String -> Html msg
view model =
    div []
        [ h1 [] [ text "RestJson" ]
        , div []
            [ text model ]
        ]


main : Html msg
main =
    view "no model yet"
