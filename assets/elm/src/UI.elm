module UI exposing (code)

import Colors.Opaque as Color
import Element as El exposing (Element)
import Element.Background as Background
import Element.Font as Font


{-| Formatted [code] snippet.
-}
code : String -> Element msg
code text =
    El.el
        [ Font.family [ Font.typeface "Roboto Mono" ]
        , Background.color Color.gainsboro
        , El.padding 1
        , Font.color Color.black
        ]
        (El.text text)
