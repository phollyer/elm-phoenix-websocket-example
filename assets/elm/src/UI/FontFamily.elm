module UI.FontFamily exposing
    ( backButton
    , code
    , default
    , exampleIntro
    , heading
    )

import Element exposing (Attribute)
import Element.Font as Font


{-| General text, paragraphs etc.
-}
default : Attribute msg
default =
    Font.family
        [ Font.typeface "Varela Round" ]


{-| Page headings.
-}
heading : Attribute msg
heading =
    Font.family
        [ Font.typeface "Oswald" ]


{-| The back button (<=)
-}
backButton : Attribute msg
backButton =
    Font.family
        [ Font.typeface "Piedra" ]


{-| The back button (<=)
-}
exampleIntro : Attribute msg
exampleIntro =
    Font.family
        [ Font.typeface "Piedra" ]


code : Attribute msg
code =
    Font.family
        [ Font.typeface "Roboto Mono" ]
