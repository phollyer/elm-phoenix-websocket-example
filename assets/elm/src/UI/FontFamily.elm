module UI.FontFamily exposing
    ( backButton
    , code
    , default
    , exampleIntro
    , heading
    )

import Element exposing (Attribute)
import Element.Font as Font



{- Default -}


default : Attribute msg
default =
    Font.family
        [ Font.typeface "Varela Round" ]



{- Headers -}


heading : Attribute msg
heading =
    Font.family
        [ Font.typeface "Oswald" ]



{- Buttons -}


backButton : Attribute msg
backButton =
    Font.family
        [ Font.typeface "Piedra" ]



{- Example Intro -}


exampleIntro : Attribute msg
exampleIntro =
    Font.family
        [ Font.typeface "Piedra" ]



{- Code -}


code : Attribute msg
code =
    Font.family
        [ Font.typeface "Roboto Mono" ]
