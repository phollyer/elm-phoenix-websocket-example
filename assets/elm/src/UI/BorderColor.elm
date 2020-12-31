module UI.BorderColor exposing
    ( examplePanel
    , link
    , mouseOverMenuItem
    , none
    , panel
    , seperatorDark
    , seperatorLight
    )

import Colors.Alpha as Alpha
import Colors.Opaque as Color
import Element exposing (Attribute, Decoration)
import Element.Border as Border


none : Attribute msg
none =
    Border.color <|
        Alpha.white 0



{- Link -}


link : Attribute msg
link =
    Border.color Color.darkgray



{- Panels -}


panel : Attribute msg
panel =
    Border.color Color.steelblue


examplePanel : Attribute msg
examplePanel =
    Border.color Color.black



{- Example Menu -}


mouseOverMenuItem : Decoration
mouseOverMenuItem =
    Border.color Color.lavender



{- Seperators -}


seperatorLight : Attribute msg
seperatorLight =
    Border.color Color.aliceblue


seperatorDark : Attribute msg
seperatorDark =
    Border.color Color.lightsteelblue
