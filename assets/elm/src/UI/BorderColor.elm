module UI.BorderColor exposing
    ( closedRoom
    , examplePanel
    , mouseOverMenuItem
    , mouseOverOpenRoom
    , mouseOverOwnRoom
    , none
    , openRoom
    , ownRoom
    , seperatorDark
    , seperatorLight
    )

import Colors.Alpha as Alpha
import Colors.Opaque as Color
import Element exposing (Attribute, Decoration)
import Element.Border as Border


none : Attribute msg
none =
    Border.color (Alpha.white 0)


seperatorLight : Attribute msg
seperatorLight =
    Border.color Color.aliceblue


seperatorDark : Attribute msg
seperatorDark =
    Border.color Color.lightsteelblue



{- Examples -}


examplePanel : Attribute msg
examplePanel =
    Border.color Color.black



{- Example Menu -}


mouseOverMenuItem : Decoration
mouseOverMenuItem =
    Border.color Color.lavender



{- Lobby Room Listing -}


ownRoom : Attribute msg
ownRoom =
    Border.color Color.darkblue


mouseOverOwnRoom : Decoration
mouseOverOwnRoom =
    Border.color Color.darkslateblue


mouseOverOpenRoom : Decoration
mouseOverOpenRoom =
    Border.color Color.darkgreen


openRoom : Attribute msg
openRoom =
    Border.color Color.green


closedRoom : Attribute msg
closedRoom =
    Border.color Color.firebrick
