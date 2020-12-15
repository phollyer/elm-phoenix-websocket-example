module UI.BackgroundColor exposing
    ( button
    , buttonDisabled
    , closedRoom
    , content
    , examplePanel
    , messages
    , openRoom
    , ownRoom
    , page
    , panel
    , panelContent
    )

import Colors.Alpha as Alpha
import Colors.Opaque as Color
import Element exposing (Attribute)
import Element.Background as Background


page : Attribute msg
page =
    Background.color Color.aliceblue


content : Attribute msg
content =
    Background.color Color.skyblue


button : Attribute msg
button =
    Background.color Color.darkseagreen


buttonDisabled : Attribute msg
buttonDisabled =
    Background.color Color.grey


messages : Attribute msg
messages =
    Background.color Color.white


panel : Attribute msg
panel =
    Background.color Color.steelblue


panelContent : Attribute msg
panelContent =
    Background.color Color.lightskyblue



{- Examples -}


examplePanel : Attribute msg
examplePanel =
    Background.color Color.white



{- Lobby Room Listing -}


ownRoom : Attribute msg
ownRoom =
    Background.color Color.skyblue


openRoom : Attribute msg
openRoom =
    Background.color (Alpha.lightgreen 0.5)


closedRoom : Attribute msg
closedRoom =
    Background.color Color.lightcoral
