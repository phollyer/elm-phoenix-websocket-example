module UI.BackgroundColor exposing
    ( button
    , buttonDisabled
    , content
    , examplePanel
    , messages
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


messages : Attribute msg
messages =
    Background.color Color.white



{- Buttons -}


button : Attribute msg
button =
    Background.color Color.darkseagreen


buttonDisabled : Attribute msg
buttonDisabled =
    Background.color Color.grey



{- Panels -}


panel : Attribute msg
panel =
    Background.color Color.steelblue


panelContent : Attribute msg
panelContent =
    Background.color Color.lightskyblue


examplePanel : Attribute msg
examplePanel =
    Background.color Color.white
