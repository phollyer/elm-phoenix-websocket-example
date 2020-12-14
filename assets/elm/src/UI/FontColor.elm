module UI.FontColor exposing
    ( button
    , buttonDisabled
    , closedRoom
    , default
    , function
    , heading
    , label
    , moduleName
    , mouseOverBackButton
    , openRoom
    , ownRoom
    , panel
    , subTitle
    , title
    , value
    )

import Colors.Opaque as Color
import Element exposing (Attribute, Color, Decoration)
import Element.Font as Font


{-| General text
-}
default : Attribute msg
default =
    Font.color Color.darkslateblue


{-| Page headings
-}
heading : Attribute msg
heading =
    Font.color Color.darkslateblue


{-| Subject titles e.g. Socket Examples
-}
title : Attribute msg
title =
    Font.color Color.slateblue


subTitle : Attribute msg
subTitle =
    Font.color Color.lavender


button : Attribute msg
button =
    Font.color Color.darkolivegreen


buttonDisabled : Attribute msg
buttonDisabled =
    Font.color Color.darkgrey


mouseOverBackButton : Decoration
mouseOverBackButton =
    Font.color Color.aliceblue


panel : Attribute msg
panel =
    Font.color Color.skyblue


label : Attribute msg
label =
    Font.color Color.lavender


value : Attribute msg
value =
    Font.color Color.black



{- Code -}


moduleName : Attribute msg
moduleName =
    Font.color Color.orange


function : Attribute msg
function =
    Font.color Color.darkgrey



{- Lobby Room Listing -}


ownRoom : Attribute msg
ownRoom =
    Font.color Color.darkblue


openRoom : Attribute msg
openRoom =
    Font.color Color.green


closedRoom : Attribute msg
closedRoom =
    Font.color Color.firebrick
