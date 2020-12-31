module UI.FontColor exposing
    ( button
    , buttonDisabled
    , default
    , error
    , function
    , heading
    , label
    , link
    , moduleName
    , mouseOverBackButton
    , panel
    , panelHeader
    , subTitle
    , title
    , value
    , vsn
    )

import Colors.Opaque as Color
import Element exposing (Attribute, Decoration)
import Element.Font as Font



{- Default -}


default : Attribute msg
default =
    Font.color Color.darkslateblue



{- Headers -}


heading : Attribute msg
heading =
    Font.color Color.darkslateblue


panelHeader : Attribute msg
panelHeader =
    Font.color Color.aliceblue



{- Titles -}


title : Attribute msg
title =
    Font.color Color.slateblue


subTitle : Attribute msg
subTitle =
    Font.color Color.lavender



{- Examples -}


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



{- Link -}


link : Attribute msg
link =
    Font.color Color.dodgerblue



{- Error -}


error : Attribute msg
error =
    Font.color Color.pink



{- Buttons -}


button : Attribute msg
button =
    Font.color Color.darkolivegreen


buttonDisabled : Attribute msg
buttonDisabled =
    Font.color Color.darkgrey



{- Code -}


moduleName : Attribute msg
moduleName =
    Font.color Color.orange


function : Attribute msg
function =
    Font.color Color.dimgrey



{- Version -}


vsn : Attribute msg
vsn =
    Font.color Color.deepskyblue
