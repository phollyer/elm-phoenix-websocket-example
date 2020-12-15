module UI.FontSize exposing
    ( default
    , heading
    , pageNotFound
    , panelContent
    , panelHeader
    , title
    , vsn
    )

import Element exposing (Attribute, Device, DeviceClass(..), Orientation(..))
import Element.Font as Font


{-| General text
-}
default : Device -> Attribute msg
default { class, orientation } =
    Font.size <|
        case ( class, orientation ) of
            ( Phone, Portrait ) ->
                12

            ( Phone, Landscape ) ->
                14

            ( Tablet, _ ) ->
                20

            _ ->
                24


{-| Page headings
-}
heading : Device -> Attribute msg
heading { class, orientation } =
    Font.size <|
        case ( class, orientation ) of
            ( Phone, Portrait ) ->
                20

            ( Phone, Landscape ) ->
                24

            _ ->
                40


{-| Subject titles e.g. Socket Examples
-}
title : Device -> Attribute msg
title { class } =
    Font.size <|
        case class of
            Phone ->
                18

            _ ->
                30


pageNotFound : Device -> Attribute msg
pageNotFound { class } =
    Font.size <|
        case class of
            Phone ->
                30

            Tablet ->
                36

            _ ->
                40


vsn : Device -> Attribute msg
vsn { class } =
    Font.size <|
        case class of
            Phone ->
                8

            Tablet ->
                10

            _ ->
                12



{- Panels -}


panelHeader : Device -> Attribute msg
panelHeader { class } =
    Font.size <|
        case class of
            Phone ->
                16

            _ ->
                20


panelContent : Device -> Attribute msg
panelContent { class } =
    Font.size <|
        case class of
            Phone ->
                12

            _ ->
                18
