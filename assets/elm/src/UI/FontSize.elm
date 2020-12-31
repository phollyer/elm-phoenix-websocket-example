module UI.FontSize exposing
    ( default
    , heading
    , pageNotFound
    , panelContent
    , panelHeader
    , small
    , title
    , vsn
    )

import Element exposing (Attribute, Device, DeviceClass(..), Orientation(..))
import Element.Font as Font



{- Default -}


default : Device -> Attribute msg
default { class } =
    Font.size <|
        case class of
            Phone ->
                12

            Tablet ->
                18

            _ ->
                24



{- Sizes -}


small : Device -> Attribute msg
small { class } =
    Font.size <|
        case class of
            Phone ->
                10

            Tablet ->
                14

            _ ->
                20



{- Headers -}


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


panelHeader : Device -> Attribute msg
panelHeader { class } =
    Font.size <|
        case class of
            Phone ->
                16

            Tablet ->
                20

            _ ->
                22



{- Titles -}


title : Device -> Attribute msg
title { class } =
    Font.size <|
        case class of
            Phone ->
                18

            _ ->
                30



{- Content -}


panelContent : Device -> Attribute msg
panelContent { class } =
    Font.size <|
        case class of
            Phone ->
                12

            Tablet ->
                16

            _ ->
                18


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



{- Version -}


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
