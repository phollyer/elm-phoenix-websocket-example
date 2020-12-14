module UI.FontSize exposing
    ( default
    , heading
    , title
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
heading { class } =
    Font.size <|
        case class of
            Phone ->
                20

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
