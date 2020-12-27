module UI.RoundedBorder exposing
    ( large
    , medium
    , small
    )

import Element exposing (Attribute, Device, DeviceClass(..))
import Element.Border as Border


small : Device -> Attribute msg
small { class } =
    Border.rounded <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


medium : Device -> Attribute msg
medium { class } =
    Border.rounded <|
        case class of
            Phone ->
                7

            Tablet ->
                10

            _ ->
                15


large : Device -> Attribute msg
large { class } =
    Border.rounded <|
        case class of
            Phone ->
                10

            Tablet ->
                15

            _ ->
                20
