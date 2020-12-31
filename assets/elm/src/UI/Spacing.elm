module UI.Spacing exposing
    ( large
    , medium
    , small
    )

import Element exposing (Attribute, Device, DeviceClass(..), spacing)


small : Device -> Attribute msg
small { class } =
    spacing <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


medium : Device -> Attribute msg
medium { class } =
    spacing <|
        case class of
            Phone ->
                7

            Tablet ->
                10

            _ ->
                15


large : Device -> Attribute msg
large { class } =
    spacing <|
        case class of
            Phone ->
                10

            Tablet ->
                15

            _ ->
                20
