module View.Example.Controls exposing
    ( Config
    , Control(..)
    , controls
    , group
    , init
    , options
    , userId
    , view
    )

import Element as El exposing (Device, DeviceClass(..), Element, Orientation(..))
import Element.Border as Border
import List.Extra as List
import UI.BorderColor as BorderColor
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import View.Button as Button
import View.Group as Group



{- Model -}


type Config msg
    = Config
        { userId : Maybe String
        , controls : List (Control msg)
        , layout : Maybe (List Int)
        , options : Element msg
        , group : Group.Config
        }


type Control msg
    = Connect msg Bool
    | Disconnect msg Bool
    | Join msg Bool
    | Leave msg Bool
    | Push msg Bool
    | CancelRetry msg Bool
    | CancelPush msg Bool


init : Config msg
init =
    Config
        { userId = Nothing
        , controls = []
        , layout = Nothing
        , options = El.none
        , group = Group.init
        }


userId : Maybe String -> Config msg -> Config msg
userId maybeUserId_ (Config config) =
    Config { config | userId = maybeUserId_ }


controls : List (Control msg) -> Config msg -> Config msg
controls list (Config config) =
    Config { config | controls = list }


options : Element msg -> Config msg -> Config msg
options element (Config config) =
    Config { config | options = element }


group : Group.Config -> Config msg -> Config msg
group group_ (Config config) =
    Config { config | group = group_ }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    let
        elements =
            List.map (toElement device) config.controls
    in
    case Group.layoutForDevice device config.group of
        Nothing ->
            column
                [ maybeUserId config.userId
                , El.column
                    [ El.width El.fill
                    , El.spacing 20
                    ]
                    [ config.options
                    , toRow <|
                        Group.orderForDevice device elements config.group
                    ]
                ]

        Just layout ->
            column
                [ maybeUserId config.userId
                , El.column
                    [ El.width El.fill
                    , El.spacing 20
                    ]
                    [ config.options
                    , El.column
                        [ El.width El.fill
                        , El.spacing 10
                        ]
                      <|
                        toRows layout <|
                            Group.orderForDevice device elements config.group
                    ]
                ]


toElement : Device -> Control msg -> Element msg
toElement device control =
    case control of
        Connect msg enabled ->
            Button.init
                |> Button.setLabel "Connect"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device

        Disconnect msg enabled ->
            Button.init
                |> Button.setLabel "Disconnect"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device

        Join msg enabled ->
            Button.init
                |> Button.setLabel "Join"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device

        Leave msg enabled ->
            Button.init
                |> Button.setLabel "Leave"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device

        Push msg enabled ->
            Button.init
                |> Button.setLabel "Push"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device

        CancelRetry msg enabled ->
            Button.init
                |> Button.setLabel "Cancel Retry"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device

        CancelPush msg enabled ->
            Button.init
                |> Button.setLabel "Cancel Push"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device


column : List (Element msg) -> Element msg
column =
    El.column
        [ Border.widthXY 0 1
        , BorderColor.seperatorLight
        , El.paddingXY 0 10
        , El.spacing 10
        , El.width El.fill
        ]


maybeUserId : Maybe String -> Element msg
maybeUserId maybeId_ =
    case maybeId_ of
        Nothing ->
            El.none

        Just id ->
            El.paragraph
                [ FontFamily.default ]
                [ El.el
                    [ FontColor.label ]
                    (El.text "User ID: ")
                , El.el
                    [ FontColor.value ]
                    (El.text id)
                ]


toRows : List Int -> List (Element msg) -> List (Element msg)
toRows layout elements_ =
    List.groupsOfVarying layout elements_
        |> List.map
            (\elements__ ->
                El.wrappedRow
                    [ El.spacing 10
                    , El.centerX
                    ]
                    [ toRow elements__ ]
            )


toRow : List (Element msg) -> Element msg
toRow elements_ =
    El.row
        [ El.width El.fill
        , El.spacing 10
        ]
        elements_
