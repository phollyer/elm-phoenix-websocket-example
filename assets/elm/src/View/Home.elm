module View.Home exposing
    ( Config
    , channels
    , init
    , presence
    , socket
    , view
    )

import Element as El exposing (Device, DeviceClass(..), Element, Orientation(..))
import UI.FontColor as FontColor
import UI.FontSize as FontSize



{- Model -}


type Config msg
    = Config
        { channels : List (Element msg)
        , presence : List (Element msg)
        , socket : List (Element msg)
        }


init : Config msg
init =
    Config
        { channels = []
        , presence = []
        , socket = []
        }


channels : List (Element msg) -> Config msg -> Config msg
channels channels_ (Config config) =
    Config { config | channels = channels_ }


presence : List (Element msg) -> Config msg -> Config msg
presence presence_ (Config config) =
    Config { config | presence = presence_ }


socket : List (Element msg) -> Config msg -> Config msg
socket socket_ (Config config) =
    Config { config | socket = socket_ }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.column
        [ El.spacing 20
        , El.width El.fill
        ]
        [ container device "Socket Examples" config.socket
        , container device "Channels Examples" config.channels
        , container device "Presence Examples" config.presence
        ]


container : Device -> String -> List (Element msg) -> Element msg
container device title panels =
    El.column
        [ El.centerX
        , El.spacing 10
        ]
        [ El.el
            [ El.centerX
            , FontColor.title
            , FontSize.title device
            ]
            (El.text title)
        , panelsContainer device panels
        ]


panelsContainer : Device -> List (Element msg) -> Element msg
panelsContainer { class, orientation } =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            El.column
                [ El.spacing 10
                , El.width El.fill
                ]

        ( Phone, Landscape ) ->
            El.wrappedRow
                [ El.spacing 10
                , El.width El.fill
                ]

        _ ->
            El.wrappedRow
                [ El.centerX
                , El.spacing 10
                ]
