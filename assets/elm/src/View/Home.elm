module View.Home exposing
    ( Config
    , channels
    , init
    , introduction
    , presence
    , socket
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Font as Font
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import UI.FontSize as FontSize
import UI.Padding as Padding



{- Model -}


type Config msg
    = Config
        { introduction : List (List (Element msg))
        , channels : List (Element msg)
        , presence : List (Element msg)
        , socket : List (Element msg)
        }


init : Config msg
init =
    Config
        { introduction = []
        , channels = []
        , presence = []
        , socket = []
        }


introduction : List (List (Element msg)) -> Config msg -> Config msg
introduction intro (Config config) =
    Config { config | introduction = intro }


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
        [ introductionView device config.introduction
        , container device
            [ exampleContainer device "Socket Examples" config.socket
            , exampleContainer device "Channels Examples" config.channels
            , exampleContainer device "Presence Example" config.presence
            ]
        ]


introductionView : Device -> List (List (Element msg)) -> Element msg
introductionView device paragraphs =
    El.column
        [ width device
        , El.centerX
        , El.spacing 10
        , Font.center
        , FontFamily.default
        , FontSize.default device
        ]
    <|
        List.map toParagraph paragraphs


toParagraph : List (Element msg) -> Element msg
toParagraph paragragh =
    El.paragraph
        [ El.width El.fill ]
        paragragh


container : Device -> List (Element msg) -> Element msg
container { class, orientation } examples =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            El.column
                [ El.width El.fill
                , El.spacing 20
                , Padding.bottom 10
                ]
                examples

        ( Phone, Landscape ) ->
            El.column
                [ El.centerX
                , El.spacing 20
                , Padding.bottom 10
                ]
                examples

        ( Tablet, _ ) ->
            El.column
                [ El.centerX
                , El.spacing 30
                , Padding.bottom 20
                ]
                examples

        _ ->
            El.row
                [ El.centerX
                , El.spacing 40
                ]
                examples


exampleContainer : Device -> String -> List (Element msg) -> Element msg
exampleContainer device title panels =
    El.column
        [ El.alignTop
        , El.centerX
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



{- Attributes -}


width : Device -> Attribute msg
width { class, orientation } =
    El.width <|
        case ( class, orientation ) of
            ( Phone, Portrait ) ->
                El.fill

            ( Phone, Landscape ) ->
                El.maximum 400 El.fill

            _ ->
                El.maximum 800 El.fill
