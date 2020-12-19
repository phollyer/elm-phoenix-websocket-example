module View.MultiRoomChat.Lobby.Form exposing
    ( init
    , onBackgroundColorChange
    , onChange
    , onForegroundColorChange
    , onSubmit
    , selectedBackgroundColor
    , selectedForegroundColor
    , text
    , view
    )

import Colors.Alpha as Color
import Element as El exposing (Attribute, Color, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import List.Extra exposing (greedyGroupsOf)
import UI.FontColor as FontColor
import UI.FontSize as FontSize
import View.Button as Button
import View.InputField as InputField



{- Model -}


type Config msg
    = Config
        { text : String
        , selectedBackgroundColor : Maybe Color
        , selectedForegroundColor : Maybe Color
        , onChange : Maybe (String -> msg)
        , onBackgroundColorChange : Maybe (Color -> msg)
        , onForegroundColorChange : Maybe (Color -> msg)
        , onSubmit : Maybe msg
        }


init : Config msg
init =
    Config
        { text = ""
        , selectedBackgroundColor = Nothing
        , selectedForegroundColor = Nothing
        , onChange = Nothing
        , onBackgroundColorChange = Nothing
        , onForegroundColorChange = Nothing
        , onSubmit = Nothing
        }


text : String -> Config msg -> Config msg
text text_ (Config config) =
    Config { config | text = text_ }


selectedBackgroundColor : Maybe Color -> Config msg -> Config msg
selectedBackgroundColor color (Config config) =
    Config { config | selectedBackgroundColor = color }


selectedForegroundColor : Maybe Color -> Config msg -> Config msg
selectedForegroundColor color (Config config) =
    Config { config | selectedForegroundColor = color }


onChange : Maybe (String -> msg) -> Config msg -> Config msg
onChange maybeMsg (Config config) =
    Config { config | onChange = maybeMsg }


onBackgroundColorChange : Maybe (Color -> msg) -> Config msg -> Config msg
onBackgroundColorChange maybeMsg (Config config) =
    Config { config | onBackgroundColorChange = maybeMsg }


onForegroundColorChange : Maybe (Color -> msg) -> Config msg -> Config msg
onForegroundColorChange maybeMsg (Config config) =
    Config { config | onForegroundColorChange = maybeMsg }


onSubmit : Maybe msg -> Config msg -> Config msg
onSubmit maybeMsg (Config config) =
    Config { config | onSubmit = maybeMsg }



{- View -}


view : Device -> Config msg -> Element msg
view device config =
    El.column
        [ El.width El.fill
        , El.spacing 20
        ]
        [ inputField device config
        , colorsView device config
        , submitButton device config
        ]


inputField : Device -> Config msg -> Element msg
inputField device (Config config) =
    InputField.init
        |> InputField.label "Username"
        |> InputField.text config.text
        |> InputField.onChange config.onChange
        |> InputField.view device


submitButton : Device -> Config msg -> Element msg
submitButton device (Config config) =
    Button.init
        |> Button.setLabel "Join Lobby"
        |> Button.setOnPress config.onSubmit
        |> Button.setEnabled (String.trim config.text /= "")
        |> Button.view device


colorsView : Device -> Config msg -> Element msg
colorsView device (Config config) =
    El.column
        [ El.width El.fill
        , El.spacing 10
        ]
        [ El.el
            [ El.centerX
            , FontColor.panelHeader
            , FontSize.title device
            ]
            (El.text "Select a Background Color")
        , El.column
            [ El.width El.fill
            , El.spacing 10
            ]
            (colorRows device config.onBackgroundColorChange config.selectedBackgroundColor config.selectedForegroundColor)
        , El.el
            [ El.centerX
            , FontColor.panelHeader
            , FontSize.title device
            ]
            (El.text "Select a Foreground Color")
        , El.column
            [ El.width El.fill
            , El.spacing 10
            ]
            (colorRows device config.onForegroundColorChange config.selectedForegroundColor config.selectedBackgroundColor)
        , case ( config.selectedBackgroundColor, config.selectedForegroundColor ) of
            ( Just bgColor, Just fontColor ) ->
                El.column
                    [ El.width El.fill
                    , El.spacing 10
                    ]
                    [ El.el
                        [ El.centerX
                        , FontColor.panelHeader
                        , FontSize.title device
                        ]
                        (El.text "Preview")
                    , El.el
                        [ padding device
                        , roundedBorder device
                        , El.centerX
                        , Background.color bgColor
                        , Font.color fontColor
                        ]
                        (El.text config.text)
                    ]

            _ ->
                El.none
        ]


colorRows : Device -> Maybe (Color -> msg) -> Maybe Color -> Maybe Color -> List (Element msg)
colorRows { class, orientation } maybeMsg selectedColor altColor =
    case maybeMsg of
        Nothing ->
            []

        Just toMsg ->
            let
                numPerRow =
                    case ( class, orientation ) of
                        ( Phone, Portrait ) ->
                            6

                        _ ->
                            10
            in
            List.filter (\color -> Just color /= altColor) colors
                |> List.map
                    (toColor toMsg selectedColor)
                |> greedyGroupsOf numPerRow
                |> List.map
                    (\row ->
                        El.wrappedRow
                            [ El.centerX
                            , El.spacing 10
                            ]
                            row
                    )


toColor : (Color -> msg) -> Maybe Color -> Color -> Element msg
toColor toMsg maybeSelected color =
    El.el
        [ El.width <|
            El.px 25
        , El.height <|
            El.px 25
        , Border.color <|
            Color.black 1
        , Border.width <|
            if Just color == maybeSelected then
                3

            else
                1
        , El.pointer
        , Background.color color
        , Event.onClick (toMsg color)
        ]
        El.none


colors : List Color
colors =
    [ Color.beige 1
    , Color.blue 1
    , Color.brown 1
    , Color.cyan 1
    , Color.gold 1
    , Color.grey 1
    , Color.lavender 1
    , Color.magenta 1
    , Color.maroon 1
    , Color.orange 1
    , Color.peachpuff 1
    , Color.pink 1
    , Color.purple 1
    , Color.red 1
    , Color.silver 1
    , Color.tan 1
    , Color.teal 1
    , Color.turquoise 1
    , Color.yellow 1
    ]



{- Attributes -}


padding : Device -> Attribute msg
padding { class } =
    El.padding <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


roundedBorder : Device -> Attribute msg
roundedBorder { class } =
    Border.rounded <|
        case class of
            Phone ->
                10

            Tablet ->
                14

            _ ->
                20
