module View.MultiRoomChat.Lobby.Registration exposing
    ( backgroundColor
    , backgroundColorError
    , foregroundColor
    , foregroundColorError
    , init
    , onBackgroundColorChange
    , onChange
    , onForegroundColorChange
    , onSubmit
    , username
    , usernameError
    , view
    )

import Colors.Alpha as Color
import Element as El exposing (Attribute, Color, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import List.Extra exposing (greedyGroupsOf)
import Type.ErrorMessage as ErrorMessage exposing (ErrorMessage)
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import UI.FontSize as FontSize
import View.Button as Button
import View.InputField as InputField



{- Model -}


type Config msg
    = Config
        { username : String
        , usernameError : Maybe ErrorMessage
        , backgroundColor : Maybe Color
        , backgroundColorError : Maybe ErrorMessage
        , foregroundColor : Maybe Color
        , foregroundColorError : Maybe ErrorMessage
        , onChange : Maybe (String -> msg)
        , onBackgroundColorChange : Maybe (Color -> msg)
        , onForegroundColorChange : Maybe (Color -> msg)
        , onSubmit : Maybe msg
        }


init : Config msg
init =
    Config
        { username = ""
        , usernameError = Nothing
        , backgroundColor = Nothing
        , backgroundColorError = Nothing
        , foregroundColor = Nothing
        , foregroundColorError = Nothing
        , onChange = Nothing
        , onBackgroundColorChange = Nothing
        , onForegroundColorChange = Nothing
        , onSubmit = Nothing
        }


username : String -> Config msg -> Config msg
username name (Config config) =
    Config { config | username = name }


usernameError : Maybe ErrorMessage -> Config msg -> Config msg
usernameError maybeError (Config config) =
    Config { config | usernameError = maybeError }


backgroundColor : Maybe Color -> Config msg -> Config msg
backgroundColor color (Config config) =
    Config { config | backgroundColor = color }


backgroundColorError : Maybe ErrorMessage -> Config msg -> Config msg
backgroundColorError maybeError (Config config) =
    Config { config | backgroundColorError = maybeError }


foregroundColor : Maybe Color -> Config msg -> Config msg
foregroundColor color (Config config) =
    Config { config | foregroundColor = color }


foregroundColorError : Maybe ErrorMessage -> Config msg -> Config msg
foregroundColorError maybeError (Config config) =
    Config { config | foregroundColorError = maybeError }


onChange : (String -> msg) -> Config msg -> Config msg
onChange toMsg (Config config) =
    Config { config | onChange = Just toMsg }


onBackgroundColorChange : (Color -> msg) -> Config msg -> Config msg
onBackgroundColorChange toMsg (Config config) =
    Config { config | onBackgroundColorChange = Just toMsg }


onForegroundColorChange : (Color -> msg) -> Config msg -> Config msg
onForegroundColorChange toMsg (Config config) =
    Config { config | onForegroundColorChange = Just toMsg }


onSubmit : msg -> Config msg -> Config msg
onSubmit msg (Config config) =
    Config { config | onSubmit = Just msg }



{- View -}


view : Device -> Config msg -> Element msg
view device config =
    El.column
        [ width device
        , El.centerX
        , El.spacing 10
        ]
        [ introduction
        , form device config
        ]


introduction : Element msg
introduction =
    El.column
        [ El.width El.fill
        , El.spacing 10
        , FontFamily.default
        ]
        [ El.paragraph
            [ El.width El.fill ]
            [ El.text "Welcome," ]
        , El.paragraph
            [ El.width El.fill ]
            [ El.text "Please enter a username, and select your colors in order to join the Lobby." ]
        ]


form : Device -> Config msg -> Element msg
form device (Config config) =
    El.column
        [ BackgroundColor.panel
        , Border.rounded 10
        , El.padding 20
        , El.spacing 20
        , El.width El.fill
        ]
        [ inputField device (Config config)
        , errorView device config.usernameError
        , colorsView device (Config config)
        , submitButton device (Config config)
        ]


errorView : Device -> Maybe ErrorMessage -> Element msg
errorView device maybeError =
    case maybeError of
        Nothing ->
            El.none

        Just error ->
            El.el
                [ El.width El.fill
                , FontColor.error
                ]
                (El.text (ErrorMessage.toString error))


inputField : Device -> Config msg -> Element msg
inputField device (Config config) =
    InputField.init
        |> InputField.label "Username"
        |> InputField.text config.username
        |> InputField.onChange config.onChange
        |> InputField.view device


submitButton : Device -> Config msg -> Element msg
submitButton device (Config config) =
    Button.init
        |> Button.setLabel "Join Lobby"
        |> Button.setOnPress config.onSubmit
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
            (colorRows device config.onBackgroundColorChange config.backgroundColor config.foregroundColor)
        , errorView device config.backgroundColorError
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
            (colorRows device config.onForegroundColorChange config.foregroundColor config.backgroundColor)
        , errorView device config.foregroundColorError
        , case ( config.foregroundColor, config.backgroundColor, not <| String.isEmpty config.username ) of
            ( Just fgColor, Just bgColor, True ) ->
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
                        , Background.color bgColor
                        , Border.color fgColor
                        , Border.width 1
                        , El.centerX
                        , Font.color fgColor
                        ]
                        (El.text config.username)
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
                            5

                        _ ->
                            10
            in
            List.map (toColor toMsg altColor selectedColor) colors
                |> greedyGroupsOf numPerRow
                |> List.map
                    (\row ->
                        El.wrappedRow
                            [ El.centerX
                            , El.spacing 10
                            ]
                            row
                    )


toColor : (Color -> msg) -> Maybe Color -> Maybe Color -> Color -> Element msg
toColor toMsg maybeAltColor maybeSelected color =
    El.el
        (List.append (colorAttrs maybeAltColor maybeSelected color) <|
            if Just color == maybeAltColor then
                []

            else
                [ Event.onClick (toMsg color) ]
        )
        El.none


colorAttrs : Maybe Color -> Maybe Color -> Color -> List (Attribute msg)
colorAttrs maybeAltColor maybeSelected color =
    [ El.width <|
        El.px 25
    , El.height <|
        El.px 25
    , Border.color <|
        Color.black 1
    , if Just color == maybeAltColor || Just color == maybeSelected then
        Border.rounded 12

      else
        El.pointer
    , Border.width <|
        if Just color == maybeSelected then
            3

        else
            1
    , Background.color color
    ]


colors : List Color
colors =
    [ Color.white 1
    , Color.beige 1
    , Color.antiquewhite 1
    , Color.mistyrose 1
    , Color.navajowhite 1
    , Color.lightsalmon 1
    , Color.indianred 1
    , Color.crimson 1
    , Color.firebrick 1
    , Color.darkred 1
    , Color.lightgreen 1
    , Color.mediumseagreen 1
    , Color.seagreen 1
    , Color.green 1
    , Color.darkgreen 1
    , Color.powderblue 1
    , Color.lightskyblue 1
    , Color.dodgerblue 1
    , Color.blue 1
    , Color.darkslateblue 1
    , Color.lightYellow1 1
    , Color.lightYellow2 1
    , Color.lightYellow3 1
    , Color.lightYellow4 1
    , Color.yellow 1
    , Color.lightpink 1
    , Color.palevioletred 1
    , Color.hotpink 1
    , Color.deeppink 1
    , Color.mediumvioletred 1
    , Color.lavender 1
    , Color.plum 1
    , Color.orchid 1
    , Color.darkorchid 1
    , Color.purple 1
    , Color.tan 1
    , Color.peru 1
    , Color.chocolate 1
    , Color.sienna 1
    , Color.saddlebrown 1
    , Color.silver 1
    , Color.darkgrey 1
    , Color.grey 1
    , Color.dimgrey 1
    , Color.black 1
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


width : Device -> Attribute msg
width { class, orientation } =
    El.width <|
        case ( class, orientation ) of
            ( Phone, Portrait ) ->
                El.fill

            ( Phone, Landscape ) ->
                El.maximum 400 El.fill

            _ ->
                El.maximum 500 El.fill
