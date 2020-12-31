module View.MultiRoomChat.Registration exposing
    ( init
    , onBackgroundColorChange
    , onChange
    , onForegroundColorChange
    , onSubmit
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
import Type.Example exposing (Example(..))
import Type.User exposing (UnregisteredUser)
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import UI.FontSize as FontSize
import UI.Link as Link
import UI.Padding as Padding
import UI.RoundedBorder as RoundedBorder
import UI.Spacing as Spacing
import View.Button as Button
import View.InputField as InputField



{- Types -}


type Config msg
    = Config
        { currentUser : UnregisteredUser
        , onChange : Maybe (String -> msg)
        , onBackgroundColorChange : Maybe (Color -> msg)
        , onForegroundColorChange : Maybe (Color -> msg)
        , onSubmit : Maybe msg
        }



{- Build -}


init : UnregisteredUser -> Config msg
init user =
    Config
        { currentUser = user
        , onChange = Nothing
        , onBackgroundColorChange = Nothing
        , onForegroundColorChange = Nothing
        , onSubmit = Nothing
        }


onChange : (String -> msg) -> Config msg -> Config msg
onChange toMsg (Config config) =
    Config { config | onChange = Just toMsg }


onBackgroundColorChange : (Color -> msg) -> Config msg -> Config msg
onBackgroundColorChange toMsg (Config config) =
    Config { config | onBackgroundColorChange = Just toMsg }


onForegroundColorChange : (Color -> msg) -> Config msg -> Config msg
onForegroundColorChange toMsg (Config config) =
    Config { config | onForegroundColorChange = Just toMsg }


onSubmit : Maybe msg -> Config msg -> Config msg
onSubmit msg (Config config) =
    Config { config | onSubmit = msg }



{- Query -}


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
    , Color.lightgreen 1
    , Color.mediumseagreen 1
    , Color.seagreen 1
    , Color.green 1
    , Color.darkgreen 1
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
    , Color.powderblue 1
    , Color.lightskyblue 1
    , Color.dodgerblue 1
    , Color.blue 1
    , Color.darkslateblue 1
    , Color.silver 1
    , Color.darkgrey 1
    , Color.grey 1
    , Color.dimgrey 1
    , Color.black 1
    ]



{- View -}


view : Device -> Config msg -> Element msg
view ({ class, orientation } as device) config =
    El.column
        [ El.centerX
        , El.width <|
            case ( class, orientation ) of
                ( Phone, Portrait ) ->
                    El.fill

                ( Phone, Landscape ) ->
                    El.maximum 400 El.fill

                _ ->
                    El.maximum 500 El.fill
        , Spacing.large device
        ]
        [ introduction device
        , form device config
        ]



{- Introduction -}


introduction : Device -> Element msg
introduction device =
    El.column
        [ El.width El.fill
        , FontFamily.default
        , Spacing.large device
        ]
        [ El.paragraph
            [ El.width El.fill ]
            [ El.text "Welcome," ]
        , El.paragraph
            [ El.width El.fill ]
            [ El.text "The main source code for this example can be found "
            , Link.srcLink MultiRoomChat
            , El.text "."
            ]
        , El.paragraph
            [ El.width El.fill ]
            [ El.text "Please enter a username, and select your identifying colors in order to join the Lobby." ]
        ]



{- Form -}


form : Device -> Config msg -> Element msg
form device (Config config) =
    El.column
        [ BackgroundColor.panel
        , El.width El.fill
        , Padding.large device
        , RoundedBorder.large device
        , Spacing.large device
        ]
        [ section device
            [ inputField device (Config config)
            , errorView config.currentUser.usernameError
            ]
        , colorsView device (Config config)
        , submitButton device (Config config)
        ]


section : Device -> List (Element msg) -> Element msg
section device =
    El.column
        [ El.width El.fill
        , Spacing.small device
        ]


errorView : Maybe ErrorMessage -> Element msg
errorView maybeError =
    case maybeError of
        Nothing ->
            El.none

        Just error ->
            El.paragraph
                [ FontColor.error ]
                [ El.text (ErrorMessage.toString error) ]


inputField : Device -> Config msg -> Element msg
inputField device (Config config) =
    InputField.init
        |> InputField.label "Username"
        |> InputField.text config.currentUser.username
        |> InputField.onChange config.onChange
        |> InputField.view device


submitButton : Device -> Config msg -> Element msg
submitButton device (Config config) =
    case config.onSubmit of
        Nothing ->
            El.el
                [ El.centerX ]
                (El.text "Joining Lobby...")

        Just _ ->
            Button.init
                |> Button.setLabel "Join Lobby"
                |> Button.setOnPress config.onSubmit
                |> Button.view device


colorsView : Device -> Config msg -> Element msg
colorsView device (Config config) =
    El.column
        [ El.width El.fill
        , Spacing.large device
        ]
        [ section device
            [ El.el
                [ El.centerX
                , FontColor.panelHeader
                , FontSize.title device
                ]
                (El.text "Select a Background Color")
            , El.column
                [ El.width El.fill
                , Spacing.large device
                ]
                (colorRows device config.onBackgroundColorChange config.currentUser.backgroundColor config.currentUser.foregroundColor)
            , El.el
                [ El.centerX ]
                (errorView config.currentUser.backgroundColorError)
            ]
        , section device
            [ El.el
                [ El.centerX
                , FontColor.panelHeader
                , FontSize.title device
                ]
                (El.text "Select a Foreground Color")
            , El.column
                [ El.width El.fill
                , Spacing.large device
                ]
                (colorRows device config.onForegroundColorChange config.currentUser.foregroundColor config.currentUser.backgroundColor)
            , El.el
                [ El.centerX ]
                (errorView config.currentUser.foregroundColorError)
            ]
        , case ( config.currentUser.foregroundColor, config.currentUser.backgroundColor, not <| String.isEmpty config.currentUser.username ) of
            ( Just fgColor, Just bgColor, True ) ->
                El.column
                    [ El.centerX
                    , Spacing.large device
                    ]
                    [ El.el
                        [ El.centerX
                        , FontColor.panelHeader
                        , FontSize.title device
                        ]
                        (El.text "Preview")
                    , El.paragraph
                        [ Background.color bgColor
                        , Border.color fgColor
                        , Border.width 1
                        , Font.center
                        , Font.color fgColor
                        , Padding.small device
                        , RoundedBorder.large device
                        ]
                        [ El.text config.currentUser.username ]
                    ]

            _ ->
                El.none
        ]


colorRows : Device -> Maybe (Color -> msg) -> Maybe Color -> Maybe Color -> List (Element msg)
colorRows ({ class, orientation } as device) maybeMsg selectedColor altColor =
    case maybeMsg of
        Nothing ->
            []

        Just toMsg ->
            List.map (toColor toMsg altColor selectedColor) colors
                |> greedyGroupsOf
                    (case ( class, orientation ) of
                        ( Phone, Portrait ) ->
                            5

                        _ ->
                            10
                    )
                |> List.map (toRow device)


toRow : Device -> List (Element msg) -> Element msg
toRow device =
    El.wrappedRow
        [ El.centerX
        , Spacing.large device
        ]


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
