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
import View.Button as Button
import View.InputField as InputField



{- Model -}


type Config msg
    = Config
        { currentUser : UnregisteredUser
        , onChange : Maybe (String -> msg)
        , onBackgroundColorChange : Maybe (Color -> msg)
        , onForegroundColorChange : Maybe (Color -> msg)
        , onSubmit : Maybe msg
        }


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
            [ El.text "The main source code for this example can be found "
            , Link.srcLink MultiRoomChat
            , El.text "."
            ]
        , El.paragraph
            [ El.width El.fill ]
            [ El.text "Please enter a username, and select your identifying colors in order to join the Lobby." ]
        ]


form : Device -> Config msg -> Element msg
form device (Config config) =
    El.column
        [ BackgroundColor.panel
        , Border.rounded 10
        , El.padding 20
        , spacing device
        , El.width El.fill
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
        [ formSpacing device
        , El.width El.fill
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
        , spacing device
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
                , El.spacing 10
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
                , El.spacing 10
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
                    , El.spacing 10
                    ]
                    [ El.el
                        [ El.centerX
                        , FontColor.panelHeader
                        , FontSize.title device
                        ]
                        (El.text "Preview")
                    , El.paragraph
                        [ padding device
                        , roundedBorder device
                        , Background.color bgColor
                        , Border.color fgColor
                        , Border.width 1
                        , Font.center
                        , Font.color fgColor
                        ]
                        [ El.text config.currentUser.username ]
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


formSpacing : Device -> Attribute msg
formSpacing { class } =
    El.spacing <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


spacing : Device -> Attribute msg
spacing { class } =
    El.spacing <|
        case class of
            Phone ->
                10

            Tablet ->
                15

            _ ->
                20


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
