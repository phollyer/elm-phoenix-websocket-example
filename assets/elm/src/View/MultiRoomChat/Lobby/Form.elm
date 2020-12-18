module View.MultiRoomChat.Lobby.Form exposing
    ( init
    , onChange
    , onColorChange
    , onSubmit
    , selectedColor
    , text
    , view
    )

import Colors.Alpha as Color
import Element as El exposing (Color, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import List.Extra exposing (greedyGroupsOf)
import UI.FontColor as FontColor
import UI.FontSize as FontSize
import View.Button as Button
import View.InputField as InputField



{- Model -}


type Config msg
    = Config
        { text : String
        , selectedColor : Maybe Color
        , onChange : Maybe (String -> msg)
        , onColorChange : Maybe (Color -> msg)
        , onSubmit : Maybe msg
        }


init : Config msg
init =
    Config
        { text = ""
        , selectedColor = Nothing
        , onChange = Nothing
        , onColorChange = Nothing
        , onSubmit = Nothing
        }


text : String -> Config msg -> Config msg
text text_ (Config config) =
    Config { config | text = text_ }


selectedColor : Maybe Color -> Config msg -> Config msg
selectedColor color (Config config) =
    Config { config | selectedColor = color }


onChange : Maybe (String -> msg) -> Config msg -> Config msg
onChange maybeMsg (Config config) =
    Config { config | onChange = maybeMsg }


onColorChange : Maybe (Color -> msg) -> Config msg -> Config msg
onColorChange maybeMsg (Config config) =
    Config { config | onColorChange = maybeMsg }


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
    case config.onColorChange of
        Nothing ->
            El.none

        Just toMsg ->
            El.column
                [ El.width El.fill
                , El.spacing 10
                ]
                [ El.el
                    [ El.centerX
                    , FontColor.panelHeader
                    , FontSize.title device
                    ]
                    (El.text "Select a Color")
                , El.column
                    [ El.width El.fill
                    , El.spacing 10
                    ]
                    (colorRows device toMsg config.selectedColor)
                ]


colorRows : Device -> (Color -> msg) -> Maybe Color -> List (Element msg)
colorRows { class, orientation } toMsg color =
    let
        numPerRow =
            case ( class, orientation ) of
                ( Phone, Portrait ) ->
                    5

                _ ->
                    10
    in
    List.map (toColor toMsg color) colors
        |> greedyGroupsOf numPerRow
        |> List.map
            (\row ->
                El.row
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
