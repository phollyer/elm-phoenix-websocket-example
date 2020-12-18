module View.MultiRoomChat.Lobby.Registration exposing
    ( init
    , onChange
    , onColorChange
    , onSubmit
    , selectedColor
    , username
    , view
    )

import Element as El exposing (Attribute, Color, Device, DeviceClass(..), Element, Orientation(..))
import Element.Border as Border
import UI.BackgroundColor as BackgroundColor
import UI.FontFamily as FontFamily
import View.MultiRoomChat.Lobby.Form as Form



{- Model -}


type Config msg
    = Config
        { username : String
        , selectedColor : Maybe Color
        , onChange : Maybe (String -> msg)
        , onColorChange : Maybe (Color -> msg)
        , onSubmit : Maybe msg
        }


init : Config msg
init =
    Config
        { username = ""
        , selectedColor = Nothing
        , onChange = Nothing
        , onColorChange = Nothing
        , onSubmit = Nothing
        }


username : String -> Config msg -> Config msg
username name (Config config) =
    Config { config | username = name }


selectedColor : Maybe Color -> Config msg -> Config msg
selectedColor color (Config config) =
    Config { config | selectedColor = color }


onChange : (String -> msg) -> Config msg -> Config msg
onChange toMsg (Config config) =
    Config { config | onChange = Just toMsg }


onColorChange : (Color -> msg) -> Config msg -> Config msg
onColorChange toMsg (Config config) =
    Config { config | onColorChange = Just toMsg }


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
            [ El.text "Please enter a username in order to join the Lobby." ]
        ]


form : Device -> Config msg -> Element msg
form device (Config config) =
    El.el
        [ BackgroundColor.panel
        , Border.rounded 10
        , El.padding 20
        , El.width El.fill
        ]
        (Form.init
            |> Form.text config.username
            |> Form.selectedColor config.selectedColor
            |> Form.onChange config.onChange
            |> Form.onColorChange config.onColorChange
            |> Form.onSubmit config.onSubmit
            |> Form.view device
        )



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
                El.maximum 500 El.fill
