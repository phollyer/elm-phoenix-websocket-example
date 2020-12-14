module View.MultiRoomChat.Lobby.Registration exposing
    ( init
    , onChange
    , onSubmit
    , username
    , view
    )

import Colors.Opaque as Color
import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import UI.BackgroundColor as BackgroundColor
import UI.FontFamily as FontFamily
import View.MultiRoomChat.Lobby.Form as Form



{- Model -}


type Config msg
    = Config
        { username : String
        , onChange : Maybe (String -> msg)
        , onSubmit : Maybe msg
        }


init : Config msg
init =
    Config
        { username = ""
        , onChange = Nothing
        , onSubmit = Nothing
        }


username : String -> Config msg -> Config msg
username name (Config config) =
    Config { config | username = name }


onChange : (String -> msg) -> Config msg -> Config msg
onChange toMsg (Config config) =
    Config { config | onChange = Just toMsg }


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
            |> Form.onChange config.onChange
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
