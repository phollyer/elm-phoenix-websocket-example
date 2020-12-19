module View.MultiRoomChat.Lobby.Registration exposing
    ( init
    , onBackgroundColorChange
    , onChange
    , onForegroundColorChange
    , onSubmit
    , selectedBackgroundColor
    , selectedForegroundColor
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
        { username = ""
        , selectedBackgroundColor = Nothing
        , selectedForegroundColor = Nothing
        , onChange = Nothing
        , onBackgroundColorChange = Nothing
        , onForegroundColorChange = Nothing
        , onSubmit = Nothing
        }


username : String -> Config msg -> Config msg
username name (Config config) =
    Config { config | username = name }


selectedBackgroundColor : Maybe Color -> Config msg -> Config msg
selectedBackgroundColor color (Config config) =
    Config { config | selectedBackgroundColor = color }


selectedForegroundColor : Maybe Color -> Config msg -> Config msg
selectedForegroundColor color (Config config) =
    Config { config | selectedForegroundColor = color }


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
            |> Form.selectedBackgroundColor config.selectedBackgroundColor
            |> Form.selectedForegroundColor config.selectedForegroundColor
            |> Form.onChange config.onChange
            |> Form.onBackgroundColorChange config.onBackgroundColorChange
            |> Form.onForegroundColorChange config.onForegroundColorChange
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
