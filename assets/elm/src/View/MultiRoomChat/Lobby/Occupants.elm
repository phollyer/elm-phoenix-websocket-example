module View.MultiRoomChat.Lobby.Occupants exposing
    ( all
    , currentUser
    , init
    , view
    )

import Colors.Opaque as Color
import Element as El exposing (Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Types exposing (User, initUser)
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor



{- Model -}


type Config
    = Config
        { all : List User
        , currentUser : User
        }


init : Config
init =
    Config
        { all = []
        , currentUser = initUser
        }


all : List User -> Config -> Config
all users (Config config) =
    Config { config | all = users }


currentUser : User -> Config -> Config
currentUser users (Config config) =
    Config { config | currentUser = users }



{- View -}


view : Device -> Config -> Element msg
view _ config =
    El.column
        [ BackgroundColor.panel
        , Border.rounded 10
        , El.padding 10
        , El.spacing 10
        , El.width El.fill
        , FontColor.panel
        ]
        [ El.el
            [ El.centerX
            , FontColor.subTitle
            ]
            (El.text "Occupants")
        , El.paragraph
            [ El.width El.fill ]
            [ toOccupants config ]
        ]


toOccupants : Config -> Element msg
toOccupants (Config config) =
    List.filter (\occupant -> occupant /= config.currentUser) config.all
        |> List.map .username
        |> List.append [ "You" ]
        |> List.intersperse ", "
        |> String.concat
        |> El.text
