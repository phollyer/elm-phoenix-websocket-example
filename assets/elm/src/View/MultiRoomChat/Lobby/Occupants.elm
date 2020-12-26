module View.MultiRoomChat.Lobby.Occupants exposing
    ( all
    , init
    , view
    )

import Element as El exposing (Device, DeviceClass(..), Element)
import Element.Border as Border
import Type.User exposing (RegisteredUser)
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import View.Tag as Tag



{- Model -}


type Config
    = Config
        { all : List RegisteredUser
        , currentUser : RegisteredUser
        }


init : RegisteredUser -> Config
init user =
    Config
        { all = []
        , currentUser = user
        }


all : List RegisteredUser -> Config -> Config
all users (Config config) =
    Config { config | all = users }



{- View -}


view : Device -> Config -> Element msg
view device config =
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
        , El.wrappedRow
            [ El.spacing 10 ]
            (toOccupants device config)
        ]


toOccupants : Device -> Config -> List (Element msg)
toOccupants device (Config config) =
    List.partition (\user -> user == config.currentUser) config.all
        |> Tuple.mapFirst
            (List.map (toOccupant device config.currentUser))
        |> Tuple.mapSecond
            (List.map (Tag.view device config.currentUser))
        |> combine List.append


toOccupant : Device -> RegisteredUser -> RegisteredUser -> Element msg
toOccupant device currentUser user =
    El.row
        []
        [ Tag.view device currentUser user ]


combine : (a -> b -> c) -> ( a, b ) -> c
combine func ( a, b ) =
    func a b
