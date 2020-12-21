module View.MultiRoomChat.Lobby.Occupants exposing
    ( all
    , currentUser
    , init
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (Font)
import Type.User as User exposing (User)
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
        , currentUser = User.init
        }


all : List User -> Config -> Config
all users (Config config) =
    Config { config | all = users }


currentUser : User -> Config -> Config
currentUser users (Config config) =
    Config { config | currentUser = users }



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
            [ El.width El.fill
            , El.spacing 10
            ]
            (toOccupants device config)
        ]


toOccupants : Device -> Config -> List (Element msg)
toOccupants device (Config config) =
    List.partition (\user -> user == config.currentUser) config.all
        |> Tuple.mapFirst
            (List.map (toOccupant device config.currentUser))
        |> Tuple.mapSecond
            (List.map (toOccupant device config.currentUser))
        |> combine List.append


toOccupant : Device -> User -> User -> Element msg
toOccupant device currentUser_ user =
    El.el
        [ padding device
        , roundedBorder device
        , Background.color user.backgroundColor
        , Border.color user.foregroundColor
        , Border.width 1
        , Font.color user.foregroundColor
        ]
        (El.text <|
            if currentUser_ == user then
                "You"

            else
                user.username
        )


combine : (a -> b -> c) -> ( a, b ) -> c
combine func ( a, b ) =
    func a b



{- Attributes -}


padding : Device -> Attribute msg
padding { class } =
    El.padding <|
        case class of
            Phone ->
                10

            Tablet ->
                14

            _ ->
                20


roundedBorder : Device -> Attribute msg
roundedBorder { class } =
    Border.rounded <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10
