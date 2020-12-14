module View.MultiRoomChat.User exposing
    ( init
    , userId
    , username
    , view
    )

import Colors.Opaque as Color
import Element as El exposing (Device, Element)
import Element.Font as Font
import UI.FontColor as FontColor



{- Model -}


type Config
    = Config
        { userId : String
        , username : String
        }


init : Config
init =
    Config
        { userId = ""
        , username = ""
        }


username : String -> Config -> Config
username name (Config config) =
    Config { config | username = name }


userId : String -> Config -> Config
userId id (Config config) =
    Config { config | userId = id }



{- View -}


view : Device -> Config -> Element msg
view _ (Config config) =
    El.column
        [ El.width El.fill
        , El.spacing 10
        ]
        [ El.paragraph
            [ El.spacing 10 ]
            [ El.el
                [ Font.bold
                , FontColor.label
                ]
                (El.text "Username: ")
            , El.el
                [ FontColor.value ]
                (El.text config.username)
            ]
        , El.paragraph
            [ El.spacing 10 ]
            [ El.el
                [ Font.bold
                , FontColor.label
                ]
                (El.text "User ID: ")
            , El.el
                [ FontColor.value ]
                (El.text config.userId)
            ]
        ]
