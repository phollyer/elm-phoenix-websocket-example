module View.Button exposing
    ( Config
    , Type(..)
    , init
    , setAlignX
    , setAlignY
    , setEnabled
    , setLabel
    , setOnPress
    , setType
    , view
    )

import Element as El exposing (Attribute, Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Type.User as User exposing (RegisteredUser, User)
import UI.Align as Align exposing (X(..), Y(..))
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import UI.Padding as Padding
import UI.RoundedBorder as RoundedBorder
import UI.Shadow as Shadow



{- Model -}


type Config msg
    = Config
        { enabled : Bool
        , label : String
        , onPress : Maybe msg
        , alignX : X
        , alignY : Y
        , type_ : Type
        }


init : Config msg
init =
    Config
        { enabled = True
        , label = ""
        , onPress = Nothing
        , alignX = Center
        , alignY = Middle
        , type_ = Default
        }


type Type
    = Default
    | User RegisteredUser


setEnabled : Bool -> Config msg -> Config msg
setEnabled enabled (Config config) =
    Config { config | enabled = enabled }


setLabel : String -> Config msg -> Config msg
setLabel label_ (Config config) =
    Config { config | label = label_ }


setOnPress : Maybe msg -> Config msg -> Config msg
setOnPress maybe (Config config) =
    Config { config | onPress = maybe }


setAlignX : X -> Config msg -> Config msg
setAlignX alignX (Config config) =
    Config { config | alignX = alignX }


setAlignY : Y -> Config msg -> Config msg
setAlignY alignY (Config config) =
    Config { config | alignY = alignY }


setType : Type -> Config msg -> Config msg
setType type_ (Config config) =
    Config { config | type_ = type_ }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    Input.button
        (List.append
            (attrs config.type_ config.enabled)
            [ Align.x config.alignX
            , Align.y config.alignY
            , Padding.small device
            , RoundedBorder.small device
            ]
        )
        { label = El.text config.label
        , onPress =
            if config.enabled then
                config.onPress

            else
                Nothing
        }


attrs : Type -> Bool -> List (Attribute msg)
attrs type_ enabled =
    case type_ of
        Default ->
            if enabled then
                [ BackgroundColor.button
                , El.mouseOver <|
                    [ Shadow.button ]
                , FontColor.button
                ]

            else
                [ BackgroundColor.buttonDisabled
                , FontColor.buttonDisabled
                ]

        User user ->
            if enabled then
                [ Background.color (User.bgColor user)
                , Border.color (User.fgColor user)
                , Border.width 1
                , El.mouseOver <|
                    [ Border.shadow
                        { size = 1
                        , blur = 3
                        , color = User.fgColor user
                        , offset = ( 0, 0 )
                        }
                    ]
                , Font.color (User.fgColor user)
                ]

            else
                [ BackgroundColor.buttonDisabled
                , FontColor.buttonDisabled
                ]
