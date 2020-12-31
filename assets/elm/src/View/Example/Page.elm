module View.Example.Page exposing
    ( Config
    , example
    , init
    , introduction
    , menu
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Font as Font
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import UI.FontSize as FontSize
import UI.Padding as Padding



{- Types -}


type Config msg
    = Config
        { introduction : List (List (Element msg))
        , menu : Element msg
        , example : Element msg
        }



{- Build -}


init : Config msg
init =
    Config
        { introduction = []
        , menu = El.none
        , example = El.none
        }


introduction : List (List (Element msg)) -> Config msg -> Config msg
introduction intro (Config config) =
    Config { config | introduction = intro }


menu : Element msg -> Config msg -> Config msg
menu menu_ (Config config) =
    Config { config | menu = menu_ }


example : Element msg -> Config msg -> Config msg
example example_ (Config config) =
    Config { config | example = example_ }



{- View -}


view : Device -> Config msg -> Element msg
view ({ class } as device) (Config config) =
    El.column
        [ El.height El.fill
        , El.width El.fill
        , El.spacing 10
        , FontColor.default
        , FontSize.default device
        , Padding.bottom 10
        ]
        [ El.column
            [ El.spacing <|
                case class of
                    Phone ->
                        18

                    _ ->
                        22
            , Font.justify
            , FontFamily.exampleIntro
            ]
          <|
            List.map
                (\paragraph ->
                    El.paragraph
                        [ El.width El.fill ]
                        paragraph
                )
                config.introduction
        , config.menu
        , config.example
        ]
