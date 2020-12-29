module View.Layout exposing
    ( Config
    , body
    , homeMsg
    , init
    , title
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import UI.FontSize as FontSize
import UI.Padding as Padding



{- Model -}


type Config msg
    = Config
        { homeMsg : Maybe msg
        , title : String
        , body : Element msg
        }


init : Config msg
init =
    Config
        { homeMsg = Nothing
        , title = ""
        , body = El.none
        }


homeMsg : Maybe msg -> Config msg -> Config msg
homeMsg msg (Config config) =
    Config { config | homeMsg = msg }


title : String -> Config msg -> Config msg
title text (Config config) =
    Config { config | title = text }


body : Element msg -> Config msg -> Config msg
body body_ (Config config) =
    Config { config | body = body_ }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.column
        [ El.htmlAttribute <|
            Attr.id "layout"
        , El.clipY
        , El.scrollbarY
        , El.inFront (homeButton device config.homeMsg)
        , El.height El.fill
        , El.width El.fill
        , FontColor.heading
        , FontFamily.heading
        , FontSize.heading device
        ]
        [ header device config.title
        , config.body
        ]


header : Device -> String -> Element msg
header device text =
    El.row
        [ El.htmlAttribute <|
            Attr.id "header"
        , El.width El.fill
        , Font.bold
        , Font.underline
        , Padding.bottom 10
        ]
        [ El.paragraph
            [ paddingY device
            , Font.center
            ]
            [ El.text text ]
        ]


homeButton : Device -> Maybe msg -> Element msg
homeButton device maybeMsg =
    case maybeMsg of
        Nothing ->
            El.none

        Just msg ->
            El.el
                [ paddingY device
                , FontFamily.backButton
                ]
            <|
                Input.button
                    [ El.mouseOver
                        [ FontColor.mouseOverBackButton ]
                    ]
                    { label = El.text "<="
                    , onPress = Just msg
                    }



{- Attributes -}


paddingY : Device -> Attribute msg
paddingY { class } =
    Padding.y <|
        case class of
            Phone ->
                5

            _ ->
                10
