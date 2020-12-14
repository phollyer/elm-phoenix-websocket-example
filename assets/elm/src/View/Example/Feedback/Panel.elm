module View.Example.Feedback.Panel exposing
    ( Config
    , init
    , scrollable
    , static
    , title
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Border as Border
import Element.Font as Font
import UI.BackgroundColor as BackgroundColor
import UI.BorderColor as BorderColor
import UI.BorderWidth as BorderWidth
import UI.FontColor as FontColor



{- Model -}


type Config msg
    = Config
        { title : String
        , static : List (Element msg)
        , scrollable : List (Element msg)
        }


init : Config msg
init =
    Config
        { title = ""
        , static = []
        , scrollable = []
        }


title : String -> Config msg -> Config msg
title title_ (Config config) =
    Config { config | title = title_ }


scrollable : List (Element msg) -> Config msg -> Config msg
scrollable scrollable_ (Config config) =
    Config { config | scrollable = scrollable_ }


static : List (Element msg) -> Config msg -> Config msg
static static_ (Config config) =
    Config { config | static = static_ }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.column
        [ BackgroundColor.examplePanel
        , Border.width 1
        , BorderColor.examplePanel
        , El.centerX
        , El.padding 10
        , El.height <|
            El.maximum 350 El.fill
        , El.width <|
            El.maximum 500 El.fill
        ]
        [ titleView device config.title
        , staticView config.static
        , scrollableView config.scrollable
        ]


titleView : Device -> String -> Element msg
titleView device title_ =
    El.el
        [ fontSize device
        , El.centerX
        , Font.bold
        , FontColor.title
        ]
        (El.text title_)


staticView : List (Element msg) -> Element msg
staticView elements =
    case elements of
        [] ->
            El.none

        _ ->
            El.column
                (List.append
                    contentAttrs
                    [ BorderWidth.top 2 ]
                )
                elements


scrollableView : List (Element msg) -> Element msg
scrollableView elements =
    case elements of
        [] ->
            El.none

        _ ->
            El.column
                (List.append
                    contentAttrs
                    [ Border.widthXY 0 2
                    , El.clipY
                    , El.scrollbarY
                    , El.height El.fill
                    ]
                )
                (elements
                    |> List.intersperse seperator
                )


seperator : Element msg
seperator =
    El.el
        [ BorderColor.seperatorDark
        , BorderWidth.bottom 1
        , El.width El.fill
        ]
        El.none



{- Attributes -}


contentAttrs : List (Attribute msg)
contentAttrs =
    [ BorderColor.seperatorDark
    , El.spacing 15
    , El.paddingXY 0 10
    , El.width El.fill
    ]


fontSize : Device -> Attribute msg
fontSize { class } =
    case class of
        Phone ->
            Font.size 16

        Tablet ->
            Font.size 20

        _ ->
            Font.size 22
