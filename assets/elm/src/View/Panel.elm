module View.Panel exposing
    ( Config
    , description
    , init
    , onClick
    , title
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import UI.BackgroundColor as BackgroundColor
import UI.BorderColor as BorderColor
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import UI.FontSize as FontSize
import UI.Shadow as Shadow
import Utils exposing (andMaybeEventWith)



{- Model -}


type Config msg
    = Config
        { title : String
        , description : List (List (Element msg))
        , onClick : Maybe msg
        }


init : Config msg
init =
    Config
        { title = ""
        , description = []
        , onClick = Nothing
        }


title : String -> Config msg -> Config msg
title text (Config config) =
    Config { config | title = text }


description : List (List (Element msg)) -> Config msg -> Config msg
description desc (Config config) =
    Config { config | description = desc }


onClick : Maybe msg -> Config msg -> Config msg
onClick maybeMsg (Config config) =
    Config { config | onClick = maybeMsg }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.el
        [ El.alignTop
        , El.height El.fill
        , El.width El.fill
        ]
    <|
        panel device
            config.onClick
            [ header device config.title
            , content device config.description
            ]



{- Panel -}


panel : Device -> Maybe msg -> List (Element msg) -> Element msg
panel device maybeMsg =
    El.column
        (panelAttrs device
            |> andMaybeEventWith maybeMsg Event.onClick
        )


panelAttrs : Device -> List (Attribute msg)
panelAttrs device =
    [ roundedBorder device
    , BackgroundColor.panel
    , Border.width 1
    , BorderColor.panel
    , El.centerX
    , El.clip
    , El.pointer
    , El.mouseDown
        [ Shadow.panel device ]
    , El.mouseOver
        [ Shadow.panel device ]
    , El.height <|
        El.maximum 300 El.fill
    , El.width <|
        El.maximum 250 El.fill
    ]



{- Header -}


header : Device -> String -> Element msg
header device text =
    El.el
        [ El.paddingXY 5 10
        , El.width El.fill
        , FontColor.panelHeader
        , FontSize.panelHeader device
        ]
        (El.paragraph
            [ El.width El.fill
            , Font.center
            ]
            [ El.text text ]
        )



{- Content -}


content : Device -> List (List (Element msg)) -> Element msg
content device paragraphs =
    El.column
        [ BackgroundColor.panelContent
        , El.padding 10
        , El.spacing 10
        , El.height El.fill
        , El.width El.fill
        , Font.justify
        , FontFamily.default
        , FontSize.panelContent device
        ]
        (List.map toParagraph paragraphs)


toParagraph : List (Element msg) -> Element msg
toParagraph paragraph =
    El.paragraph
        [ El.width El.fill ]
        paragraph



{- Attributes -}


roundedBorder : Device -> Attribute msg
roundedBorder { class } =
    Border.rounded <|
        case class of
            Phone ->
                10

            Tablet ->
                14

            _ ->
                20
