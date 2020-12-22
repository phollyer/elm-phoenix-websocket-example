module View.Panel exposing
    ( Config
    , description
    , element
    , init
    , onClick
    , title
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..), paragraph)
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import UI.BackgroundColor as BackgroundColor
import UI.BorderColor as BorderColor
import UI.BorderWidth as BorderWidth
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
        , element : Element msg
        , onClick : Maybe msg
        }


init : Config msg
init =
    Config
        { title = ""
        , description = []
        , element = El.none
        , onClick = Nothing
        }


title : String -> Config msg -> Config msg
title text (Config config) =
    Config { config | title = text }


description : List (List (Element msg)) -> Config msg -> Config msg
description desc (Config config) =
    Config { config | description = desc }


element : Element msg -> Config msg -> Config msg
element el (Config config) =
    Config { config | element = el }


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
            , El.column
                [ BackgroundColor.panelContent
                , El.height El.fill
                , El.width El.fill
                ]
                [ descriptionView device config.description
                , elementView config.element
                ]
            ]



{- Panel -}


panel : Device -> Maybe msg -> List (Element msg) -> Element msg
panel device maybeMsg =
    El.column
        (panelAttrs device maybeMsg
            |> andMaybeEventWith maybeMsg Event.onClick
        )



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



{- Description -}


descriptionView : Device -> List (List (Element msg)) -> Element msg
descriptionView device paragraphs =
    case paragraphs of
        [] ->
            El.none

        _ ->
            El.column
                [ BackgroundColor.panelContent
                , El.padding 10
                , El.spacing 10
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



{- Element -}


elementView : Element msg -> Element msg
elementView el =
    if el == El.none then
        El.none

    else
        El.el
            [ BorderWidth.top 1
            , BorderColor.panel
            , El.width El.fill
            ]
            el



{- Attributes -}


panelAttrs : Device -> Maybe msg -> List (Attribute msg)
panelAttrs device maybeMsg =
    List.append
        (case maybeMsg of
            Nothing ->
                []

            Just _ ->
                [ El.pointer ]
        )
        [ roundedBorder device
        , BackgroundColor.panel
        , Border.width 1
        , BorderColor.panel
        , El.centerX
        , El.clip
        , El.mouseDown
            [ Shadow.panel device ]
        , El.mouseOver
            [ Shadow.panel device ]
        , El.height El.fill
        , El.width El.fill
        ]


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
