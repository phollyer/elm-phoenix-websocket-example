module View.Example exposing
    ( Config
    , controls
    , description
    , feedback
    , id
    , init
    , view
    )

import Colors.Opaque as Color
import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Font as Font
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import UI.FontSize as FontSize
import UI.Padding as Padding



{- Model -}


type Config msg
    = Config
        { id : Maybe String
        , description : List (List (Element msg))
        , controls : Element msg
        , feedback : Element msg
        }


init : Config msg
init =
    Config
        { id = Nothing
        , description = []
        , controls = El.none
        , feedback = El.none
        }


controls : Element msg -> Config msg -> Config msg
controls cntrls (Config config) =
    Config { config | controls = cntrls }


description : List (List (Element msg)) -> Config msg -> Config msg
description desc (Config config) =
    Config { config | description = desc }


feedback : Element msg -> Config msg -> Config msg
feedback feedback_ (Config config) =
    Config { config | feedback = feedback_ }


id : Maybe String -> Config msg -> Config msg
id maybeId_ (Config config) =
    Config { config | id = maybeId_ }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.column
        [ El.spacing 10
        , El.height El.fill
        , El.width El.fill
        , FontColor.default
        , FontFamily.default
        , FontSize.default device
        , Padding.bottom 10
        ]
        [ descriptionView config.description
        , maybeId config.id
        , El.el
            [ El.width El.fill ]
            config.controls
        , config.feedback
        ]



{- Description -}


descriptionView : List (List (Element msg)) -> Element msg
descriptionView paragraphs =
    El.column
        [ El.width El.fill
        , Font.justify
        ]
    <|
        List.map
            (\paragraph ->
                El.paragraph
                    [ El.width El.fill ]
                    paragraph
            )
            paragraphs



{- Example ID -}


maybeId : Maybe String -> Element msg
maybeId maybeId_ =
    case maybeId_ of
        Nothing ->
            El.none

        Just id_ ->
            El.paragraph
                [ Font.center ]
                [ El.el [ FontColor.label ] (El.text "Example ID: ")
                , El.el [ FontColor.value ] (El.text id_)
                ]
