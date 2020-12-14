module Page exposing (view)

import Browser exposing (Document)
import Element as El exposing (Attribute, Device, DeviceClass(..), Element)
import Element.Border as Border
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import UI.FontSize as FontSize
import UI.Padding as Padding
import UI.Shadow as Shadow



{- View -}


view : String -> Device -> { title : String, content : Element msg } -> Document msg
view vsn device { title, content } =
    { title = title ++ " - Elm Phoenix Websocket Example"
    , body =
        [ El.layout
            [ padding device
            , BackgroundColor.page
            , El.height El.fill
            , El.width El.fill
            , El.inFront <|
                El.el
                    [ El.alignBottom
                    , El.alignRight
                    , FontColor.vsn
                    , FontSize.vsn device
                    ]
                    (El.text ("v" ++ vsn))
            ]
          <|
            El.el
                [ paddingX device
                , roundedBorder device
                , BackgroundColor.content
                , El.clip
                , El.height El.fill
                , El.width El.fill
                , Shadow.content device
                ]
                content
        ]
    }



{- Attributes -}


padding : Device -> Attribute msg
padding { class } =
    El.padding <|
        case class of
            Phone ->
                10

            Tablet ->
                20

            _ ->
                30


paddingX : Device -> Attribute msg
paddingX { class } =
    Padding.x <|
        case class of
            Phone ->
                10

            Tablet ->
                20

            _ ->
                30


roundedBorder : Device -> Attribute msg
roundedBorder { class } =
    Border.rounded <|
        case class of
            Phone ->
                10

            Tablet ->
                20

            _ ->
                30
