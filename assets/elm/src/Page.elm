module Page exposing (view)

import Browser exposing (Document)
import Colors.Opaque as Color
import Element as El exposing (Attribute, Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font



{- View -}


view : String -> Device -> { title : String, content : Element msg } -> Document msg
view vsn device { title, content } =
    { title = title ++ " - Elm Phoenix Websocket Example"
    , body =
        [ El.layout
            [ padding device
            , Background.color Color.aliceblue
            , El.height El.fill
            , El.width El.fill
            , El.inFront <|
                El.el
                    [ fontSize device
                    , El.alignBottom
                    , El.alignRight
                    , Font.color Color.deepskyblue
                    ]
                    (El.text ("v" ++ vsn))
            ]
          <|
            El.el
                [ roundedBorder device
                , shadow device
                , paddingXY device
                , Background.color Color.skyblue
                , El.height El.fill
                , El.width El.fill
                , El.clip
                ]
                content
        ]
    }



{- Attributes -}


fontSize : Device -> Attribute msg
fontSize { class } =
    case class of
        Phone ->
            Font.size 8

        Tablet ->
            Font.size 10

        _ ->
            Font.size 12


padding : Device -> Attribute msg
padding { class } =
    case class of
        Phone ->
            El.padding 10

        Tablet ->
            El.padding 20

        _ ->
            El.padding 30


paddingXY : Device -> Attribute msg
paddingXY { class } =
    case class of
        Phone ->
            El.paddingXY 10 0

        Tablet ->
            El.paddingXY 20 0

        _ ->
            El.paddingXY 30 0


roundedBorder : Device -> Attribute msg
roundedBorder { class } =
    case class of
        Phone ->
            Border.rounded 10

        Tablet ->
            Border.rounded 20

        _ ->
            Border.rounded 30


shadow : Device -> Attribute msg
shadow { class } =
    case class of
        Phone ->
            Border.shadow
                { size = 2
                , blur = 5
                , color = Color.lightblue
                , offset = ( 0, 0 )
                }

        Tablet ->
            Border.shadow
                { size = 3
                , blur = 10
                , color = Color.lightblue
                , offset = ( 0, 0 )
                }

        _ ->
            Border.shadow
                { size = 5
                , blur = 20
                , color = Color.lightblue
                , offset = ( 0, 0 )
                }
