module UI.Shadow exposing
    ( button
    , content
    , panel
    )

import Colors.Alpha as Alpha
import Colors.Opaque as Color
import Element exposing (Attribute, Decoration, Device, DeviceClass(..), Orientation(..))
import Element.Border as Border



{- Page -}


content : Device -> Attribute msg
content { class } =
    Border.shadow <|
        case class of
            Phone ->
                { size = 2
                , blur = 5
                , color = Color.lightblue
                , offset = ( 0, 0 )
                }

            Tablet ->
                { size = 3
                , blur = 10
                , color = Color.lightblue
                , offset = ( 0, 0 )
                }

            _ ->
                { size = 5
                , blur = 20
                , color = Color.lightblue
                , offset = ( 0, 0 )
                }



{- Panels -}


panel : Device -> Decoration
panel { class } =
    Border.shadow <|
        case class of
            Phone ->
                { size = 1
                , blur = 3
                , color = Color.steelblue
                , offset = ( 0, 0 )
                }

            Tablet ->
                { size = 1
                , blur = 4
                , color = Color.steelblue
                , offset = ( 0, 0 )
                }

            _ ->
                { size = 2
                , blur = 5
                , color = Color.steelblue
                , offset = ( 0, 0 )
                }



{- Buttons -}


button : Decoration
button =
    Border.shadow
        { size = 1
        , blur = 5
        , color = Alpha.darkslateblue 0.9
        , offset = ( 0, 0 )
        }
