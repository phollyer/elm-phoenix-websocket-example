module UI.Shadow exposing
    ( button
    , openRoom
    , ownRoom
    )

import Colors.Opaque as Color
import Element exposing (Decoration)
import Element.Border as Border


button : Decoration
button =
    Border.shadow
        { size = 1
        , blur = 2
        , color = Color.seagreen
        , offset = ( 0, 0 )
        }



{- Lobby Room Listing -}


ownRoom : Decoration
ownRoom =
    Border.shadow
        { size = 1
        , blur = 5
        , color = Color.darkslateblue
        , offset = ( 0, 0 )
        }


openRoom : Decoration
openRoom =
    Border.shadow
        { size = 1
        , blur = 5
        , color = Color.darkgreen
        , offset = ( 0, 0 )
        }
