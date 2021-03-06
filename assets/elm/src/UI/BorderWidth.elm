module UI.BorderWidth exposing
    ( bottom
    , top
    )

import Element exposing (Attribute)
import Element.Border as Border



{- Types -}


type alias BorderEach =
    { left : Int
    , top : Int
    , right : Int
    , bottom : Int
    }



{- Helper -}


borderEach : BorderEach
borderEach =
    { left = 0
    , top = 0
    , right = 0
    , bottom = 0
    }



{- Build -}


bottom : Int -> Attribute msg
bottom amount =
    Border.widthEach
        { borderEach | bottom = amount }


top : Int -> Attribute msg
top amount =
    Border.widthEach
        { borderEach | top = amount }
