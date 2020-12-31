module UI.BorderWidth exposing
    ( bottom
    , left
    , right
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


left : Int -> Attribute msg
left amount =
    Border.widthEach
        { borderEach | left = amount }


right : Int -> Attribute msg
right amount =
    Border.widthEach
        { borderEach | right = amount }


top : Int -> Attribute msg
top amount =
    Border.widthEach
        { borderEach | top = amount }
