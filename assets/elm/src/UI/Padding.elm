module UI.Padding exposing
    ( bottom
    , left
    , right
    , top
    , x
    , y
    )

import Element as El exposing (Attribute)



{- Types -}


type alias PaddingEach =
    { left : Int
    , top : Int
    , right : Int
    , bottom : Int
    }



{- Helpers -}


paddingEach : PaddingEach
paddingEach =
    { left = 0
    , top = 0
    , right = 0
    , bottom = 0
    }



{- Build -}


bottom : Int -> Attribute msg
bottom amount =
    El.paddingEach
        { paddingEach | bottom = amount }


left : Int -> Attribute msg
left amount =
    El.paddingEach
        { paddingEach | left = amount }


top : Int -> Attribute msg
top amount =
    El.paddingEach
        { paddingEach | top = amount }


right : Int -> Attribute msg
right amount =
    El.paddingEach
        { paddingEach | right = amount }


x : Int -> Attribute msg
x amount =
    El.paddingXY amount 0


y : Int -> Attribute msg
y amount =
    El.paddingXY 0 amount
