module UI.Align exposing
    ( X(..)
    , Y(..)
    , x
    , y
    )

import Element as El exposing (Attribute)


type X
    = Center
    | Left
    | Right


type Y
    = Middle
    | Top
    | Bottom


x : X -> Attribute msg
x align =
    case align of
        Center ->
            El.centerX

        Left ->
            El.alignLeft

        Right ->
            El.alignRight


y : Y -> Attribute msg
y align =
    case align of
        Middle ->
            El.centerY

        Top ->
            El.alignTop

        Bottom ->
            El.alignBottom
