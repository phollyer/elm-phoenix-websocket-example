module Type.Color exposing
    ( decoder
    , encode
    )

import Element as El exposing (Color)
import Json.Decode as JD exposing (Value, andThen)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE


type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


encode : Color -> Value
encode color =
    let
        rgba =
            El.toRgb color
    in
    JE.object
        [ ( "red", JE.float rgba.red )
        , ( "green", JE.float rgba.green )
        , ( "blue", JE.float rgba.blue )
        , ( "alpha", JE.float rgba.alpha )
        ]


decoder : JD.Decoder Color
decoder =
    JD.succeed
        RGBA
        |> andMap (JD.field "red" JD.float)
        |> andMap (JD.field "green" JD.float)
        |> andMap (JD.field "blue" JD.float)
        |> andMap (JD.field "alpha" JD.float)
        |> andThen fromRgbaDecoder


fromRgbaDecoder : RGBA -> JD.Decoder Color
fromRgbaDecoder rgba =
    JD.succeed (El.fromRgb rgba)
