module Type.User exposing
    ( User
    , decode
    , decoder
    , encode
    , init
    )

import Colors.Alpha as Color
import Element exposing (Color)
import Json.Decode as JD
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE exposing (Value)
import Type.Color as Color


type alias User =
    { id : String
    , username : String
    , backgroundColor : Color
    , foregroundColor : Color
    }


init : User
init =
    { id = ""
    , username = ""
    , backgroundColor = Color.black 1
    , foregroundColor = Color.white 1
    }



{- Encode -}


encode : User -> Value
encode user =
    JE.object
        [ ( "id", JE.string user.id )
        , ( "username", JE.string user.username )
        , ( "background_color", Color.encode user.backgroundColor )
        , ( "foreground_color", Color.encode user.foregroundColor )
        ]



{- Decode -}


decode : Value -> Result JD.Error User
decode payload =
    JD.decodeValue decoder payload


decoder : JD.Decoder User
decoder =
    JD.succeed
        User
        |> andMap (JD.field "id" JD.string)
        |> andMap (JD.field "username" JD.string)
        |> andMap
            (JD.field "background_color" Color.decoder
                |> JD.andThen Color.fromRgbaDecoder
            )
        |> andMap
            (JD.field "foreground_color" Color.decoder
                |> JD.andThen Color.fromRgbaDecoder
            )
