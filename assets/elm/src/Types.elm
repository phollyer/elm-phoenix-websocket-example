module Types exposing
    ( Message
    , Meta
    , Presence
    , Room
    , User
    , decodeMessage
    , decodeMessages
    , decodeMetas
    , decodeRoom
    , decodeRooms
    , decodeUser
    , initRoom
    , initUser
    , messageDecoder
    , userDecoder
    )

import Colors.Alpha as Color
import Element as El exposing (Color)
import Json.Decode as JD exposing (Value)
import Json.Decode.Extra exposing (andMap)


type alias Room =
    { id : String
    , owner : User
    , members : List User
    , messages : List Message
    }


initRoom : Room
initRoom =
    { id = ""
    , owner = initUser
    , members = []
    , messages = []
    }


type alias User =
    { id : String
    , username : String
    , backgroundColor : Color
    , foregroundColor : Color
    }


initUser : User
initUser =
    { id = ""
    , username = ""
    , backgroundColor = Color.black 1
    , foregroundColor = Color.white 1
    }


type alias Message =
    { text : String
    , owner : User
    , createdAt : Int
    }


type alias Presence =
    { id : String
    , metas : List Meta
    , user : User
    }


type alias Meta =
    { online_at : String
    , device : String
    }



{- Decoders -}


decodeMetas : List Value -> List Meta
decodeMetas metas =
    List.map
        (\meta ->
            JD.decodeValue metaDecoder meta
                |> Result.toMaybe
                |> Maybe.withDefault (Meta "" "")
        )
        metas


metaDecoder : JD.Decoder Meta
metaDecoder =
    JD.succeed
        Meta
        |> andMap (JD.field "online_at" JD.string)
        |> andMap (JD.field "device" JD.string)



{- Messages -}


decodeMessages : Value -> Result JD.Error (List Message)
decodeMessages payload =
    JD.decodeValue (JD.field "messages" (JD.list messageDecoder)) payload


decodeMessage : Value -> Result JD.Error Message
decodeMessage payload =
    JD.decodeValue messageDecoder payload


messageDecoder : JD.Decoder Message
messageDecoder =
    JD.succeed
        Message
        |> andMap (JD.field "text" JD.string)
        |> andMap (JD.field "owner" userDecoder)
        |> andMap (JD.field "created_at" JD.int)



{- Room -}


decodeRooms : Value -> Result JD.Error (List Room)
decodeRooms payload =
    JD.decodeValue (JD.field "rooms" (JD.list roomDecoder)) payload


decodeRoom : Value -> Result JD.Error Room
decodeRoom payload =
    JD.decodeValue roomDecoder payload


roomDecoder : JD.Decoder Room
roomDecoder =
    JD.succeed
        Room
        |> andMap (JD.field "id" JD.string)
        |> andMap (JD.field "owner" userDecoder)
        |> andMap (JD.field "members" (JD.list userDecoder))
        |> andMap (JD.field "messages" (JD.list messageDecoder))



{- User -}


decodeUser : Value -> Result JD.Error User
decodeUser payload =
    JD.decodeValue userDecoder payload


userDecoder : JD.Decoder User
userDecoder =
    JD.succeed
        User
        |> andMap (JD.field "id" JD.string)
        |> andMap (JD.field "username" JD.string)
        |> andMap
            (JD.field "background_color" rgbaDecoder
                |> JD.andThen toColor
            )
        |> andMap
            (JD.field "foreground_color" rgbaDecoder
                |> JD.andThen toColor
            )


toColor : RGBA -> JD.Decoder Color
toColor rgba =
    JD.succeed (El.fromRgb rgba)


type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


rgbaDecoder : JD.Decoder RGBA
rgbaDecoder =
    JD.succeed
        RGBA
        |> andMap (JD.field "red" JD.float)
        |> andMap (JD.field "green" JD.float)
        |> andMap (JD.field "blue" JD.float)
        |> andMap (JD.field "alpha" JD.float)
