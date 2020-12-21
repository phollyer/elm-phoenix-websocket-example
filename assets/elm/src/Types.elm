module Types exposing
    ( ErrorMessage(..)
    , Message
    , Meta
    , Presence
    , Room
    , RoomInvitation
    , TwoTrack(..)
    , User
    , bind
    , decodeMessage
    , decodeMessages
    , decodeMetas
    , decodeRoom
    , decodeRoomInvitation
    , decodeRooms
    , decodeUser
    , encodeColor
    , encodeRoom
    , encodeUser
    , errorToString
    , initRoom
    , initUser
    , messageDecoder
    , toPresences
    , userDecoder
    )

import Colors.Alpha as Color
import Element as El exposing (Color)
import Json.Decode as JD exposing (Value)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE
import Phoenix



{- Errors -}


type ErrorMessage
    = UsernameCannotBeBlank
    | BackgroundColorNotSelected
    | ForegroundColorNotSelected


errorToString : ErrorMessage -> String
errorToString error =
    case error of
        BackgroundColorNotSelected ->
            "A Background Color must be seleceted"

        ForegroundColorNotSelected ->
            "A Foreground Color must be selected"

        UsernameCannotBeBlank ->
            "Username can not be empty"



{- Two Track -}


type TwoTrack entity
    = Success (List entity)
    | Failure (List ErrorMessage)


bind : (a -> TwoTrack entity) -> a -> TwoTrack entity -> TwoTrack entity
bind func input twoTrack =
    case ( func input, twoTrack ) of
        ( Failure e, Failure f ) ->
            Failure (List.append e f)

        ( Failure e, _ ) ->
            Failure e

        ( _, Failure f ) ->
            Failure f

        ( Success a, Success b ) ->
            Success (List.append a b)



{- Room -}


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


encodeRoom : Room -> Value
encodeRoom room =
    JE.object
        [ ( "id", JE.string room.id ) ]


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


encodeUser : User -> Value
encodeUser user =
    JE.object
        [ ( "id", JE.string user.id )
        , ( "username", JE.string user.username )
        , ( "background_color", encodeColor user.backgroundColor )
        , ( "foreground_color", encodeColor user.foregroundColor )
        ]


encodeColor : Color -> Value
encodeColor color =
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



{- Message -}


type alias Message =
    { text : String
    , owner : User
    , createdAt : Int
    }


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



{- Presence -}


type alias Presence =
    { id : String
    , metas : List Meta
    , user : User
    }


toPresences : List Phoenix.Presence -> List Presence
toPresences presences =
    List.map
        (\presence ->
            { id = presence.id
            , metas = decodeMetas presence.metas
            , user =
                decodeUser presence.user
                    |> Result.toMaybe
                    |> Maybe.withDefault initUser
            }
        )
        presences


type alias Meta =
    { online_at : String
    , device : String
    }


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



{- Room Invitation -}


type alias RoomInvitation =
    { from : User
    , to_id : String
    , room_id : String
    }


decodeRoomInvitation : Value -> Result JD.Error RoomInvitation
decodeRoomInvitation payload =
    JD.decodeValue roomInvitationDecoder payload


roomInvitationDecoder : JD.Decoder RoomInvitation
roomInvitationDecoder =
    JD.succeed
        RoomInvitation
        |> andMap (JD.field "from" userDecoder)
        |> andMap (JD.field "to" JD.string)
        |> andMap (JD.field "room" JD.string)



{- Color -}


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
