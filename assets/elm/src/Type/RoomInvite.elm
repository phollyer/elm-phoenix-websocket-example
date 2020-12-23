module Type.RoomInvite exposing
    ( RoomInvite
    , decode
    , encode
    )

import Json.Decode as JD exposing (Value)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE
import Type.User as User exposing (User)


type alias RoomInvite =
    { from : User
    , toId : String
    , roomId : String
    }


encode : RoomInvite -> Value
encode invite =
    JE.object
        [ ( "from", User.encode invite.from )
        , ( "to_id", JE.string invite.toId )
        , ( "room_id", JE.string invite.roomId )
        ]


decode : Value -> Result JD.Error RoomInvite
decode payload =
    JD.decodeValue decoder payload


decoder : JD.Decoder RoomInvite
decoder =
    JD.succeed
        RoomInvite
        |> andMap (JD.field "from" User.decoder)
        |> andMap (JD.field "to_id" JD.string)
        |> andMap (JD.field "room_id" JD.string)
