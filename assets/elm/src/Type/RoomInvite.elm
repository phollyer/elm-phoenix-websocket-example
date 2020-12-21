module Type.RoomInvite exposing
    ( RoomInvite
    , decode
    )

import Json.Decode as JD exposing (Value)
import Json.Decode.Extra exposing (andMap)
import Type.User as User exposing (User)


type alias RoomInvite =
    { from : User
    , to_id : String
    , room_id : String
    }


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
