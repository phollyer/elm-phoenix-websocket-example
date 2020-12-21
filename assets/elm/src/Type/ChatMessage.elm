module Type.ChatMessage exposing
    ( ChatMessage
    , decode
    , decodeList
    , decoder
    )

import Json.Decode as JD exposing (Value)
import Json.Decode.Extra exposing (andMap)
import Type.User as User exposing (User)


type alias ChatMessage =
    { text : String
    , owner : User
    , createdAt : Int
    }


decode : Value -> Result JD.Error ChatMessage
decode payload =
    JD.decodeValue decoder payload


decodeList : Value -> Result JD.Error (List ChatMessage)
decodeList payload =
    JD.decodeValue (JD.field "messages" (JD.list decoder)) payload


decoder : JD.Decoder ChatMessage
decoder =
    JD.succeed
        ChatMessage
        |> andMap (JD.field "text" JD.string)
        |> andMap (JD.field "owner" User.decoder)
        |> andMap (JD.field "created_at" JD.int)
