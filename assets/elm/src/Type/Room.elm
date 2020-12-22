module Type.Room exposing
    ( Room
    , decode
    , decodeList
    , encode
    , init
    , updateMembers
    )

import Json.Decode as JD exposing (Value)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE
import Type.ChatMessage as ChatMessage exposing (ChatMessage)
import Type.User as User exposing (User)


type alias Room =
    { id : String
    , owner : User
    , members : List User
    , messages : List ChatMessage
    }


init : Room
init =
    { id = ""
    , owner = User.init
    , members = []
    , messages = []
    }



{- Build -}


updateMembers : List User -> Room -> Room
updateMembers users room =
    { room | members = users }



{- Encode -}


encode : Room -> Value
encode room =
    JE.object
        [ ( "id", JE.string room.id ) ]



{- Decode -}


decode : Value -> Result JD.Error Room
decode payload =
    JD.decodeValue decoder payload


decodeList : Value -> Result JD.Error (List Room)
decodeList payload =
    JD.decodeValue (JD.field "rooms" (JD.list decoder)) payload


decoder : JD.Decoder Room
decoder =
    JD.succeed
        Room
        |> andMap (JD.field "id" JD.string)
        |> andMap (JD.field "owner" User.decoder)
        |> andMap (JD.field "members" (JD.list User.decoder))
        |> andMap (JD.field "messages" (JD.list ChatMessage.decoder))
