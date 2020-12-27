module Type.Room exposing
    ( Room
    , addOccupantTyping
    , clearMessage
    , decode
    , decodeList
    , dropOccupantTyping
    , encode
    , id
    , init
    , isOccupant
    , updateMembers
    , updateMessage
    , updateMessages
    )

import Json.Decode as JD exposing (Value)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE
import Type.ChatMessage as ChatMessage exposing (ChatMessage)
import Type.User as User exposing (RegisteredUser)


type alias Room =
    { id : String
    , owner : RegisteredUser
    , members : List RegisteredUser
    , message : String
    , messages : List ChatMessage
    , occupantsTyping : List RegisteredUser
    }


init : RegisteredUser -> Room
init owner =
    { id = ""
    , owner = owner
    , members = []
    , message = ""
    , messages = []
    , occupantsTyping = []
    }



{- Build -}


updateMembers : List RegisteredUser -> Room -> Room
updateMembers users room =
    { room | members = users }


updateMessage : String -> Room -> Room
updateMessage message room =
    { room | message = message }


updateMessages : List ChatMessage -> Room -> Room
updateMessages messages room =
    { room | messages = messages }


clearMessage : Room -> Room
clearMessage room =
    { room | message = "" }


addOccupantTyping : RegisteredUser -> RegisteredUser -> Room -> Room
addOccupantTyping currentUser user room =
    if not <| User.match currentUser user && (not <| List.member user room.occupantsTyping) then
        { room | occupantsTyping = user :: room.occupantsTyping }

    else
        room


dropOccupantTyping : RegisteredUser -> Room -> Room
dropOccupantTyping user room =
    { room | occupantsTyping = List.filter (\user_ -> not <| User.match user_ user) room.occupantsTyping }



{- Query -}


id : Room -> String
id room =
    room.id


isOccupant : RegisteredUser -> Room -> Bool
isOccupant user room =
    List.filter (\occupant -> User.match occupant user) room.members
        |> List.isEmpty
        |> not



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
        |> andMap (JD.succeed "")
        |> andMap (JD.field "messages" (JD.list ChatMessage.decoder))
        |> andMap (JD.succeed [])
