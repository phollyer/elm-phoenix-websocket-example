module Type.Room exposing
    ( Room
    , addOccupantTyping
    , clearMessage
    , decode
    , decodeList
    , dropOccupantTyping
    , encode
    , groupByOwnerWith
    , init
    , isOccupant
    , isOpen
    , mostMembers
    , partition
    , updateMembers
    , updateMessage
    , updateMessages
    )

import Json.Decode as JD exposing (Value)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE
import List.Extra as List
import Type.ChatMessage as ChatMessage exposing (ChatMessage)
import Type.User as User exposing (RegisteredUser)



{- Type -}


type alias Room =
    { id : String
    , owner : RegisteredUser
    , members : List RegisteredUser
    , message : String
    , messages : List ChatMessage
    , occupantsTyping : List RegisteredUser
    }



{- Build -}


init : RegisteredUser -> Room
init owner =
    { id = ""
    , owner = owner
    , members = []
    , message = ""
    , messages = []
    , occupantsTyping = []
    }



{- Transform -}


updateMembers : List RegisteredUser -> Room -> Room
updateMembers users room =
    { room | members = User.sortWith User.byUsername users }


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
    if not <| User.match currentUser user && (not <| User.member user room.occupantsTyping) then
        { room | occupantsTyping = user :: room.occupantsTyping }

    else
        room


dropOccupantTyping : RegisteredUser -> Room -> Room
dropOccupantTyping user room =
    { room | occupantsTyping = User.drop user room.occupantsTyping }


partition : RegisteredUser -> List Room -> ( List Room, List Room )
partition currentUser rooms =
    List.partition (\room -> User.match currentUser room.owner) rooms


groupByOwnerWith : (Room -> Room -> Order) -> List Room -> List ( RegisteredUser, List Room )
groupByOwnerWith sortFunc rooms =
    List.sortWith byOwner rooms
        |> List.groupWhile matchOwner
        |> List.map (toOwnerWith sortFunc)


toOwnerWith : (Room -> Room -> Order) -> ( Room, List Room ) -> ( RegisteredUser, List Room )
toOwnerWith sortFunc ( room, rooms ) =
    ( room.owner, List.sortWith sortFunc (room :: rooms) )



{- Query -}


isOccupant : RegisteredUser -> Room -> Bool
isOccupant user room =
    User.member user room.members


isOpen : Room -> Bool
isOpen room =
    List.member room.owner room.members



{- Sorting -}


byOwner : Room -> Room -> Order
byOwner roomA roomB =
    byUsername roomA.owner roomB.owner


byUsername : RegisteredUser -> RegisteredUser -> Order
byUsername userA userB =
    case compare (User.username userA) (User.username userB) of
        LT ->
            LT

        EQ ->
            EQ

        GT ->
            GT


mostMembers : Room -> Room -> Order
mostMembers roomA roomB =
    case compare (List.length roomA.members) (List.length roomB.members) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT



{- Predicates -}


matchOwner : Room -> Room -> Bool
matchOwner roomA roomB =
    User.match roomA.owner roomB.owner



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
