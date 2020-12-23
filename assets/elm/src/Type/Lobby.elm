module Type.Lobby exposing
    ( Lobby
    , init
    , inviteAccepted
    , inviteDeclined
    , inviteError
    , inviteExpired
    , inviteRevoked
    , occupantLeftRoom
    , occupants
    , presenceState
    , roomClosed
    , roomInvite
    , roomList
    , showOccupants
    )

import Json.Decode as JD exposing (Decoder, Value)
import Json.Decode.Extra exposing (andMap)
import Type.ErrorMessage exposing (ErrorMessage(..))
import Type.Presence exposing (Presence)
import Type.Room exposing (Room)
import Type.RoomInvite exposing (RoomInvite)
import Type.User as User exposing (User)


type alias Lobby =
    { user : User
    , presences : List Presence
    , rooms : List Room
    , showOccupants : Maybe Room
    , roomInvites : List RoomInvite
    , inviteError : Maybe ErrorMessage
    }


init : User -> List Presence -> Lobby
init user presences =
    { user = user
    , presences = presences
    , rooms = []
    , showOccupants = Nothing
    , roomInvites = []
    , inviteError = Nothing
    }


showOccupants : Maybe Room -> Lobby -> Lobby
showOccupants maybeRoom lobby =
    { lobby | showOccupants = maybeRoom }


occupants : Lobby -> List Presence
occupants { presences } =
    presences


roomList : List Room -> Lobby -> Lobby
roomList rooms lobby =
    { lobby | rooms = rooms }


roomClosed : Room -> Lobby -> Lobby
roomClosed room lobby =
    { lobby | roomInvites = List.filter (\invite_ -> invite_.roomId /= room.id) lobby.roomInvites }


roomInvite : RoomInvite -> Lobby -> Lobby
roomInvite invite lobby =
    if invite.toId == lobby.user.id then
        { lobby | roomInvites = List.append lobby.roomInvites [ invite ] }

    else
        lobby


inviteAccepted : RoomInvite -> Lobby -> Lobby
inviteAccepted invite lobby =
    if invite.toId == lobby.user.id then
        { lobby | roomInvites = List.filter (\invite_ -> invite_ /= invite) lobby.roomInvites }

    else
        lobby


inviteDeclined : RoomInvite -> Lobby -> Lobby
inviteDeclined invite lobby =
    if invite.toId == lobby.user.id then
        { lobby | roomInvites = List.filter (\invite_ -> invite_ /= invite) lobby.roomInvites }

    else
        lobby


inviteExpired : RoomInvite -> Lobby -> Lobby
inviteExpired invite lobby =
    if invite.toId == lobby.user.id then
        { lobby
            | roomInvites = List.filter (\invite_ -> invite_ /= invite) lobby.roomInvites
            , inviteError = Just (RoomClosed invite)
        }

    else
        lobby


inviteRevoked : RoomInvite -> Lobby -> Lobby
inviteRevoked invite lobby =
    if invite.toId == lobby.user.id then
        { lobby | roomInvites = List.filter (\invite_ -> invite_ /= invite) lobby.roomInvites }

    else
        lobby


inviteError : Maybe ErrorMessage -> Lobby -> Lobby
inviteError maybeError lobby =
    { lobby | inviteError = maybeError }


occupantLeftRoom : Value -> Lobby -> Lobby
occupantLeftRoom payload lobby =
    case decodeOccupant payload of
        Ok occupant ->
            { lobby | roomInvites = List.filter (\invite_ -> invite_.from.id /= occupant.userId && invite_.roomId /= occupant.roomId) lobby.roomInvites }

        Err _ ->
            lobby


type alias Occupant =
    { userId : String
    , roomId : String
    }


decodeOccupant : Value -> Result JD.Error Occupant
decodeOccupant payload =
    JD.decodeValue occupantDecoder payload


occupantDecoder : JD.Decoder Occupant
occupantDecoder =
    JD.succeed
        Occupant
        |> andMap (JD.field "user_id" JD.string)
        |> andMap (JD.field "room_id" JD.string)


presenceState : List Presence -> Lobby -> Lobby
presenceState state model =
    { model | presences = state }
