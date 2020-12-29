module Type.Lobby exposing
    ( Lobby
    , RoomAction(..)
    , init
    , inspectingRoom
    , occupants
    , occupantsState
    , roomList
    , updateRoomAction
    )

import Type.Room as Room exposing (Room)
import Type.User as User exposing (RegisteredUser)



{- Type -}


type alias Lobby =
    { occupants : List RegisteredUser
    , inviteableUsers : List RegisteredUser
    , rooms : ( List Room, List ( RegisteredUser, List Room ) )
    , roomAction : RoomAction
    , inspectingRoom : Maybe Room
    }


type RoomAction
    = NoAction
    | Inspecting (Maybe Room)
    | Entering Room
    | Deleting Room



{- Build -}


init : Lobby
init =
    { occupants = []
    , inviteableUsers = []
    , rooms = ( [], [] )
    , roomAction = NoAction
    , inspectingRoom = Nothing
    }


updateRoomAction : RoomAction -> Lobby -> Lobby
updateRoomAction roomAction lobby =
    case ( lobby.roomAction, roomAction ) of
        ( Entering _, Inspecting _ ) ->
            lobby

        ( Deleting _, Inspecting _ ) ->
            lobby

        _ ->
            { lobby | roomAction = roomAction }


occupantsState : RegisteredUser -> List RegisteredUser -> Lobby -> Lobby
occupantsState currentUser allUsers lobby =
    let
        occupants_ =
            User.sortWith User.byUsername allUsers
                |> User.currentUserFirst currentUser
    in
    { lobby
        | occupants = occupants_
        , inviteableUsers = inviteableUsers lobby.rooms occupants_
    }


roomList : RegisteredUser -> List Room -> Lobby -> Lobby
roomList currentUser rooms lobby =
    let
        rooms_ =
            Room.partition currentUser rooms
                |> Tuple.mapSecond (Room.groupByOwnerWith Room.mostMembers)
    in
    { lobby
        | rooms = rooms_
        , inviteableUsers = inviteableUsers rooms_ lobby.occupants
    }


inspectingRoom : Maybe Room -> Lobby -> Lobby
inspectingRoom maybeRoom lobby =
    { lobby | inspectingRoom = maybeRoom }



{- Query -}


occupants : Lobby -> List RegisteredUser
occupants lobby =
    lobby.occupants


inviteableUsers : ( List Room, List ( RegisteredUser, List Room ) ) -> List RegisteredUser -> List RegisteredUser
inviteableUsers allRooms users =
    List.filter (isInviteable allRooms) users


isInviteable : ( List Room, List ( RegisteredUser, List Room ) ) -> RegisteredUser -> Bool
isInviteable allRooms user =
    List.filter (User.match user) (allOccupants allRooms)
        |> List.isEmpty


allOccupants : ( List Room, List ( RegisteredUser, List Room ) ) -> List RegisteredUser
allOccupants ( ownRooms, othersRooms ) =
    List.map Tuple.second othersRooms
        |> List.concat
        |> List.append ownRooms
        |> List.map .members
        |> List.concat
