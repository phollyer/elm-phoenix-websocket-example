module Type.Lobby exposing
    ( Lobby
    , init
    , occupants
    , occupantsState
    , roomList
    , selectedRoom
    )

import Type.Room as Room exposing (Room)
import Type.User as User exposing (RegisteredUser)



{- Type -}


type alias Lobby =
    { occupants : List RegisteredUser
    , inviteableUsers : List RegisteredUser
    , rooms : ( List Room, List ( RegisteredUser, List Room ) )
    , selectedRoom : Maybe Room
    }



{- Build -}


init : Lobby
init =
    { occupants = []
    , inviteableUsers = []
    , rooms = ( [], [] )
    , selectedRoom = Nothing
    }



{- Transform -}


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


selectedRoom : Maybe Room -> Lobby -> Lobby
selectedRoom maybeRoom lobby =
    { lobby | selectedRoom = maybeRoom }



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
