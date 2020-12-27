module Type.Lobby exposing
    ( Lobby
    , init
    , occupants
    , occupantsState
    , roomList
    , selectedRoom
    )

import Json.Decode.Extra exposing (combine)
import List.Extra as List
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


occupantsState : RegisteredUser -> List RegisteredUser -> Lobby -> Lobby
occupantsState currentUser state lobby =
    let
        occupants_ =
            List.partition (User.match currentUser) state
                |> Tuple.mapSecond (List.sortWith byUsername)
                |> combine
    in
    { lobby
        | occupants = occupants_
        , inviteableUsers = inviteableUsers lobby.rooms occupants_
    }


combine : ( List a, List a ) -> List a
combine ( a, b ) =
    List.append a b


roomList : RegisteredUser -> List Room -> Lobby -> Lobby
roomList currentUser rooms lobby =
    let
        rooms_ =
            List.partition (\room -> User.match currentUser room.owner) rooms
                |> Tuple.mapSecond sortOtherRooms
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


allOccupants : ( List Room, List ( RegisteredUser, List Room ) ) -> List RegisteredUser
allOccupants ( ownRooms, othersRooms ) =
    List.map Tuple.second othersRooms
        |> List.concat
        |> List.append ownRooms
        |> List.map .members
        |> List.concat


isInviteable : ( List Room, List ( RegisteredUser, List Room ) ) -> RegisteredUser -> Bool
isInviteable allRooms user =
    List.filter (User.match user) (allOccupants allRooms)
        |> List.isEmpty



{- Sort -}


sortOtherRooms : List Room -> List ( RegisteredUser, List Room )
sortOtherRooms rooms_ =
    List.sortWith byOwner rooms_
        |> List.groupWhile roomOwnersMatch
        |> List.map
            (\( a, b ) ->
                ( a.owner
                , List.sortWith mostMembers (a :: b)
                )
            )


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


roomOwnersMatch : Room -> Room -> Bool
roomOwnersMatch roomA roomB =
    User.match roomA.owner roomB.owner
