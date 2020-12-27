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
        , inviteableUsers = isNotInARoom lobby.rooms occupants_
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
        , inviteableUsers = isNotInARoom rooms_ lobby.occupants
    }


selectedRoom : Maybe Room -> Lobby -> Lobby
selectedRoom maybeRoom lobby =
    { lobby | selectedRoom = maybeRoom }



{- Query -}


occupants : Lobby -> List RegisteredUser
occupants lobby =
    lobby.occupants



{- Sort -}


sortOtherRooms : List Room -> List ( RegisteredUser, List Room )
sortOtherRooms rooms_ =
    List.sortWith byOwner rooms_
        |> List.groupWhile (\roomA roomB -> roomA.owner == roomB.owner)
        |> List.map
            (\( a, b ) ->
                ( a.owner
                , List.sortWith mostMembers (a :: b)
                )
            )


byUsername : RegisteredUser -> RegisteredUser -> Order
byUsername userA userB =
    case compare (User.username userA) (User.username userB) of
        LT ->
            LT

        EQ ->
            EQ

        GT ->
            GT


byOwner : Room -> Room -> Order
byOwner roomA roomB =
    byUsername roomA.owner roomB.owner


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


isNotInARoom : ( List Room, List ( RegisteredUser, List Room ) ) -> List RegisteredUser -> List RegisteredUser
isNotInARoom allRooms users =
    List.filter
        (\user ->
            List.filter
                (\occupant -> User.match user occupant)
                (allOccupants allRooms)
                |> List.isEmpty
        )
        users


allOccupants : ( List Room, List ( RegisteredUser, List Room ) ) -> List RegisteredUser
allOccupants ( ownRooms, othersRooms ) =
    List.map Tuple.second othersRooms
        |> List.concat
        |> List.append ownRooms
        |> List.map (\room -> room.members)
        |> List.concat
