module Type.Lobby exposing
    ( Lobby
    , init
    , occupants
    , presenceState
    , roomList
    , showOccupants
    )

import Type.ErrorMessage exposing (ErrorMessage(..))
import Type.Room exposing (Room)
import Type.User exposing (RegisteredUser)


type alias Lobby =
    { presences : List RegisteredUser
    , rooms : List Room
    , showOccupants : Maybe Room
    }


init : List RegisteredUser -> Lobby
init presences =
    { presences = presences
    , rooms = []
    , showOccupants = Nothing
    }


showOccupants : Maybe Room -> Lobby -> Lobby
showOccupants maybeRoom lobby =
    { lobby | showOccupants = maybeRoom }


occupants : Lobby -> List RegisteredUser
occupants { presences } =
    presences


presenceState : List RegisteredUser -> Lobby -> Lobby
presenceState state model =
    { model | presences = state }


roomList : List Room -> Lobby -> Lobby
roomList rooms lobby =
    { lobby | rooms = rooms }
