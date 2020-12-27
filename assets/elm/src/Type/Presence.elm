module Type.Presence exposing (Presence)

import Type.User exposing (RegisteredUser)


type alias Presence =
    { id : String
    , user : RegisteredUser
    }
