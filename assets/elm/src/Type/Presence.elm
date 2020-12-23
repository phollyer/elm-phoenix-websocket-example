module Type.Presence exposing
    ( Presence
    , decodeState
    )

import Json.Decode as JD exposing (Value)
import Json.Decode.Extra exposing (andMap)
import Phoenix
import Type.User as User exposing (User)


type alias Presence =
    { id : String
    , user : User
    }



{- Decode -}


decode : Phoenix.Presence -> Presence
decode presence =
    { id = presence.id
    , user =
        User.decode presence.user
            |> Result.toMaybe
            |> Maybe.withDefault User.init
    }


decodeState : List Phoenix.Presence -> List Presence
decodeState presences =
    List.map decode presences
