module Type.Presence exposing
    ( Presence
    , decodeList
    )

import Json.Decode as JD exposing (Value)
import Json.Decode.Extra exposing (andMap)
import Phoenix
import Type.User as User exposing (User)


type alias Presence =
    { id : String
    , metas : List Meta
    , user : User
    }


type alias Meta =
    { online_at : String
    , device : String
    }



{- Decode -}


decode : Phoenix.Presence -> Presence
decode presence =
    { id = presence.id
    , metas = decodeMetas presence.metas
    , user =
        User.decode presence.user
            |> Result.toMaybe
            |> Maybe.withDefault User.init
    }


decodeList : List Phoenix.Presence -> List Presence
decodeList presences =
    List.map decode presences


decodeMetas : List Value -> List Meta
decodeMetas metas =
    List.map
        (\meta ->
            JD.decodeValue metaDecoder meta
                |> Result.toMaybe
                |> Maybe.withDefault (Meta "" "")
        )
        metas


metaDecoder : JD.Decoder Meta
metaDecoder =
    JD.succeed
        Meta
        |> andMap (JD.field "online_at" JD.string)
        |> andMap (JD.field "device" JD.string)
