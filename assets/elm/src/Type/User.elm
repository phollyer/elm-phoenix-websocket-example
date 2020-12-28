module Type.User exposing
    ( InviteState(..)
    , RegisteredUser
    , RoomInvite
    , UnregisteredUser
    , User(..)
    , bgColor
    , bgColorSelected
    , byUsername
    , createInvite
    , currentUserFirst
    , decode
    , decodePresenceState
    , decodeRoomInvite
    , decoder
    , deleteReceivedInvite
    , drop
    , dropInviteForRoom
    , encode
    , encodeFields
    , encodeRoomInvite
    , fgColor
    , fgColorSelected
    , findInviteTo
    , hasInvites
    , init
    , invitesReceived
    , leftRoom
    , match
    , member
    , processErrors
    , roomClosed
    , sortWith
    , updateReceivedInvites
    , updateSentInvites
    , username
    , usernameChanged
    , validate
    )

import Colors.Alpha as Color
import Dict exposing (Dict)
import Element exposing (Color)
import Json.Decode as JD exposing (andThen, fail, field, string, succeed)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE exposing (Value)
import Phoenix
import Type.Color as Color
import Type.ErrorMessage exposing (ErrorMessage(..))
import Type.TwoTrack exposing (TwoTrack(..), bind)



{- Types -}


type User
    = Unregistered UnregisteredUser
    | Registered RegisteredUser


type alias UnregisteredUser =
    { username : String
    , usernameError : Maybe ErrorMessage
    , backgroundColor : Maybe Color
    , backgroundColorError : Maybe ErrorMessage
    , foregroundColor : Maybe Color
    , foregroundColorError : Maybe ErrorMessage
    }


type RegisteredUser
    = RegisteredUser
        { id : ID
        , username : String
        , backgroundColor : Color
        , foregroundColor : Color
        , sentInvites : Dict String ( InviteState, RoomInvite )
        , receivedInvites : Dict String ( InviteState, RoomInvite )
        }


type ID
    = ID String


type alias RoomInvite =
    { id : String
    , from : RegisteredUser
    , to : RegisteredUser
    , roomId : String
    }


type InviteState
    = Accepting
    | Accepted
    | Declining
    | Declined
    | Expired
    | Inviting
    | Invited
    | Revoking
    | Revoked



{- Build -}


init : UnregisteredUser
init =
    { username = ""
    , usernameError = Nothing
    , backgroundColor = Nothing
    , backgroundColorError = Nothing
    , foregroundColor = Nothing
    , foregroundColorError = Nothing
    }


createInvite : RegisteredUser -> String -> RegisteredUser -> RoomInvite
createInvite toUser roomId fromUser =
    { id = createInviteId toUser roomId fromUser
    , from = fromUser
    , to = toUser
    , roomId = roomId
    }


createInviteId : RegisteredUser -> String -> RegisteredUser -> String
createInviteId (RegisteredUser toUser) roomId (RegisteredUser fromUser) =
    (toUser.id |> id) ++ ":" ++ roomId ++ ":" ++ (fromUser.id |> id)


deleteReceivedInvite : RoomInvite -> RegisteredUser -> RegisteredUser
deleteReceivedInvite invite (RegisteredUser user) =
    let
        key =
            createInviteId invite.to invite.roomId invite.from
    in
    RegisteredUser
        { user | receivedInvites = Dict.remove key user.receivedInvites }


deleteInvitesForRoom : String -> Dict String ( InviteState, RoomInvite ) -> Dict String ( InviteState, RoomInvite )
deleteInvitesForRoom roomId invites =
    Dict.foldl
        (\key val dict ->
            case String.split ":" key of
                [ _, roomId_, _ ] ->
                    if roomId_ == roomId then
                        Dict.remove key dict

                    else
                        dict

                _ ->
                    dict
        )
        invites
        invites


deleteInviteFromRoom : RegisteredUser -> String -> Dict String ( InviteState, RoomInvite ) -> Dict String ( InviteState, RoomInvite )
deleteInviteFromRoom (RegisteredUser fromUser) roomId invites =
    Dict.foldl
        (\key val dict ->
            case String.split ":" key of
                [ _, roomId_, fromId ] ->
                    if roomId_ == roomId && fromId == (fromUser.id |> id) then
                        Dict.remove key dict

                    else
                        dict

                _ ->
                    dict
        )
        invites
        invites


updateReceivedInvites : ( InviteState, RoomInvite ) -> RegisteredUser -> RegisteredUser
updateReceivedInvites ( state, invite ) (RegisteredUser user) =
    let
        key =
            createInviteId invite.to invite.roomId invite.from
    in
    case state of
        Accepted ->
            RegisteredUser
                { user | receivedInvites = Dict.remove key user.receivedInvites }

        Declined ->
            RegisteredUser
                { user | receivedInvites = Dict.remove key user.receivedInvites }

        Revoked ->
            RegisteredUser
                { user | receivedInvites = Dict.remove key user.receivedInvites }

        _ ->
            RegisteredUser
                { user | receivedInvites = Dict.insert key ( state, invite ) user.receivedInvites }


updateSentInvites : ( InviteState, RoomInvite ) -> RegisteredUser -> RegisteredUser
updateSentInvites ( state, invite ) (RegisteredUser user) =
    let
        key =
            createInviteId invite.to invite.roomId invite.from
    in
    case state of
        Accepted ->
            RegisteredUser
                { user | sentInvites = Dict.remove key user.sentInvites }

        Declined ->
            RegisteredUser
                { user | sentInvites = Dict.remove key user.sentInvites }

        Revoked ->
            RegisteredUser
                { user | sentInvites = Dict.remove key user.sentInvites }

        _ ->
            RegisteredUser
                { user | sentInvites = Dict.insert key ( state, invite ) user.sentInvites }



{- Validations -}


type Field
    = Username String
    | BackgroundColor Color
    | ForegroundColor Color


validate : UnregisteredUser -> TwoTrack Field
validate user =
    Success []
        |> bind switch validateUsername user.username
        |> bind switch validateBackgroundColor user.backgroundColor
        |> bind switch validateForegroundColor user.foregroundColor


switch : Result ErrorMessage Field -> TwoTrack Field
switch result =
    case result of
        Ok field ->
            Success [ field ]

        Err error ->
            Failure [ error ]


validateUsername : String -> Result ErrorMessage Field
validateUsername uname =
    if String.trim uname == "" then
        Err UsernameCannotBeBlank

    else if usernamePartMoreThan12Chars uname then
        Err UsernamePartsCannotBeMoreThan12Chars

    else if String.length uname > 25 then
        Err UsernameCannotBeMoreThan25Chars

    else
        Ok (Username (String.trim uname))


usernamePartMoreThan12Chars : String -> Bool
usernamePartMoreThan12Chars uname =
    String.words uname
        |> List.filter (\word -> String.length word > 12)
        |> List.isEmpty
        |> not


validateBackgroundColor : Maybe Color -> Result ErrorMessage Field
validateBackgroundColor maybeColor =
    case maybeColor of
        Nothing ->
            Err BackgroundColorNotSelected

        Just color ->
            Ok (BackgroundColor color)


validateForegroundColor : Maybe Color -> Result ErrorMessage Field
validateForegroundColor maybeColor =
    case maybeColor of
        Nothing ->
            Err ForegroundColorNotSelected

        Just color ->
            Ok (ForegroundColor color)



{- Errors -}


processErrors : List ErrorMessage -> UnregisteredUser -> UnregisteredUser
processErrors errors user =
    List.foldl
        (\error u ->
            case error of
                UsernameCannotBeBlank ->
                    { u | usernameError = Just UsernameCannotBeBlank }

                UsernamePartsCannotBeMoreThan12Chars ->
                    { u | usernameError = Just UsernamePartsCannotBeMoreThan12Chars }

                UsernameCannotBeMoreThan25Chars ->
                    { u | usernameError = Just UsernameCannotBeMoreThan25Chars }

                BackgroundColorNotSelected ->
                    { u | backgroundColorError = Just BackgroundColorNotSelected }

                ForegroundColorNotSelected ->
                    { u | foregroundColorError = Just ForegroundColorNotSelected }

                _ ->
                    u
        )
        { user
            | usernameError = Nothing
            , backgroundColorError = Nothing
            , foregroundColorError = Nothing
        }
        errors



{- Transform -}


usernameChanged : String -> UnregisteredUser -> UnregisteredUser
usernameChanged name user =
    { user
        | username = name
        , usernameError =
            if String.trim name == "" then
                user.usernameError

            else
                Nothing
    }


bgColorSelected : Color -> UnregisteredUser -> UnregisteredUser
bgColorSelected color user =
    { user
        | backgroundColor = Just color
        , backgroundColorError = Nothing
    }


fgColorSelected : Color -> UnregisteredUser -> UnregisteredUser
fgColorSelected color user =
    { user
        | foregroundColor = Just color
        , foregroundColorError = Nothing
    }


currentUserFirst : RegisteredUser -> List RegisteredUser -> List RegisteredUser
currentUserFirst currentUser allUsers =
    List.partition (match currentUser) allUsers
        |> combine


combine : ( List a, List a ) -> List a
combine ( a, b ) =
    List.append a b


dropInviteForRoom : String -> RegisteredUser -> RegisteredUser
dropInviteForRoom roomId (RegisteredUser user) =
    RegisteredUser
        { user
            | receivedInvites = deleteInvitesForRoom roomId user.receivedInvites
        }


roomClosed : String -> RegisteredUser -> RegisteredUser
roomClosed roomId (RegisteredUser user) =
    RegisteredUser
        { user
            | receivedInvites = deleteInvitesForRoom roomId user.receivedInvites
            , sentInvites = deleteInvitesForRoom roomId user.sentInvites
        }


leftRoom : Value -> RegisteredUser -> RegisteredUser
leftRoom payload (RegisteredUser user) =
    case decodeTempRoom payload of
        Ok { occupant, roomId } ->
            RegisteredUser
                { user | receivedInvites = deleteInviteFromRoom occupant roomId user.receivedInvites }

        Err _ ->
            RegisteredUser user


drop : RegisteredUser -> List RegisteredUser -> List RegisteredUser
drop user users =
    List.partition (match user) users
        |> Tuple.second



{- Query -}


id : ID -> String
id (ID id_) =
    id_


username : RegisteredUser -> String
username (RegisteredUser user) =
    user.username


bgColor : RegisteredUser -> Color
bgColor (RegisteredUser { backgroundColor }) =
    backgroundColor


fgColor : RegisteredUser -> Color
fgColor (RegisteredUser { foregroundColor }) =
    foregroundColor


findInviteTo : RegisteredUser -> String -> RegisteredUser -> Maybe ( InviteState, RoomInvite )
findInviteTo (RegisteredUser toUser) roomId (RegisteredUser fromUser) =
    Dict.get ((toUser.id |> id) ++ ":" ++ roomId ++ ":" ++ (fromUser.id |> id)) fromUser.sentInvites


match : RegisteredUser -> RegisteredUser -> Bool
match (RegisteredUser userA) (RegisteredUser userB) =
    userA.id == userB.id


hasInvites : RegisteredUser -> Bool
hasInvites (RegisteredUser user) =
    not <| Dict.isEmpty user.receivedInvites


invitesReceived : RegisteredUser -> List ( InviteState, RoomInvite )
invitesReceived (RegisteredUser { receivedInvites }) =
    Dict.values receivedInvites


member : RegisteredUser -> List RegisteredUser -> Bool
member user users =
    List.partition (match user) users
        |> Tuple.first
        |> List.isEmpty
        |> not



{- Sorting -}


sortWith : (RegisteredUser -> RegisteredUser -> Order) -> List RegisteredUser -> List RegisteredUser
sortWith sortFunc =
    List.sortWith sortFunc


byUsername : RegisteredUser -> RegisteredUser -> Order
byUsername (RegisteredUser user1) (RegisteredUser user2) =
    case compare user1.username user2.username of
        LT ->
            LT

        EQ ->
            EQ

        GT ->
            GT



{- Encode -}


encode : RegisteredUser -> Value
encode (RegisteredUser user) =
    JE.object
        [ ( "id", JE.string (user.id |> id) )
        , ( "username", JE.string user.username )
        , ( "background_color", Color.encode user.backgroundColor )
        , ( "foreground_color", Color.encode user.foregroundColor )
        ]


encodeRoomInvite : RoomInvite -> Value
encodeRoomInvite invite =
    JE.object
        [ ( "id", JE.string invite.id )
        , ( "from", encode invite.from )
        , ( "to", encode invite.to )
        , ( "room_id", JE.string invite.roomId )
        ]


encodeFields : List Field -> Value
encodeFields fields =
    JE.object <|
        List.map encodeField fields


encodeField : Field -> ( String, Value )
encodeField field =
    case field of
        Username uname ->
            ( "username", JE.string uname )

        BackgroundColor color ->
            ( "background_color", Color.encode color )

        ForegroundColor color ->
            ( "foreground_color", Color.encode color )



{- Decode -}


type alias TempUser =
    { id : ID
    , username : String
    , backgroundColor : Color
    , foregroundColor : Color
    , sentInvites : Dict String ( InviteState, RoomInvite )
    , receivedInvites : Dict String ( InviteState, RoomInvite )
    }


decode : Value -> Result JD.Error RegisteredUser
decode payload =
    JD.decodeValue decoder payload


decodePresenceState : List Phoenix.Presence -> List RegisteredUser
decodePresenceState state =
    List.filterMap
        (\{ user } ->
            case decode user of
                Ok u ->
                    Just u

                Err _ ->
                    Nothing
        )
        state


decoder : JD.Decoder RegisteredUser
decoder =
    succeed
        TempUser
        |> andMap
            (field "id" string
                |> andThen toID
            )
        |> andMap (field "username" string)
        |> andMap (field "background_color" Color.decoder)
        |> andMap (field "foreground_color" Color.decoder)
        |> andMap (succeed Dict.empty)
        |> andMap (succeed Dict.empty)
        |> andThen (\user -> succeed (RegisteredUser user))


toID : String -> JD.Decoder ID
toID id_ =
    if String.isEmpty id_ then
        fail "User ID cannot be empty."

    else
        succeed (ID id_)


decodeRoomInvite : Value -> Result JD.Error RoomInvite
decodeRoomInvite payload =
    JD.decodeValue tempDecoder payload


tempDecoder : JD.Decoder RoomInvite
tempDecoder =
    succeed
        RoomInvite
        |> andMap (field "id" string)
        |> andMap (field "from" decoder)
        |> andMap (field "to" decoder)
        |> andMap (field "room_id" string)


type alias TempRoom =
    { occupant : RegisteredUser
    , roomId : String
    }


decodeTempRoom : Value -> Result JD.Error TempRoom
decodeTempRoom payload =
    JD.decodeValue tempRoomDecoder payload


tempRoomDecoder : JD.Decoder TempRoom
tempRoomDecoder =
    JD.succeed
        TempRoom
        |> andMap (JD.field "occupant" decoder)
        |> andMap (JD.field "room_id" JD.string)
