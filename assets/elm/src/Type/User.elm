module Type.User exposing
    ( RegisteredUser
    , RoomInvite
    , UnregisteredUser
    , User(..)
    , bgColor
    , bgColorSelected
    , cancelInviteError
    , decode
    , decodePresenceState
    , decodeRoomInvite
    , decoder
    , dropInviteReceived
    , dropInviteSent
    , encode
    , encodeFields
    , encodeRoomInvite
    , fgColor
    , fgColorSelected
    , init
    , inviteError
    , inviteExpired
    , inviteReceieved
    , inviteSent
    , invitesReceived
    , invitesSent
    , isInvited
    , leftRoom
    , match
    , processErrors
    , roomClosed
    , sortByUsername
    , username
    , usernameChanged
    , validate
    )

import Colors.Alpha as Color
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
        , sentInvites : List RoomInvite
        , receivedInvites : List RoomInvite
        , inviteError : Maybe ErrorMessage
        }


type ID
    = ID String


type alias RoomInvite =
    { from : RegisteredUser
    , to : RegisteredUser
    , roomId : String
    }



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
usernameChanged name registration =
    { registration
        | username = name
        , usernameError =
            if String.trim name == "" then
                registration.usernameError

            else
                Nothing
    }


bgColorSelected : Color -> UnregisteredUser -> UnregisteredUser
bgColorSelected color registration =
    { registration
        | backgroundColor = Just color
        , backgroundColorError = Nothing
    }


fgColorSelected : Color -> UnregisteredUser -> UnregisteredUser
fgColorSelected color registration =
    { registration
        | foregroundColor = Just color
        , foregroundColorError = Nothing
    }


inviteReceieved : RoomInvite -> RegisteredUser -> RegisteredUser
inviteReceieved invite (RegisteredUser user) =
    if matchID invite.to (RegisteredUser user) then
        RegisteredUser { user | receivedInvites = List.append user.receivedInvites [ invite ] }

    else
        RegisteredUser user


dropInviteReceived : RoomInvite -> RegisteredUser -> RegisteredUser
dropInviteReceived invite (RegisteredUser user) =
    if matchID invite.to (RegisteredUser user) then
        RegisteredUser { user | receivedInvites = List.filter (\invite_ -> invite_ /= invite) user.receivedInvites }

    else
        RegisteredUser user


inviteSent : RoomInvite -> RegisteredUser -> RegisteredUser
inviteSent invite (RegisteredUser user) =
    if List.member invite user.sentInvites then
        RegisteredUser user

    else
        RegisteredUser { user | sentInvites = invite :: user.sentInvites }


dropInviteSent : RoomInvite -> RegisteredUser -> RegisteredUser
dropInviteSent invite (RegisteredUser user) =
    RegisteredUser
        { user
            | sentInvites =
                List.filter (\invite_ -> invite_ /= invite) user.sentInvites
        }


inviteExpired : RoomInvite -> RegisteredUser -> RegisteredUser
inviteExpired invite (RegisteredUser user) =
    if matchID invite.to (RegisteredUser user) then
        dropInviteReceived invite <|
            RegisteredUser { user | inviteError = Just RoomClosed }

    else
        RegisteredUser user


cancelInviteError : RegisteredUser -> RegisteredUser
cancelInviteError (RegisteredUser user) =
    RegisteredUser { user | inviteError = Nothing }


roomClosed : String -> RegisteredUser -> RegisteredUser
roomClosed roomId (RegisteredUser user) =
    RegisteredUser
        { user
            | receivedInvites = List.filter (\invite -> invite.roomId /= roomId) user.receivedInvites
            , sentInvites = List.filter (\invite -> invite.roomId /= roomId) user.sentInvites
        }


leftRoom : Value -> RegisteredUser -> RegisteredUser
leftRoom payload (RegisteredUser user) =
    case decodeOccupant payload of
        Ok occupant ->
            RegisteredUser { user | receivedInvites = List.filter (\invite -> (invite.from |> userId |> id) /= occupant.id && invite.roomId /= occupant.roomId) user.receivedInvites }

        Err _ ->
            RegisteredUser user


type alias Occupant =
    { id : String
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



{- Query -}


id : ID -> String
id (ID id_) =
    id_


userId : RegisteredUser -> ID
userId (RegisteredUser user) =
    user.id


username : RegisteredUser -> String
username (RegisteredUser user) =
    user.username


bgColor : RegisteredUser -> Color
bgColor (RegisteredUser { backgroundColor }) =
    backgroundColor


fgColor : RegisteredUser -> Color
fgColor (RegisteredUser { foregroundColor }) =
    foregroundColor


invitesReceived : RegisteredUser -> List RoomInvite
invitesReceived (RegisteredUser { receivedInvites }) =
    receivedInvites


invitesSent : RegisteredUser -> List RoomInvite
invitesSent (RegisteredUser { sentInvites }) =
    sentInvites


inviteError : RegisteredUser -> Maybe ErrorMessage
inviteError (RegisteredUser user) =
    user.inviteError



{- Sorting -}


sortByUsername : List RegisteredUser -> List RegisteredUser
sortByUsername users =
    List.sortWith byUsername users


byUsername : RegisteredUser -> RegisteredUser -> Order
byUsername (RegisteredUser user1) (RegisteredUser user2) =
    case compare user1.username user2.username of
        LT ->
            LT

        EQ ->
            EQ

        GT ->
            GT



{- Predicates -}


match : RegisteredUser -> RegisteredUser -> Bool
match (RegisteredUser userA) (RegisteredUser userB) =
    userA.id == userB.id


matchID : RegisteredUser -> RegisteredUser -> Bool
matchID (RegisteredUser userA) (RegisteredUser userB) =
    userA.id == userB.id


isInvited : RegisteredUser -> RegisteredUser -> Bool
isInvited (RegisteredUser user) (RegisteredUser currentUser) =
    case List.filter (\{ to } -> (to |> userId) == user.id) currentUser.sentInvites of
        [] ->
            False

        _ ->
            True



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
        [ ( "from", encode invite.from )
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
    , sentInvites : List RoomInvite
    , receivedInvites : List RoomInvite
    , inviteError : Maybe ErrorMessage
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
        |> andMap
            (field "username" string)
        |> andMap
            (field "background_color" Color.decoder
                |> andThen Color.fromRgbaDecoder
            )
        |> andMap
            (field "foreground_color" Color.decoder
                |> andThen Color.fromRgbaDecoder
            )
        |> andMap (succeed [])
        |> andMap (succeed [])
        |> andMap (succeed Nothing)
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
        |> andMap (field "from" decoder)
        |> andMap (field "to" decoder)
        |> andMap (field "room_id" string)
