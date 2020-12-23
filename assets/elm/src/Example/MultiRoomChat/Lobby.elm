module Example.MultiRoomChat.Lobby exposing
    ( Model
    , Msg
    , enter
    , init
    , inviteAccepted
    , inviteDeclined
    , inviteExpired
    , inviteRevoked
    , occupantLeftRoom
    , occupants
    , presenceState
    , roomClosed
    , roomInvite
    , roomList
    , toPhoenix
    , update
    , userId
    , view
    )

import Configs exposing (joinConfig, pushConfig)
import Element exposing (Device, Element)
import Json.Decode as JD
import Json.Decode.Extra exposing (andMap)
import Json.Encode as JE exposing (Value)
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..))
import Type.ErrorMessage exposing (ErrorMessage(..))
import Type.Presence as Presence exposing (Presence)
import Type.Room exposing (Room)
import Type.RoomInvite exposing (RoomInvite)
import Type.User as User exposing (User)
import Utils exposing (updatePhoenixWith)
import View.MultiRoomChat.Lobby as Lobby



{- Model -}


type Model
    = Model
        { phoenix : Phoenix.Model
        , user : User
        , presences : List Presence
        , rooms : List Room
        , showRoomMembers : Maybe Room
        , roomInvites : List RoomInvite
        , inviteError : Maybe ErrorMessage
        }


init : Phoenix.Model -> Model
init phoenix =
    Model
        { phoenix = phoenix
        , user = User.init
        , presences = []
        , rooms = []
        , showRoomMembers = Nothing
        , roomInvites = []
        , inviteError = Nothing
        }


enter : User -> Phoenix.Model -> Model
enter user phoenix =
    Model
        { phoenix = phoenix
        , user = user
        , presences =
            Phoenix.presenceState "example:lobby" phoenix
                |> Presence.decodeState
        , rooms = []
        , showRoomMembers = Nothing
        , roomInvites = []
        , inviteError = Nothing
        }


toPhoenix : Model -> Phoenix.Model
toPhoenix (Model { phoenix }) =
    phoenix


occupants : Model -> List Presence
occupants (Model { presences }) =
    presences


userId : Model -> String
userId (Model model) =
    model.user.id



{- Update -}


type Msg
    = PhoenixMsg Phoenix.Msg
    | GotCreateRoom
    | GotEnterRoom Room
    | GotDeleteRoom Room
    | GotShowRoomMembers (Maybe Room)
    | GotAcceptRoomInvite RoomInvite
    | GotDeclineRoomInvite RoomInvite
    | GotInviteErrorOk RoomInvite


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        GotCreateRoom ->
            Phoenix.push
                { pushConfig
                    | topic = "example:lobby"
                    , event = "create_room"
                }
                model.phoenix
                |> updatePhoenixWith PhoenixMsg model
                |> Tuple.mapFirst Model

        GotDeleteRoom room ->
            Phoenix.push
                { pushConfig
                    | topic = "example:lobby"
                    , event = "delete_room"
                    , payload =
                        JE.object
                            [ ( "room_id", JE.string room.id ) ]
                }
                model.phoenix
                |> updatePhoenixWith PhoenixMsg model
                |> Tuple.mapFirst Model

        GotEnterRoom room ->
            enterRoom room.id (Model model)

        GotShowRoomMembers maybeRoom ->
            ( Model { model | showRoomMembers = maybeRoom }, Cmd.none )

        GotAcceptRoomInvite invite ->
            Phoenix.push
                { pushConfig
                    | topic = "example:lobby"
                    , event = "invite_accepted"
                    , payload =
                        JE.object
                            [ ( "from", User.encode invite.from )
                            , ( "room_id", JE.string invite.roomId )
                            ]
                }
                model.phoenix
                |> updatePhoenixWith PhoenixMsg model
                |> Tuple.mapFirst Model

        GotDeclineRoomInvite invite ->
            Phoenix.push
                { pushConfig
                    | topic = "example:lobby"
                    , event = "invite_declined"
                    , payload =
                        JE.object
                            [ ( "from", User.encode invite.from )
                            , ( "room_id", JE.string invite.roomId )
                            ]
                }
                model.phoenix
                |> updatePhoenixWith PhoenixMsg { model | roomInvites = List.filter (\invite_ -> invite_ /= invite) model.roomInvites }
                |> Tuple.mapFirst Model

        GotInviteErrorOk _ ->
            ( Model { model | inviteError = Nothing }, Cmd.none )

        _ ->
            ( Model model, Cmd.none )


enterRoom : String -> Model -> ( Model, Cmd Msg )
enterRoom roomId (Model model) =
    let
        topic =
            "example:room:" ++ roomId
    in
    Phoenix.setJoinConfig
        { joinConfig
            | topic = topic
            , events =
                [ "message_list"
                , "member_started_typing"
                , "member_stopped_typing"
                , "room_closed"
                , "room_deleted"
                ]
            , payload =
                JE.object
                    [ ( "id", JE.string model.user.id ) ]
        }
        model.phoenix
        |> Phoenix.join topic
        |> updatePhoenixWith PhoenixMsg model
        |> Tuple.mapFirst Model


roomList : List Room -> Model -> Model
roomList rooms (Model model) =
    Model { model | rooms = rooms }


roomClosed : Room -> Model -> Model
roomClosed room (Model model) =
    Model
        { model | roomInvites = List.filter (\invite_ -> invite_.roomId /= room.id) model.roomInvites }



{- Room Invites -}


inviteAccepted : RoomInvite -> Model -> Model
inviteAccepted invite (Model model) =
    if invite.toId == model.user.id then
        Model
            { model | roomInvites = List.filter (\invite_ -> invite_ /= invite) model.roomInvites }

    else
        Model model


inviteDeclined : RoomInvite -> Model -> Model
inviteDeclined invite (Model model) =
    if invite.toId == model.user.id then
        Model
            { model | roomInvites = List.filter (\invite_ -> invite_ /= invite) model.roomInvites }

    else
        Model model


inviteExpired : RoomInvite -> Model -> Model
inviteExpired invite (Model model) =
    if invite.toId == model.user.id then
        Model
            { model
                | roomInvites = List.filter (\invite_ -> invite_ /= invite) model.roomInvites
                , inviteError = Just (RoomClosed invite)
            }

    else
        Model model


inviteRevoked : RoomInvite -> Model -> Model
inviteRevoked invite (Model model) =
    if invite.toId == model.user.id then
        Model
            { model | roomInvites = List.filter (\invite_ -> invite_ /= invite) model.roomInvites }

    else
        Model model


roomInvite : RoomInvite -> Model -> Model
roomInvite invite (Model model) =
    if invite.toId == model.user.id then
        Model
            { model | roomInvites = List.append model.roomInvites [ invite ] }

    else
        Model model


occupantLeftRoom : Value -> Model -> Model
occupantLeftRoom payload (Model model) =
    case decodeOccupant payload of
        Ok occupant ->
            Model { model | roomInvites = List.filter (\invite_ -> invite_.from.id /= occupant.userId && invite_.roomId /= occupant.roomId) model.roomInvites }

        Err _ ->
            Model model


type alias Occupant =
    { userId : String
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


presenceState : List Presence -> Model -> Model
presenceState state (Model model) =
    Model { model | presences = state }



{- View -}


view : Device -> Model -> Element Msg
view device (Model { presences, rooms, user, showRoomMembers, roomInvites, inviteError }) =
    Lobby.init
        |> Lobby.user user
        |> Lobby.onCreateRoom GotCreateRoom
        |> Lobby.onEnterRoom GotEnterRoom
        |> Lobby.onDeleteRoom GotDeleteRoom
        |> Lobby.onMouseEnterRoom GotShowRoomMembers
        |> Lobby.onAcceptRoomInvite GotAcceptRoomInvite
        |> Lobby.onDeclineRoomInvite GotDeclineRoomInvite
        |> Lobby.onInviteErrorOk GotInviteErrorOk
        |> Lobby.showRoomMembers showRoomMembers
        |> Lobby.members presences
        |> Lobby.rooms rooms
        |> Lobby.roomInvites roomInvites
        |> Lobby.inviteError inviteError
        |> Lobby.view device
