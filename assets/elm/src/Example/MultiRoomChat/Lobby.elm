module Example.MultiRoomChat.Lobby exposing
    ( Model
    , Msg
    , enter
    , init
    , occupants
    , subscriptions
    , toPhoenix
    , update
    , view
    )

import Configs exposing (joinConfig, pushConfig)
import Element exposing (Device, Element)
import Json.Decode as JD
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..))
import Types exposing (ErrorMessage(..), Presence, Room, RoomInvitation, User, decodeMetas, decodeRoomInvitation, decodeRooms, decodeUser, encodeUser, initRoom, initUser, toPresences)
import Utils exposing (updatePhoenixWith)
import View.MultiRoomChat.Lobby as Lobby exposing (inviteError, roomInvitations)



{- Model -}


type Model
    = Model
        { phoenix : Phoenix.Model
        , user : User
        , presences : List Presence
        , rooms : List Room
        , showRoomMembers : Maybe Room
        , roomInvites : List RoomInvitation
        , inviteError : Maybe ErrorMessage
        }


init : Phoenix.Model -> Model
init phoenix =
    Model
        { phoenix = phoenix
        , user = initUser
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
        , presences = []
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



{- Update -}


type Msg
    = PhoenixMsg Phoenix.Msg
    | GotCreateRoom
    | GotEnterRoom Room
    | GotDeleteRoom Room
    | GotShowRoomMembers (Maybe Room)
    | GotAcceptRoomInvite RoomInvitation
    | GotDeclineRoomInvite RoomInvitation
    | GotInviteErrorOk RoomInvitation


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
                            [ ( "from", encodeUser invite.from )
                            , ( "room_id", JE.string invite.room_id )
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
                            [ ( "from", encodeUser invite.from )
                            , ( "room_id", JE.string invite.room_id )
                            ]
                }
                model.phoenix
                |> updatePhoenixWith PhoenixMsg { model | roomInvites = List.filter (\invite_ -> invite_ /= invite) model.roomInvites }
                |> Tuple.mapFirst Model

        GotInviteErrorOk invite ->
            ( Model { model | inviteError = Nothing }, Cmd.none )

        PhoenixMsg subMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                ChannelEvent "example:lobby" "room_list" payload ->
                    case decodeRooms payload of
                        Ok rooms ->
                            ( Model
                                { newModel
                                    | rooms = rooms
                                    , showRoomMembers =
                                        Maybe.map
                                            (\room ->
                                                case List.filter (\r -> r.id == room.id) rooms of
                                                    [] ->
                                                        initRoom

                                                    first :: _ ->
                                                        first
                                            )
                                            newModel.showRoomMembers
                                }
                            , cmd
                            )

                        Err error ->
                            ( Model newModel, cmd )

                ChannelEvent "example:lobby" "room_invite" payload ->
                    case decodeRoomInvitation payload of
                        Ok invite ->
                            if invite.to_id == newModel.user.id then
                                ( Model
                                    { newModel | roomInvites = List.append newModel.roomInvites [ invite ] }
                                , cmd
                                )

                            else
                                ( Model newModel, cmd )

                        Err e ->
                            ( Model newModel, cmd )

                ChannelEvent "example:lobby" "revoke_invite" payload ->
                    case decodeRoomInvitation payload of
                        Ok invite ->
                            if invite.to_id == newModel.user.id then
                                ( Model
                                    { newModel | roomInvites = List.filter (\invite_ -> invite_ /= invite) newModel.roomInvites }
                                , cmd
                                )

                            else
                                ( Model newModel, cmd )

                        Err e ->
                            ( Model newModel, cmd )

                ChannelResponse (PushOk "example:lobby" "invite_accepted" _ payload) ->
                    case decodeRoomInvitation payload of
                        Ok invite ->
                            enterRoom invite.room_id (Model { model | roomInvites = [] })

                        Err _ ->
                            ( Model newModel, cmd )

                ChannelResponse (PushError "example:lobby" "invite_accepted" _ payload) ->
                    case decodeRoomInvitation payload of
                        Ok invite ->
                            ( Model
                                { newModel
                                    | roomInvites = List.filter (\invite_ -> invite_ /= invite) newModel.roomInvites
                                    , inviteError = Just (RoomClosed invite)
                                }
                            , cmd
                            )

                        Err _ ->
                            ( Model newModel, cmd )

                ChannelResponse (PushOk "example:lobby" "invite_declined" _ payload) ->
                    case decodeRoomInvitation payload of
                        Ok invite ->
                            ( Model
                                { newModel
                                    | roomInvites = List.filter (\invite_ -> invite_ /= invite) newModel.roomInvites
                                }
                            , cmd
                            )

                        Err _ ->
                            ( Model newModel, cmd )

                PresenceEvent (Phoenix.State "example:lobby" state) ->
                    ( Model { newModel | presences = toPresences state }, cmd )

                _ ->
                    ( Model newModel, cmd )


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



{- Subscriptions -}


subscriptions : (Msg -> msg) -> Model -> Sub msg
subscriptions toMsg (Model { phoenix }) =
    Phoenix.subscriptions phoenix
        |> Sub.map PhoenixMsg
        |> Sub.map toMsg



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
        |> Lobby.roomInvitations roomInvites
        |> Lobby.inviteError inviteError
        |> Lobby.view device
