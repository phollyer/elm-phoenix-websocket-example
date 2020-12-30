module Example.MultiRoomChat exposing
    ( Model
    , Msg
    , back
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Configs exposing (joinConfig)
import Element exposing (Color, Device, DeviceClass(..), Element, Orientation(..))
import Internal.Push exposing (Push)
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), JoinConfig, PhoenixMsg(..), PushConfig, pushConfig)
import Route
import Task
import Type.ChatMessage as ChatMessage
import Type.Lobby as Lobby exposing (Lobby, RoomAction(..))
import Type.Room as Room exposing (Room)
import Type.TwoTrack exposing (TwoTrack(..))
import Type.User as User exposing (InviteState(..), RegisteredUser, RoomInvite, UnregisteredUser, User(..))
import Utils exposing (updatePhoenixWith)
import View.MultiRoomChat.Lobby as LobbyView
import View.MultiRoomChat.Registration as RegistrationView
import View.MultiRoomChat.Room as RoomView



{- Types -}


type alias Model =
    { phoenix : Phoenix.Model
    , state : State
    , lobby : Lobby
    , layoutHeight : Float
    }


type State
    = Unregistered UnregisteredUser
    | InLobby RegisteredUser
    | InRoom Room RegisteredUser



{- Build -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix = phoenix
    , state = Unregistered User.init
    , lobby = Lobby.init
    , layoutHeight = 0
    }



{- Configs -}


lobbyJoinConfig : JoinConfig
lobbyJoinConfig =
    { joinConfig
        | topic = "example:lobby"
        , events =
            [ "room_closed"
            , "room_list"
            , "room_invite"
            , "invite_accepted"
            , "invite_declined"
            , "invite_expired"
            , "invite_revoked"
            , "occupant_left_room"
            ]
    }


lobbyPushConfig : PushConfig
lobbyPushConfig =
    { pushConfig
        | topic = "example:lobby"
    }


roomJoinConfig : JoinConfig
roomJoinConfig =
    { joinConfig
        | events =
            [ "message_list"
            , "member_started_typing"
            , "member_stopped_typing"
            , "room_closed"
            , "room_deleted"
            ]
    }



{- Update -}


type Msg
    = NoOp
    | OnResize Int Int
    | GotLayoutHeight (Result Dom.Error Dom.Element)
      -- Registration --
    | GotBackgroundColorSelection UnregisteredUser Color
    | GotForegroundColorSelection UnregisteredUser Color
    | GotUsernameChange UnregisteredUser String
    | GotJoinLobby UnregisteredUser
      -- Lobby --
    | GotCreateRoom RegisteredUser
    | GotEnterRoom RegisteredUser Room
    | GotDeleteRoom RegisteredUser Room
    | GotShowRoomMembers RegisteredUser Room
    | GotHideRoomMembers RegisteredUser Room
    | GotAcceptRoomInvite RegisteredUser RoomInvite
    | GotDeclineRoomInvite RegisteredUser RoomInvite
    | GotInviteErrorOk RegisteredUser RoomInvite
      -- Room --
    | GotInviteUser RegisteredUser Room RegisteredUser
    | GotMessageChange RegisteredUser Room String
    | GotMemberStartedTyping RegisteredUser Room
    | GotMemberStoppedTyping RegisteredUser Room
    | GotSendMessage RegisteredUser Room
      -- Phoenix --
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnResize _ _ ->
            ( model, getLayoutHeight )

        GotLayoutHeight result ->
            case result of
                Ok { element } ->
                    ( { model | layoutHeight = element.height }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        {- Registration -}
        GotUsernameChange currentUser name ->
            ( { model
                | state =
                    Unregistered <|
                        User.usernameChanged name currentUser
              }
            , Cmd.none
            )

        GotBackgroundColorSelection currentUser color ->
            ( { model
                | state =
                    Unregistered <|
                        User.bgColorSelected color currentUser
              }
            , Cmd.none
            )

        GotForegroundColorSelection currentUser color ->
            ( { model
                | state =
                    Unregistered <|
                        User.fgColorSelected color currentUser
              }
            , Cmd.none
            )

        GotJoinLobby currentUser ->
            case User.validate currentUser of
                Success fields ->
                    updatePhoenixWith PhoenixMsg model <|
                        Phoenix.join "example:lobby" <|
                            Phoenix.setJoinConfig
                                { lobbyJoinConfig | payload = User.encodeFields fields }
                                model.phoenix

                Failure errors ->
                    ( { model
                        | state =
                            Unregistered <|
                                User.processErrors errors currentUser
                      }
                    , Cmd.none
                    )

        {- Lobby -}
        GotCreateRoom _ ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { lobbyPushConfig | event = "create_room" }
                    model.phoenix

        GotDeleteRoom _ room ->
            updatePhoenixWith PhoenixMsg { model | lobby = Lobby.updateRoomAction Deleting room model.lobby } <|
                Phoenix.push
                    { lobbyPushConfig
                        | event = "delete_room"
                        , payload =
                            JE.object
                                [ ( "room_id", JE.string room.id ) ]
                    }
                    model.phoenix

        GotEnterRoom currentUser room ->
            updatePhoenixWith PhoenixMsg { model | lobby = Lobby.updateRoomAction Entering room model.lobby } <|
                Phoenix.join ("example:room:" ++ room.id) <|
                    Phoenix.setJoinConfig
                        { roomJoinConfig
                            | topic = "example:room:" ++ room.id
                            , payload = User.encode currentUser
                        }
                        model.phoenix

        GotAcceptRoomInvite currentUser invite ->
            updatePhoenixWith PhoenixMsg { model | state = InLobby (User.updateReceivedInvites ( Accepting, invite ) currentUser) } <|
                Phoenix.push
                    { lobbyPushConfig
                        | event = "invite_accepted"
                        , payload = User.encodeRoomInvite invite
                    }
                    model.phoenix

        GotDeclineRoomInvite currentUser invite ->
            updatePhoenixWith PhoenixMsg { model | state = InLobby (User.updateReceivedInvites ( Declining, invite ) currentUser) } <|
                Phoenix.push
                    { lobbyPushConfig
                        | event = "invite_declined"
                        , payload = User.encodeRoomInvite invite
                    }
                    model.phoenix

        GotInviteErrorOk currentUser invite ->
            ( { model
                | state =
                    InLobby <|
                        User.deleteReceivedInvite invite currentUser
              }
            , Cmd.none
            )

        GotShowRoomMembers _ room ->
            ( { model | lobby = Lobby.updateRoomAction Inspecting room model.lobby }
            , Cmd.none
            )

        GotHideRoomMembers _ room ->
            ( { model | lobby = Lobby.updateRoomAction NoAction room model.lobby }
            , Cmd.none
            )

        {- Room -}
        GotInviteUser currentUser room toUser ->
            let
                ( user, event, invite ) =
                    case User.findInviteTo toUser room.id currentUser of
                        Just ( Invited, invite_ ) ->
                            ( User.updateSentInvites ( Revoking, invite_ ) currentUser
                            , "revoke_invite"
                            , invite_
                            )

                        Just ( Inviting, invite_ ) ->
                            ( User.updateSentInvites ( Revoking, invite_ ) currentUser
                            , "revoke_invite"
                            , invite_
                            )

                        _ ->
                            let
                                invite_ =
                                    User.createInvite toUser room.id currentUser
                            in
                            ( User.updateSentInvites ( Inviting, invite_ ) currentUser
                            , "room_invite"
                            , invite_
                            )
            in
            updatePhoenixWith PhoenixMsg { model | state = InRoom room user } <|
                Phoenix.push
                    { lobbyPushConfig
                        | event = event
                        , payload = User.encodeRoomInvite invite
                    }
                    model.phoenix

        GotMessageChange currentUser room message ->
            ( { model
                | state =
                    InRoom (Room.updateMessage message room) currentUser
              }
            , Cmd.none
            )

        GotSendMessage currentUser room ->
            updatePhoenixWith PhoenixMsg { model | state = InRoom (Room.clearMessage room) currentUser } <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:room:" ++ room.id
                        , event = "new_message"
                        , payload = ChatMessage.encode room.message
                    }
                    model.phoenix

        GotMemberStartedTyping currentUser room ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:room:" ++ room.id
                        , event = "member_started_typing"
                        , payload = User.encode currentUser
                    }
                    model.phoenix

        GotMemberStoppedTyping currentUser room ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:room:" ++ room.id
                        , event = "member_stopped_typing"
                        , payload = User.encode currentUser
                    }
                    model.phoenix

        -- Incoming --
        PhoenixMsg subMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                -- Lobby Channel --
                ChannelResponse (JoinOk "example:lobby" payload) ->
                    case User.decode payload of
                        Ok currentUser ->
                            ( { newModel | state = InLobby currentUser }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "room_list" payload ->
                    case ( newModel.state, Room.decodeList payload ) of
                        ( InLobby currentUser, Ok rooms ) ->
                            ( { newModel | lobby = Lobby.roomList currentUser rooms newModel.lobby }, cmd )

                        ( InRoom _ currentUser, Ok rooms ) ->
                            ( { newModel | lobby = Lobby.roomList currentUser rooms newModel.lobby }, cmd )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "room_invite" payload ->
                    case ( newModel.state, User.decodeRoomInvite payload ) of
                        ( InLobby currentUser, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InLobby <|
                                        User.updateReceivedInvites ( Invited, invite ) currentUser
                              }
                            , cmd
                            )

                        ( InRoom room currentUser, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InRoom room <|
                                        User.updateSentInvites ( Invited, invite ) currentUser
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "invite_expired" payload ->
                    case ( newModel.state, User.decodeRoomInvite payload ) of
                        ( InLobby currentUser, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InLobby <|
                                        User.updateReceivedInvites ( Expired, invite ) currentUser
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "invite_revoked" payload ->
                    case ( newModel.state, User.decodeRoomInvite payload ) of
                        ( InLobby currentUser, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InLobby <|
                                        User.updateReceivedInvites ( Revoked, invite ) currentUser
                              }
                            , cmd
                            )

                        ( InRoom room currentUser, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InRoom room <|
                                        User.updateSentInvites ( Revoked, invite ) currentUser
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "occupant_left_room" payload ->
                    case newModel.state of
                        InLobby user ->
                            ( { newModel
                                | state =
                                    InLobby <|
                                        User.leftRoom payload user
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "invite_accepted" payload ->
                    case ( newModel.state, User.decodeRoomInvite payload ) of
                        ( InLobby currentUser, Ok invite ) ->
                            if User.match currentUser invite.to then
                                updatePhoenixWith PhoenixMsg { newModel | state = InLobby (User.updateReceivedInvites ( Accepted, invite ) currentUser) } <|
                                    Phoenix.join ("example:room:" ++ invite.roomId) <|
                                        Phoenix.setJoinConfig
                                            { roomJoinConfig
                                                | topic = "example:room:" ++ invite.roomId
                                                , payload = User.encode currentUser
                                            }
                                            newModel.phoenix

                            else
                                ( newModel, cmd )

                        ( InRoom room currentUser, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InRoom room <|
                                        User.updateSentInvites ( Accepted, invite ) currentUser
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "invite_declined" payload ->
                    case ( newModel.state, User.decodeRoomInvite payload ) of
                        ( InLobby currentUser, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InLobby <|
                                        User.updateReceivedInvites ( Declined, invite ) currentUser
                              }
                            , cmd
                            )

                        ( InRoom room currentUser, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InRoom room <|
                                        User.updateSentInvites ( Declined, invite ) currentUser
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                PresenceEvent (Phoenix.State "example:lobby" state) ->
                    case newModel.state of
                        InLobby currentUser ->
                            ( { newModel
                                | lobby =
                                    Lobby.occupantsState currentUser
                                        (User.decodePresenceState state)
                                        newModel.lobby
                              }
                            , cmd
                            )

                        InRoom _ currentUser ->
                            ( { newModel
                                | lobby =
                                    Lobby.occupantsState currentUser
                                        (User.decodePresenceState state)
                                        newModel.lobby
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                -- Room Channel --
                ChannelResponse (JoinOk _ payload) ->
                    case ( newModel.state, Room.decode payload ) of
                        ( InLobby currentUser, Ok room ) ->
                            ( { newModel
                                | state =
                                    InRoom room <|
                                        User.dropInviteForRoom room.id currentUser
                              }
                            , Cmd.batch [ cmd, getLayoutHeight ]
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent _ "message_list" payload ->
                    case ( newModel.state, ChatMessage.decodeList payload ) of
                        ( InRoom room currentUser, Ok messages ) ->
                            ( { newModel
                                | state =
                                    InRoom (Room.updateMessages messages room) currentUser
                              }
                            , Cmd.batch
                                [ cmd
                                , scrollToBottom "message-list"
                                ]
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent _ "member_started_typing" payload ->
                    case ( newModel.state, User.decode payload ) of
                        ( InRoom room currentUser, Ok user ) ->
                            ( { newModel
                                | state =
                                    InRoom (Room.addOccupantTyping currentUser user room) currentUser
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent _ "member_stopped_typing" payload ->
                    case ( newModel.state, User.decode payload ) of
                        ( InRoom room currentUser, Ok user ) ->
                            ( { newModel
                                | state =
                                    InRoom (Room.dropOccupantTyping user room) currentUser
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent _ "room_closed" payload ->
                    case ( newModel.state, Room.decode payload ) of
                        ( InLobby currentUser, Ok room ) ->
                            ( { newModel
                                | state =
                                    InLobby <|
                                        User.roomClosed room.id currentUser
                              }
                            , cmd
                            )

                        ( InRoom room _, Ok roomClosed ) ->
                            if room.id == roomClosed.id then
                                Phoenix.leave ("example:room:" ++ roomClosed.id) newModel.phoenix
                                    |> updatePhoenixWith PhoenixMsg newModel

                            else
                                ( newModel, cmd )

                        _ ->
                            ( newModel, cmd )

                PresenceEvent (Phoenix.State _ state) ->
                    case ( newModel.state, User.decodePresenceState state ) of
                        ( InRoom room currentUser, users ) ->
                            ( { newModel
                                | state =
                                    InRoom (Room.updateMembers users room) currentUser
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelResponse (LeaveOk _) ->
                    case newModel.state of
                        InRoom room currentUser ->
                            ( { newModel
                                | lobby = Lobby.resetRoomAction room newModel.lobby
                                , state =
                                    InLobby <|
                                        User.roomClosed room.id currentUser
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                _ ->
                    ( newModel, cmd )



{- Cmd -}


scrollToBottom : String -> Cmd Msg
scrollToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\{ scene } -> Dom.setViewportOf id 0 scene.height)
        |> Task.attempt (\_ -> NoOp)


getLayoutHeight : Cmd Msg
getLayoutHeight =
    Task.attempt GotLayoutHeight (Dom.getElement "layout")



{- Navigation -}


back : Nav.Key -> Model -> ( Model, Cmd Msg )
back key model =
    case model.state of
        Unregistered _ ->
            ( model, Route.back key )

        InRoom room currentUser ->
            Phoenix.leave ("example:room:" ++ room.id) model.phoenix
                |> updatePhoenixWith PhoenixMsg { model | state = InLobby currentUser }

        InLobby _ ->
            Phoenix.leave "example:lobby" model.phoenix
                |> updatePhoenixWith PhoenixMsg { model | state = Unregistered User.init }



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize OnResize
        , Sub.map PhoenixMsg <|
            Phoenix.subscriptions model.phoenix
        ]



{- View -}


view : Device -> Model -> Element Msg
view device { state, lobby, layoutHeight, phoenix } =
    case state of
        Unregistered user ->
            RegistrationView.init user
                |> RegistrationView.onChange (GotUsernameChange user)
                |> RegistrationView.onBackgroundColorChange (GotBackgroundColorSelection user)
                |> RegistrationView.onForegroundColorChange (GotForegroundColorSelection user)
                |> RegistrationView.onSubmit
                    (if Phoenix.channelQueued "example:lobby" phoenix then
                        Nothing

                     else
                        Just (GotJoinLobby user)
                    )
                |> RegistrationView.view device

        InLobby user ->
            LobbyView.init user lobby
                |> LobbyView.onCreateRoom (maybeOnCreateRoom phoenix user)
                |> LobbyView.onDeleteRoom (GotDeleteRoom user)
                |> LobbyView.onEnterRoom (GotEnterRoom user)
                |> LobbyView.onMouseEnterRoom (GotShowRoomMembers user)
                |> LobbyView.onMouseLeaveRoom (GotHideRoomMembers user)
                |> LobbyView.onAcceptRoomInvite (GotAcceptRoomInvite user)
                |> LobbyView.onDeclineRoomInvite (GotDeclineRoomInvite user)
                |> LobbyView.onInviteErrorOk GotInviteErrorOk
                |> LobbyView.view device

        InRoom room user ->
            RoomView.init room user
                |> RoomView.onChangeMessage (GotMessageChange user room)
                |> RoomView.onSubmitMessage (maybeOnSubmitMessage phoenix user room)
                |> RoomView.onFocusMessage (GotMemberStartedTyping user room)
                |> RoomView.onLoseFocusMessage (GotMemberStoppedTyping user room)
                |> RoomView.onClickUser (GotInviteUser user room)
                |> RoomView.chatMaxHeight (chatMaxHeight layoutHeight)
                |> RoomView.inviteableUsers lobby.inviteableUsers
                |> RoomView.view device


maybeOnCreateRoom : Phoenix.Model -> RegisteredUser -> Maybe Msg
maybeOnCreateRoom phoenix user =
    if Phoenix.pushWaiting (\push -> push.event == "create_room") phoenix then
        Nothing

    else
        Just (GotCreateRoom user)


maybeOnSubmitMessage : Phoenix.Model -> RegisteredUser -> Room -> Maybe Msg
maybeOnSubmitMessage phoenix user room =
    if Phoenix.pushWaiting (\push -> push.event == "new_message") phoenix then
        Nothing

    else
        Just (GotSendMessage user room)


chatMaxHeight : Float -> Int
chatMaxHeight layoutHeight =
    floor <|
        (layoutHeight - 20)
