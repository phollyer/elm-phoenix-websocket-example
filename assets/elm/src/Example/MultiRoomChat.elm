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
import Browser.Navigation as Nav
import Configs exposing (joinConfig)
import Element as El exposing (Color, Device, Element)
import Example.MultiRoomChat.Lobby as Lobby
import Example.MultiRoomChat.Room as ChatRoom exposing (OutMsg(..))
import Json.Decode as JD
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), pushConfig)
import Route
import Task
import Type.ChatMessage as ChatMessage
import Type.Presence as Presence
import Type.Registration as Registration exposing (Registration)
import Type.Room as Room exposing (Room)
import Type.RoomInvite as RoomInvite
import Type.TwoTrack exposing (TwoTrack(..))
import Type.User as User exposing (User)
import Utils exposing (updatePhoenixWith)
import View.MultiRoomChat.Lobby.Registration as RegistrationView



{- Init -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix = phoenix
    , state = Unregistered
    , registration = Registration.init
    , lobby = Lobby.init phoenix
    , room = ChatRoom.init phoenix
    }



{- Model -}


type alias Model =
    { phoenix : Phoenix.Model
    , state : State
    , registration : Registration
    , lobby : Lobby.Model
    , room : ChatRoom.Model
    }


type State
    = Unregistered
    | InLobby User
    | InRoom User Room



{- Update -}


type Msg
    = NoOp
      -- Registration --
    | GotBackgroundColorSelection Color
    | GotForegroundColorSelection Color
    | GotUsernameChange String
    | GotJoinLobby
    | PhoenixMsg Phoenix.Msg
    | LobbyMsg Lobby.Msg
    | RoomMsg ChatRoom.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotUsernameChange name ->
            ( { model | registration = Registration.usernameChanged name model.registration }
            , Cmd.none
            )

        GotBackgroundColorSelection color ->
            ( { model | registration = Registration.bgColorSelected color model.registration }
            , Cmd.none
            )

        GotForegroundColorSelection color ->
            ( { model | registration = Registration.fgColorSelected color model.registration }
            , Cmd.none
            )

        GotJoinLobby ->
            case Registration.validate model.registration of
                Success fields ->
                    updatePhoenixWith PhoenixMsg model <|
                        Phoenix.join "example:lobby" <|
                            Phoenix.setJoinConfig
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
                                    , payload = Registration.encode fields
                                }
                                model.phoenix

                Failure errors ->
                    ( { model | registration = Registration.processErrors errors model.registration }
                    , Cmd.none
                    )

        LobbyMsg subMsg ->
            let
                ( lobby, lobbyCmd ) =
                    Lobby.update subMsg model.lobby
            in
            ( { model
                | lobby = lobby
                , phoenix = Lobby.toPhoenix lobby
              }
            , Cmd.map LobbyMsg lobbyCmd
            )

        RoomMsg subMsg ->
            let
                ( room, roomCmd, roomMsg ) =
                    ChatRoom.update subMsg model.room
            in
            case roomMsg of
                Empty ->
                    ( { model
                        | room = room
                        , phoenix = ChatRoom.toPhoenix room
                      }
                    , Cmd.map RoomMsg roomCmd
                    )

                SendMessage message room_ ->
                    updatePhoenixWith PhoenixMsg model <|
                        Phoenix.push
                            { pushConfig
                                | topic = "example:room:" ++ room_.id
                                , event = "new_message"
                                , payload =
                                    JE.object
                                        [ ( "message", JE.string message ) ]
                            }
                            model.phoenix

                OccupantStartedTyping user room_ ->
                    updatePhoenixWith PhoenixMsg model <|
                        Phoenix.push
                            { pushConfig
                                | topic = "example:room:" ++ room_.id
                                , event = "member_started_typing"
                                , payload =
                                    JE.object
                                        [ ( "username", JE.string user.username ) ]
                            }
                            model.phoenix

                OccupantStoppedTyping user room_ ->
                    updatePhoenixWith PhoenixMsg model <|
                        Phoenix.push
                            { pushConfig
                                | topic = "example:room:" ++ room_.id
                                , event = "member_stopped_typing"
                                , payload =
                                    JE.object
                                        [ ( "username", JE.string user.username ) ]
                            }
                            model.phoenix

        PhoenixMsg subMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                ChannelEvent "example:lobby" "room_list" payload ->
                    case Room.decodeList payload of
                        Ok rooms ->
                            ( { newModel | lobby = Lobby.roomList rooms newModel.lobby }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "room_invite" payload ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            ( { newModel | lobby = Lobby.roomInvite invite newModel.lobby }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "invite_accepted" payload ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            if invite.toId == Lobby.userId newModel.lobby then
                                let
                                    topic =
                                        "example:room:" ++ invite.roomId
                                in
                                updatePhoenixWith PhoenixMsg { newModel | room = ChatRoom.inviteAccepted invite newModel.room } <|
                                    Phoenix.join topic <|
                                        Phoenix.setJoinConfig
                                            { joinConfig
                                                | topic = topic
                                                , events =
                                                    [ "message_list"
                                                    , "member_started_typing"
                                                    , "member_stopped_typing"
                                                    , "room_closed"
                                                    ]
                                                , payload =
                                                    JE.object
                                                        [ ( "id", JE.string invite.toId ) ]
                                            }
                                            model.phoenix

                            else
                                ( newModel, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "invite_expired" payload ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            ( { newModel
                                | lobby = Lobby.inviteExpired invite newModel.lobby
                              }
                            , cmd
                            )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "invite_revoked" payload ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            ( { newModel | lobby = Lobby.inviteRevoked invite newModel.lobby }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "occupant_left_room" payload ->
                    ( { newModel | lobby = Lobby.occupantLeftRoom payload newModel.lobby }, cmd )

                ChannelEvent _ "invite_declined" payload ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            ( { newModel
                                | lobby = Lobby.inviteDeclined invite newModel.lobby
                                , room = ChatRoom.inviteDeclined invite newModel.room
                              }
                            , cmd
                            )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent _ "message_list" payload ->
                    case ChatMessage.decodeList payload of
                        Ok messages ->
                            ( { newModel | room = ChatRoom.messageList messages newModel.room }
                            , Cmd.batch
                                [ cmd
                                , scrollToBottom "message-list"
                                ]
                            )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent _ "member_started_typing" payload ->
                    case JD.decodeValue (JD.field "username" JD.string) payload of
                        Ok username ->
                            ( { newModel | room = ChatRoom.occupantIsTyping username newModel.room }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent _ "member_stopped_typing" payload ->
                    case JD.decodeValue (JD.field "username" JD.string) payload of
                        Ok username ->
                            ( { newModel | room = ChatRoom.occupantStoppedTyping username newModel.room }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent topic "room_closed" payload ->
                    case Phoenix.topicParts topic of
                        [ "example", "lobby" ] ->
                            case Room.decode payload of
                                Ok room ->
                                    ( { newModel | lobby = Lobby.roomClosed room newModel.lobby }, cmd )

                                Err _ ->
                                    ( newModel, cmd )

                        [ "example", "room", id ] ->
                            Phoenix.leave ("example:room:" ++ id) newModel.phoenix
                                |> updatePhoenixWith PhoenixMsg newModel

                        _ ->
                            ( newModel, cmd )

                ChannelResponse (JoinOk "example:lobby" payload) ->
                    case User.decode payload of
                        Ok user ->
                            ( { newModel
                                | lobby = Lobby.enter user newModel.phoenix
                                , state = InLobby user
                              }
                            , cmd
                            )

                        Err _ ->
                            ( newModel, cmd )

                ChannelResponse (JoinOk _ payload) ->
                    case Room.decode payload of
                        Ok room ->
                            case model.state of
                                InLobby user ->
                                    let
                                        ( newRoom, roomCmd ) =
                                            ChatRoom.enter newModel.phoenix (Lobby.occupants model.lobby) user room
                                    in
                                    ( { newModel
                                        | state = InRoom user room
                                        , room = newRoom
                                      }
                                    , Cmd.batch
                                        [ cmd
                                        , Cmd.map RoomMsg roomCmd
                                        ]
                                    )

                                _ ->
                                    ( newModel, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelResponse (LeaveOk "example:lobby") ->
                    ( { newModel
                        | state = Unregistered
                        , registration = Registration.init
                      }
                    , cmd
                    )

                ChannelResponse (LeaveOk _) ->
                    ( { newModel
                        | state = InLobby (ChatRoom.owner model.room)
                        , lobby =
                            Lobby.enter
                                (ChatRoom.owner model.room)
                                newModel.phoenix
                      }
                    , cmd
                    )

                ChannelResponse (PushOk "example:lobby" "room_invite" _ payload) ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            ( { newModel | room = ChatRoom.inviteSent invite newModel.room }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelResponse (PushOk "example:lobby" "revoke_invite" _ payload) ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            ( { newModel | room = ChatRoom.revokeInvite invite newModel.room }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                PresenceEvent (Phoenix.State "example:lobby" state) ->
                    let
                        presenceState =
                            Presence.decodeState state
                    in
                    ( { newModel
                        | lobby = Lobby.presenceState presenceState newModel.lobby
                        , room = ChatRoom.lobbyPresenceState presenceState newModel.room
                      }
                    , cmd
                    )

                PresenceEvent (Phoenix.State _ state) ->
                    ( { newModel | room = ChatRoom.presenceState (Presence.decodeState state) newModel.room }, cmd )

                _ ->
                    ( newModel, cmd )


scrollToBottom : String -> Cmd Msg
scrollToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\{ scene } -> Dom.setViewportOf id 0 scene.height)
        |> Task.attempt (\_ -> NoOp)



{- Navigation -}


back : Nav.Key -> Model -> ( Model, Cmd Msg )
back key model =
    case model.state of
        InRoom _ room ->
            Phoenix.leave ("example:room:" ++ room.id) model.phoenix
                |> updatePhoenixWith PhoenixMsg model

        InLobby _ ->
            Phoenix.leave "example:lobby" model.phoenix
                |> updatePhoenixWith PhoenixMsg model

        Unregistered ->
            ( model, Route.back key )



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map PhoenixMsg <|
            Phoenix.subscriptions model.phoenix
        , case model.state of
            InRoom _ _ ->
                ChatRoom.subscriptions model.room
                    |> Sub.map RoomMsg

            _ ->
                Sub.none
        ]



{- View -}


view : Device -> Model -> Element Msg
view device { state, registration, lobby, room } =
    case state of
        Unregistered ->
            RegistrationView.init
                |> RegistrationView.username registration.username
                |> RegistrationView.usernameError registration.usernameError
                |> RegistrationView.backgroundColor registration.backgroundColor
                |> RegistrationView.backgroundColorError registration.backgroundColorError
                |> RegistrationView.foregroundColor registration.foregroundColor
                |> RegistrationView.foregroundColorError registration.foregroundColorError
                |> RegistrationView.onChange GotUsernameChange
                |> RegistrationView.onBackgroundColorChange GotBackgroundColorSelection
                |> RegistrationView.onForegroundColorChange GotForegroundColorSelection
                |> RegistrationView.onSubmit GotJoinLobby
                |> RegistrationView.view device

        InLobby _ ->
            Lobby.view device lobby
                |> El.map LobbyMsg

        InRoom _ _ ->
            ChatRoom.view device room
                |> El.map RoomMsg
