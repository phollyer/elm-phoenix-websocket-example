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
import Element as El exposing (Attribute, Color, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), JoinConfig, PhoenixMsg(..), pushConfig)
import Route
import Task
import Type.ChatMessage as ChatMessage
import Type.Lobby as Lobby exposing (Lobby)
import Type.Room as Room exposing (Room)
import Type.TwoTrack exposing (TwoTrack(..))
import Type.User as User exposing (RegisteredUser, RoomInvite, UnregisteredUser, User(..))
import UI.Padding as Padding
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
    | InRoom RegisteredUser Room



{- Build -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix = phoenix
    , state = Unregistered User.init
    , lobby = Lobby.init
    , layoutHeight = 0
    }


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
    | GotShowRoomMembers RegisteredUser (Maybe Room)
    | GotAcceptRoomInvite RegisteredUser RoomInvite
    | GotDeclineRoomInvite RegisteredUser RoomInvite
    | GotInviteErrorOk RegisteredUser
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
                    { pushConfig
                        | topic = "example:lobby"
                        , event = "create_room"
                    }
                    model.phoenix

        GotDeleteRoom _ room ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:lobby"
                        , event = "delete_room"
                        , payload =
                            JE.object
                                [ ( "room_id", JE.string room.id ) ]
                    }
                    model.phoenix

        GotEnterRoom currentUser room ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.join ("example:room:" ++ room.id) <|
                    Phoenix.setJoinConfig
                        { roomJoinConfig
                            | topic = "example:room:" ++ room.id
                            , payload = User.encode currentUser
                        }
                        model.phoenix

        GotAcceptRoomInvite _ invite ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:lobby"
                        , event = "invite_accepted"
                        , payload = User.encodeRoomInvite invite
                    }
                    model.phoenix

        GotDeclineRoomInvite _ invite ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:lobby"
                        , event = "invite_declined"
                        , payload = User.encodeRoomInvite invite
                    }
                    model.phoenix

        GotInviteErrorOk currentUser ->
            ( { model
                | state =
                    InLobby <|
                        User.cancelInviteError currentUser
              }
            , Cmd.none
            )

        GotShowRoomMembers _ maybeRoom ->
            ( { model | lobby = Lobby.selectedRoom maybeRoom model.lobby }
            , Cmd.none
            )

        {- Room -}
        GotInviteUser from room to ->
            let
                event =
                    if User.isInvited to from then
                        "revoke_invite"

                    else
                        "room_invite"
            in
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:lobby"
                        , event = event
                        , payload =
                            User.encodeRoomInvite
                                { from = from
                                , to = to
                                , roomId = room.id
                                }
                    }
                    model.phoenix

        GotMessageChange currentUser room message ->
            ( { model
                | state =
                    InRoom currentUser <|
                        Room.updateMessage message room
              }
            , Cmd.none
            )

        GotSendMessage currentUser room ->
            updatePhoenixWith PhoenixMsg { model | state = InRoom currentUser <| Room.clearMessage room } <|
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

        PhoenixMsg subMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                ChannelEvent "example:lobby" "room_list" payload ->
                    case ( newModel.state, Room.decodeList payload ) of
                        ( InLobby currentUser, Ok rooms ) ->
                            ( { newModel | lobby = Lobby.roomList currentUser rooms newModel.lobby }, cmd )

                        ( InRoom currentUser _, Ok rooms ) ->
                            ( { newModel | lobby = Lobby.roomList currentUser rooms newModel.lobby }, cmd )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "room_invite" payload ->
                    case ( newModel.state, User.decodeRoomInvite payload ) of
                        ( InLobby currentUser, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InLobby <|
                                        User.inviteReceieved invite currentUser
                              }
                            , cmd
                            )

                        ( InRoom currentUser room, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InRoom
                                        (User.inviteSent invite currentUser)
                                        room
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
                                        User.inviteExpired invite currentUser
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
                                        User.dropInviteReceived invite currentUser
                              }
                            , cmd
                            )

                        ( InRoom currentUser room, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InRoom
                                        (User.dropInviteSent invite currentUser)
                                        room
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

                ChannelEvent _ "invite_accepted" payload ->
                    case ( newModel.state, User.decodeRoomInvite payload ) of
                        ( InLobby currentUser, Ok invite ) ->
                            if User.match currentUser invite.to then
                                updatePhoenixWith PhoenixMsg { newModel | state = InLobby (User.dropInviteReceived invite currentUser) } <|
                                    Phoenix.join ("example:room:" ++ invite.roomId) <|
                                        Phoenix.setJoinConfig
                                            { roomJoinConfig
                                                | topic = "example:room:" ++ invite.roomId
                                                , payload = User.encode currentUser
                                            }
                                            newModel.phoenix

                            else
                                ( newModel, cmd )

                        ( InRoom currentUser room, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InRoom
                                        (User.dropInviteSent invite currentUser)
                                        room
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent _ "invite_declined" payload ->
                    case ( newModel.state, User.decodeRoomInvite payload ) of
                        ( InRoom currentUser room, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InRoom
                                        (User.dropInviteSent invite currentUser)
                                        room
                              }
                            , cmd
                            )

                        ( InLobby currentUser, Ok invite ) ->
                            ( { newModel
                                | state =
                                    InLobby <|
                                        User.dropInviteReceived invite currentUser
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent _ "message_list" payload ->
                    case ( newModel.state, ChatMessage.decodeList payload ) of
                        ( InRoom currentUser room, Ok messages ) ->
                            ( { newModel
                                | state =
                                    InRoom currentUser <|
                                        Room.updateMessages messages room
                              }
                            , Cmd.batch [ cmd, scrollToBottom "message-list" ]
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent _ "member_started_typing" payload ->
                    case ( newModel.state, User.decode payload ) of
                        ( InRoom currentUser room, Ok user ) ->
                            ( { newModel
                                | state =
                                    InRoom currentUser <|
                                        Room.addOccupantTyping currentUser user room
                              }
                            , cmd
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelEvent _ "member_stopped_typing" payload ->
                    case ( newModel.state, User.decode payload ) of
                        ( InRoom currentUser room, Ok user ) ->
                            ( { newModel
                                | state =
                                    InRoom currentUser <|
                                        Room.dropOccupantTyping user room
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

                        ( InRoom _ room, Ok roomClosed ) ->
                            if room.id == roomClosed.id then
                                Phoenix.leave ("example:room:" ++ roomClosed.id) newModel.phoenix
                                    |> updatePhoenixWith PhoenixMsg newModel

                            else
                                ( newModel, cmd )

                        _ ->
                            ( newModel, cmd )

                ChannelResponse (JoinOk "example:lobby" payload) ->
                    case User.decode payload of
                        Ok currentUser ->
                            ( { newModel | state = InLobby currentUser }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelResponse (JoinOk _ payload) ->
                    case ( newModel.state, Room.decode payload ) of
                        ( InLobby currentUser, Ok room ) ->
                            ( { newModel | state = InRoom currentUser room }
                            , Cmd.batch [ cmd, getLayoutHeight ]
                            )

                        _ ->
                            ( newModel, cmd )

                ChannelResponse (LeaveOk _) ->
                    case newModel.state of
                        InLobby _ ->
                            ( { newModel | state = Unregistered User.init }, cmd )

                        InRoom currentUser room ->
                            ( { newModel
                                | state =
                                    InLobby <|
                                        User.roomClosed room.id currentUser
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

                        InRoom currentUser _ ->
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

                PresenceEvent (Phoenix.State _ state) ->
                    case ( newModel.state, User.decodePresenceState state ) of
                        ( InRoom currentUser room, users ) ->
                            ( { newModel
                                | state =
                                    InRoom currentUser <|
                                        Room.updateMembers users room
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

        InRoom _ room ->
            Phoenix.leave ("example:room:" ++ room.id) model.phoenix
                |> updatePhoenixWith PhoenixMsg model

        InLobby _ ->
            Phoenix.leave "example:lobby" model.phoenix
                |> updatePhoenixWith PhoenixMsg model



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
view device { state, lobby, layoutHeight } =
    case state of
        Unregistered user ->
            RegistrationView.init user
                |> RegistrationView.onChange (GotUsernameChange user)
                |> RegistrationView.onBackgroundColorChange (GotBackgroundColorSelection user)
                |> RegistrationView.onForegroundColorChange (GotForegroundColorSelection user)
                |> RegistrationView.onSubmit (GotJoinLobby user)
                |> RegistrationView.view device

        InLobby user ->
            LobbyView.init user lobby
                |> LobbyView.onCreateRoom (GotCreateRoom user)
                |> LobbyView.onEnterRoom (GotEnterRoom user)
                |> LobbyView.onDeleteRoom (GotDeleteRoom user)
                |> LobbyView.onMouseEnterRoom (GotShowRoomMembers user)
                |> LobbyView.onAcceptRoomInvite (GotAcceptRoomInvite user)
                |> LobbyView.onDeclineRoomInvite (GotDeclineRoomInvite user)
                |> LobbyView.onInviteErrorOk GotInviteErrorOk
                |> LobbyView.view device

        InRoom user room ->
            RoomView.init user room
                |> RoomView.onChangeMessage (GotMessageChange user room)
                |> RoomView.onSubmitMessage (GotSendMessage user room)
                |> RoomView.onFocusMessage (GotMemberStartedTyping user room)
                |> RoomView.onLoseFocusMessage (GotMemberStoppedTyping user room)
                |> RoomView.onClickUser (GotInviteUser user room)
                |> RoomView.chatMaxHeight (maxHeight layoutHeight)
                |> RoomView.inviteableUsers lobby.inviteableUsers
                |> RoomView.view device


maxHeight : Float -> Int
maxHeight layoutHeight =
    floor <|
        (layoutHeight - 20)
