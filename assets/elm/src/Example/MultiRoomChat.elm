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
import Colors.Alpha as Color
import Configs exposing (joinConfig)
import Element as El exposing (Color, Device, Element)
import Example.MultiRoomChat.Lobby as Lobby
import Example.MultiRoomChat.Registration as Registration
import Example.MultiRoomChat.Room as ChatRoom exposing (OutMsg(..))
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..))
import Route
import Task
import Type.ChatMessage as ChatMessage
import Type.Presence as Presence
import Type.Room as Room exposing (Room)
import Type.RoomInvite as RoomInvite
import Type.User as User exposing (User)
import Utils exposing (updatePhoenixWith)



{- Init -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix = phoenix
    , state = Unregistered
    , registration = Registration.init phoenix
    , lobby = Lobby.init phoenix
    , room = ChatRoom.init phoenix
    }



{- Model -}


type alias Model =
    { phoenix : Phoenix.Model
    , state : State
    , registration : Registration.Model
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
    | PhoenixMsg Phoenix.Msg
    | LobbyMsg Lobby.Msg
    | RegistrationMsg Registration.Msg
    | RoomMsg ChatRoom.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RegistrationMsg subMsg ->
            let
                ( registration, registrationCmd, outMsg ) =
                    Registration.update subMsg model.registration
            in
            case outMsg of
                Registration.JoinLobby phoenix user ->
                    ( { model
                        | lobby = Lobby.enter user phoenix
                        , phoenix = phoenix
                        , state = InLobby user
                      }
                    , Cmd.map RegistrationMsg registrationCmd
                    )

                Registration.Empty ->
                    ( { model
                        | registration = registration
                        , phoenix = Registration.toPhoenix registration
                      }
                    , Cmd.map RegistrationMsg registrationCmd
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

                LeaveRoom ->
                    Phoenix.leave ("example:room:" ++ ChatRoom.toId room) model.phoenix
                        |> updatePhoenixWith PhoenixMsg { model | room = room }

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

                        Err error ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "room_invite" payload ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            ( { newModel | lobby = Lobby.roomInvite invite newModel.lobby }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "revoke_invite" payload ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            ( { newModel | lobby = Lobby.revokeInvite invite newModel.lobby }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent topic "message_list" payload ->
                    case ( Phoenix.topicParts topic, ChatMessage.decodeList payload ) of
                        ( [ "example", "room", id ], Ok messages ) ->
                            ( { newModel | room = ChatRoom.messageList id messages newModel.room }
                            , Cmd.batch
                                [ cmd
                                , scrollToBottom "message-list"
                                ]
                            )

                        _ ->
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

                ChannelResponse (JoinOk "example:lobby" payload) ->
                    case User.decode payload of
                        Ok user ->
                            ( { newModel
                                | lobby = Lobby.enter user newModel.phoenix
                                , state = InLobby user
                              }
                            , cmd
                            )

                        Err error ->
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
                        , registration = Registration.init newModel.phoenix
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

                PresenceEvent (Phoenix.State topic state) ->
                    case Phoenix.topicParts topic of
                        [ "example", "room", id ] ->
                            ( { newModel | room = ChatRoom.presenceState id (Presence.decodeState state) newModel.room }, cmd )

                        _ ->
                            ( newModel, cmd )

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
        InRoom user room ->
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
            InLobby _ ->
                Lobby.subscriptions LobbyMsg model.lobby

            InRoom _ _ ->
                ChatRoom.subscriptions model.room
                    |> Sub.map RoomMsg

            _ ->
                Sub.none
        ]



{- View -}


view : Device -> Model -> Element Msg
view device model =
    case model.state of
        Unregistered ->
            Registration.view device model.registration
                |> El.map RegistrationMsg

        InLobby _ ->
            Lobby.view device model.lobby
                |> El.map LobbyMsg

        InRoom _ _ ->
            ChatRoom.view device model.room
                |> El.map RoomMsg
