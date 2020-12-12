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
import Configs exposing (joinConfig, pushConfig)
import Element exposing (Device, Element)
import Json.Decode as JD
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..))
import Route
import Task
import Types exposing (Message, Presence, Room, User, decodeMessages, decodeMetas, decodeRoom, decodeRooms, decodeUser, initUser)
import Utils exposing (updatePhoenixWith)
import View.MultiRoomChat.Lobby as Lobby
import View.MultiRoomChat.Lobby.Registration as LobbyRegistration
import View.MultiRoomChat.Room as Room



{- Init -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix = phoenix
    , state = Unregistered
    , username = ""
    , message = ""
    , messages = []
    , presences = []
    , rooms = []
    , membersTyping = []
    , layoutHeight = 0
    }



{- Model -}


type alias Model =
    { phoenix : Phoenix.Model
    , state : State
    , username : String
    , message : String
    , messages : List Message
    , presences : List Presence
    , rooms : List Room
    , membersTyping : List String
    , layoutHeight : Float
    }


type State
    = Unregistered
    | InLobby User
    | InRoom User Room



{- Update -}


type Msg
    = NoOp
    | GotUsernameChange String
    | GotJoinLobby
    | GotCreateRoom
    | GotDeleteRoom Room
    | GotEnterRoom Room
    | GotMessageChange String
    | GotMemberStartedTyping User Room
    | GotMemberStoppedTyping User Room
    | GotSendMessage
    | PhoenixMsg Phoenix.Msg
    | OnResize Int Int
    | LayoutHeight (Result Dom.Error Dom.Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnResize _ _ ->
            ( model, getLayoutHeight )

        LayoutHeight result ->
            case result of
                Ok { element } ->
                    ( { model | layoutHeight = element.height }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotUsernameChange name ->
            ( { model | username = name }, Cmd.none )

        GotMessageChange message ->
            ( { model | message = message }, Cmd.none )

        GotJoinLobby ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.join "example:lobby" <|
                    Phoenix.setJoinConfig
                        { joinConfig
                            | topic = "example:lobby"
                            , events = [ "room_list" ]
                            , payload =
                                JE.object
                                    [ ( "username", JE.string (String.trim model.username) ) ]
                        }
                        model.phoenix

        GotCreateRoom ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:lobby"
                        , event = "create_room"
                    }
                    model.phoenix

        GotDeleteRoom room ->
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

        GotEnterRoom room ->
            case model.state of
                InLobby user ->
                    let
                        topic =
                            "example:room:" ++ room.id
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
                                    [ ( "id", JE.string (String.trim user.id) ) ]
                        }
                        model.phoenix
                        |> Phoenix.join topic
                        |> updatePhoenixWith PhoenixMsg model
                        |> Utils.batch [ getLayoutHeight ]

                _ ->
                    ( model, Cmd.none )

        GotSendMessage ->
            case model.state of
                InRoom _ room ->
                    updatePhoenixWith PhoenixMsg { model | message = "" } <|
                        Phoenix.push
                            { pushConfig
                                | topic = "example:room:" ++ room.id
                                , event = "new_message"
                                , payload =
                                    JE.object
                                        [ ( "message", JE.string model.message ) ]
                            }
                            model.phoenix

                _ ->
                    ( model, Cmd.none )

        GotMemberStartedTyping user room ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:room:" ++ room.id
                        , event = "member_started_typing"
                        , payload =
                            JE.object
                                [ ( "username", JE.string user.username ) ]
                    }
                    model.phoenix

        GotMemberStoppedTyping user room ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:room:" ++ room.id
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
                ChannelResponse (JoinOk "example:lobby" payload) ->
                    case decodeUser payload of
                        Ok user ->
                            ( { newModel | state = InLobby user }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelResponse (JoinOk _ payload) ->
                    case decodeRoom payload of
                        Ok room ->
                            case model.state of
                                InLobby user ->
                                    ( { newModel | state = InRoom user room }, cmd )

                                _ ->
                                    ( newModel, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent "example:lobby" "room_list" payload ->
                    case decodeRooms payload of
                        Ok rooms ->
                            ( { newModel | rooms = rooms }, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent _ "member_started_typing" payload ->
                    case JD.decodeValue (JD.field "username" JD.string) payload of
                        Ok username ->
                            if username /= (toUser newModel).username && (not <| List.member username newModel.membersTyping) then
                                ( { newModel | membersTyping = username :: newModel.membersTyping }, cmd )

                            else
                                ( newModel, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent _ "member_stopped_typing" payload ->
                    case JD.decodeValue (JD.field "username" JD.string) payload of
                        Ok username ->
                            ( { newModel
                                | membersTyping =
                                    List.filter (\name -> name /= username) newModel.membersTyping
                              }
                            , cmd
                            )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent _ "message_list" payload ->
                    case decodeMessages payload of
                        Ok messages_ ->
                            ( { newModel | messages = messages_ }
                            , Cmd.batch
                                [ cmd
                                , scrollToBottom "message-list"
                                , scrollToBottom "layout"
                                ]
                            )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent _ "room_closed" payload ->
                    case decodeRoom payload of
                        Ok room ->
                            ( maybeLeaveRoom room newModel, cmd )

                        Err _ ->
                            ( newModel, cmd )

                ChannelEvent _ "room_deleted" payload ->
                    case decodeRoom payload of
                        Ok room ->
                            ( maybeLeaveRoom room newModel, cmd )

                        Err _ ->
                            ( newModel, cmd )

                PresenceEvent (Phoenix.State "example:lobby" state) ->
                    ( { newModel | presences = toPresences state }, cmd )

                _ ->
                    ( newModel, cmd )

        NoOp ->
            ( model, Cmd.none )


getLayoutHeight : Cmd Msg
getLayoutHeight =
    Task.attempt LayoutHeight (Dom.getElement "layout")


scrollToBottom : String -> Cmd Msg
scrollToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\{ scene } -> Dom.setViewportOf id 0 scene.height)
        |> Task.attempt (\_ -> NoOp)


maybeLeaveRoom : Room -> Model -> Model
maybeLeaveRoom room model =
    case model.state of
        InRoom user room_ ->
            if room_.id == room.id then
                { model | state = InLobby user }

            else
                model

        _ ->
            model


toPresences : List Phoenix.Presence -> List Presence
toPresences presences =
    List.map
        (\presence ->
            { id = presence.id
            , metas = decodeMetas presence.metas
            , user =
                decodeUser presence.user
                    |> Result.toMaybe
                    |> Maybe.withDefault
                        (User "" "")
            }
        )
        presences


toUser : Model -> User
toUser model =
    case model.state of
        Unregistered ->
            initUser

        InLobby user ->
            user

        InRoom user _ ->
            user



{- Navigation -}


back : Nav.Key -> Model -> ( Model, Cmd Msg )
back key model =
    case model.state of
        InRoom user room ->
            Phoenix.leave ("example:room:" ++ room.id) model.phoenix
                |> updatePhoenixWith PhoenixMsg
                    { model | state = InLobby user }

        InLobby _ ->
            Phoenix.leave "example:lobby" model.phoenix
                |> updatePhoenixWith PhoenixMsg
                    { model | state = Unregistered }

        Unregistered ->
            ( model, Route.back key )



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
view device model =
    case model.state of
        Unregistered ->
            LobbyRegistration.init
                |> LobbyRegistration.username model.username
                |> LobbyRegistration.onChange GotUsernameChange
                |> LobbyRegistration.onSubmit GotJoinLobby
                |> LobbyRegistration.view device

        InLobby user ->
            Lobby.init
                |> Lobby.user user
                |> Lobby.onCreateRoom GotCreateRoom
                |> Lobby.onEnterRoom GotEnterRoom
                |> Lobby.onDeleteRoom GotDeleteRoom
                |> Lobby.members model.presences
                |> Lobby.rooms model.rooms
                |> Lobby.view device

        InRoom user room ->
            Room.init
                |> Room.user user
                |> Room.room room
                |> Room.messages model.messages
                |> Room.messagesContainerMaxHeight (maxHeight model)
                |> Room.membersTyping model.membersTyping
                |> Room.userText model.message
                |> Room.onChange GotMessageChange
                |> Room.onFocus (GotMemberStartedTyping user room)
                |> Room.onLoseFocus (GotMemberStoppedTyping user room)
                |> Room.onSubmit GotSendMessage
                |> Room.view device


maxHeight : Model -> Int
maxHeight model =
    floor <|
        (model.layoutHeight - 20)
