module Example.MultiRoomChat exposing
    ( Model
    , Msg
    , back
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Colors.Alpha as Color
import Configs exposing (joinConfig)
import Element exposing (Color, Device, Element)
import Example.MultiRoomChat.Lobby as Lobby
import Example.MultiRoomChat.Room as Room exposing (OutMsg(..))
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..))
import Route
import Types exposing (Room, User, decodeRoom, decodeUser)
import Utils exposing (updatePhoenixWith)
import View.MultiRoomChat.Lobby.Registration as LobbyRegistration



{- Init -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix = phoenix
    , state = Unregistered
    , username = ""
    , selectedColor = Nothing
    , lobby = Lobby.init phoenix
    , room = Room.init phoenix
    }



{- Model -}


type alias Model =
    { phoenix : Phoenix.Model
    , state : State
    , username : String
    , selectedColor : Maybe Color
    , lobby : Lobby.Model
    , room : Room.Model
    }


type State
    = Unregistered
    | InLobby User
    | InRoom User Room



{- Update -}


type Msg
    = PhoenixMsg Phoenix.Msg
    | LobbyMsg Lobby.Msg
    | RoomMsg Room.Msg
    | GotColorSelection Color
    | GotUsernameChange String
    | GotJoinLobby


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUsernameChange name ->
            ( { model | username = name }, Cmd.none )

        GotColorSelection color ->
            ( { model | selectedColor = Just color }, Cmd.none )

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
                    Room.update subMsg model.room
            in
            case roomMsg of
                Empty ->
                    ( { model
                        | room = room
                        , phoenix = Room.toPhoenix room
                      }
                    , Cmd.map RoomMsg roomCmd
                    )

                LeaveRoom ->
                    ( { model
                        | room = room
                        , state = InLobby (Room.owner room)
                      }
                    , Cmd.map RoomMsg roomCmd
                    )

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
                            ( { newModel
                                | lobby = Lobby.enter user newModel.phoenix
                                , state = InLobby user
                              }
                            , cmd
                            )

                        Err _ ->
                            ( newModel, cmd )

                ChannelResponse (JoinOk _ payload) ->
                    case decodeRoom payload of
                        Ok room ->
                            case model.state of
                                InLobby user ->
                                    let
                                        ( newRoom, roomCmd ) =
                                            Room.enter user room newModel.phoenix
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

                _ ->
                    ( newModel, cmd )



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
        [ Sub.map PhoenixMsg <|
            Phoenix.subscriptions model.phoenix
        , case model.state of
            InLobby _ ->
                Lobby.subscriptions LobbyMsg model.lobby

            InRoom _ _ ->
                Room.subscriptions model.room
                    |> Sub.map RoomMsg

            _ ->
                Sub.none
        ]



{- View -}


view : Device -> Model -> Element Msg
view device model =
    case model.state of
        Unregistered ->
            LobbyRegistration.init
                |> LobbyRegistration.username model.username
                |> LobbyRegistration.selectedColor model.selectedColor
                |> LobbyRegistration.onChange GotUsernameChange
                |> LobbyRegistration.onColorChange GotColorSelection
                |> LobbyRegistration.onSubmit GotJoinLobby
                |> LobbyRegistration.view device

        InLobby _ ->
            Lobby.view device model.lobby
                |> Element.map LobbyMsg

        InRoom _ _ ->
            Room.view device model.room
                |> Element.map RoomMsg
