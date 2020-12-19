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
import Element as El exposing (Color, Device, Element)
import Example.MultiRoomChat.Lobby as Lobby
import Example.MultiRoomChat.Room as Room exposing (OutMsg(..))
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
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
    , selectedBackgroundColor = Nothing
    , selectedForegroundColor = Nothing
    , lobby = Lobby.init phoenix
    , room = Room.init phoenix
    }



{- Model -}


type alias Model =
    { phoenix : Phoenix.Model
    , state : State
    , username : String
    , selectedBackgroundColor : Maybe Color
    , selectedForegroundColor : Maybe Color
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
    | GotBackgroundColorSelection Color
    | GotForegroundColorSelection Color
    | GotUsernameChange String
    | GotJoinLobby


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUsernameChange name ->
            ( { model | username = name }, Cmd.none )

        GotBackgroundColorSelection color ->
            ( { model | selectedBackgroundColor = Just color }, Cmd.none )

        GotForegroundColorSelection color ->
            ( { model | selectedForegroundColor = Just color }, Cmd.none )

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
            case validateUserInput model.username model.selectedBackgroundColor model.selectedForegroundColor of
                Success fields ->
                    updatePhoenixWith PhoenixMsg model <|
                        Phoenix.join "example:lobby" <|
                            Phoenix.setJoinConfig
                                { joinConfig
                                    | topic = "example:lobby"
                                    , events = [ "room_list" ]
                                    , payload = encodeFields fields
                                }
                                model.phoenix

                _ ->
                    ( model, Cmd.none )

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

                        Err error ->
                            let
                                _ =
                                    Debug.log "" (JD.errorToString error)
                            in
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



{- Validation -}


validateUserInput : String -> Maybe Color -> Maybe Color -> TwoTrack
validateUserInput username maybeBackgroundColor maybeForegroundColor =
    Success []
        |> bind validateUsername username
        |> bind validateBackgroundColor maybeBackgroundColor
        |> bind validateForegroundColor maybeForegroundColor


bind : (a -> TwoTrack) -> a -> TwoTrack -> TwoTrack
bind switch input twoTrack =
    case ( switch input, twoTrack ) of
        ( Success a, Success b ) ->
            Success (List.append a b)

        ( Failure e, Failure f ) ->
            Failure (List.append e f)

        ( Failure e, _ ) ->
            Failure e

        ( _, Failure f ) ->
            Failure f


type TwoTrack
    = Success (List Field)
    | Failure (List ErrorMessage)


type Field
    = Username String
    | BackgroundColor Color
    | ForegroundColor Color


type ErrorMessage
    = UsernameCannotBeBlank
    | BackgroundColorNotSelected
    | ForegroundColorNotSelected


validateUsername : String -> TwoTrack
validateUsername username =
    if username == "" then
        Failure [ UsernameCannotBeBlank ]

    else
        Success [ Username username ]


validateBackgroundColor : Maybe Color -> TwoTrack
validateBackgroundColor maybeColor =
    case maybeColor of
        Nothing ->
            Failure [ BackgroundColorNotSelected ]

        Just color ->
            Success [ BackgroundColor color ]


validateForegroundColor : Maybe Color -> TwoTrack
validateForegroundColor maybeColor =
    case maybeColor of
        Nothing ->
            Failure [ ForegroundColorNotSelected ]

        Just color ->
            Success [ ForegroundColor color ]



{- Encoder -}


encodeFields : List Field -> Value
encodeFields fields =
    JE.object <|
        List.map encodeField fields


encodeField : Field -> ( String, Value )
encodeField field =
    case field of
        Username username ->
            ( "username", JE.string username )

        BackgroundColor color ->
            ( "background_color", encodeColor color )

        ForegroundColor color ->
            ( "foreground_color", encodeColor color )


encodeColor : Color -> Value
encodeColor color =
    let
        rgba =
            El.toRgb color
    in
    JE.object
        [ ( "red", JE.float rgba.red )
        , ( "green", JE.float rgba.green )
        , ( "blue", JE.float rgba.blue )
        , ( "alpha", JE.float rgba.alpha )
        ]



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
                |> LobbyRegistration.selectedBackgroundColor model.selectedBackgroundColor
                |> LobbyRegistration.selectedForegroundColor model.selectedForegroundColor
                |> LobbyRegistration.onChange GotUsernameChange
                |> LobbyRegistration.onBackgroundColorChange GotBackgroundColorSelection
                |> LobbyRegistration.onForegroundColorChange GotForegroundColorSelection
                |> LobbyRegistration.onSubmit GotJoinLobby
                |> LobbyRegistration.view device

        InLobby _ ->
            Lobby.view device model.lobby
                |> El.map LobbyMsg

        InRoom _ _ ->
            Room.view device model.room
                |> El.map RoomMsg
