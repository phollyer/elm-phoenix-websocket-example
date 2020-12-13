module Example.MultiRoomChat.Lobby exposing
    ( Model
    , Msg
    , enter
    , init
    , subscriptions
    , toPhoenix
    , update
    , view
    )

import Configs exposing (joinConfig, pushConfig)
import Element exposing (Device, Element)
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..))
import Types exposing (Presence, Room, User, decodeMetas, decodeRooms, decodeUser, initUser)
import Utils
import View.MultiRoomChat.Lobby as Lobby



{- Model -}


type Model
    = Model
        { phoenix : Phoenix.Model
        , user : User
        , presences : List Presence
        , rooms : List Room
        }


init : Phoenix.Model -> Model
init phoenix =
    Model
        { phoenix = phoenix
        , user = initUser
        , presences = []
        , rooms = []
        }


enter : User -> Phoenix.Model -> Model
enter user phoenix =
    Model
        { phoenix = phoenix
        , user = user
        , presences = []
        , rooms = []
        }


toPhoenix : Model -> Phoenix.Model
toPhoenix (Model model) =
    model.phoenix



{- Update -}


type Msg
    = PhoenixMsg Phoenix.Msg
    | GotCreateRoom
    | GotEnterRoom Room
    | GotDeleteRoom Room


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        GotCreateRoom ->
            updatePhoenixWith PhoenixMsg (Model model) <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:lobby"
                        , event = "create_room"
                    }
                    model.phoenix

        GotDeleteRoom room ->
            updatePhoenixWith PhoenixMsg (Model model) <|
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
                            [ ( "id", JE.string model.user.id ) ]
                }
                model.phoenix
                |> Phoenix.join topic
                |> updatePhoenixWith PhoenixMsg (Model model)

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
                            ( Model { newModel | rooms = rooms }, cmd )

                        Err _ ->
                            ( Model newModel, cmd )

                PresenceEvent (Phoenix.State "example:lobby" state) ->
                    ( Model { newModel | presences = toPresences state }, cmd )

                _ ->
                    ( Model newModel, cmd )


updatePhoenixWith : (Phoenix.Msg -> Msg) -> Model -> ( Phoenix.Model, Cmd Phoenix.Msg ) -> ( Model, Cmd Msg )
updatePhoenixWith toMsg (Model model) ( phoenix, phoenixCmd ) =
    ( Model { model | phoenix = phoenix }, Cmd.map toMsg phoenixCmd )


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



{- Subscriptions -}


subscriptions : (Msg -> msg) -> Model -> Sub msg
subscriptions toMsg (Model { phoenix }) =
    Phoenix.subscriptions phoenix
        |> Sub.map PhoenixMsg
        |> Sub.map toMsg



{- View -}


view : Device -> Model -> Element Msg
view device (Model { presences, rooms, user }) =
    Lobby.init
        |> Lobby.user user
        |> Lobby.onCreateRoom GotCreateRoom
        |> Lobby.onEnterRoom GotEnterRoom
        |> Lobby.onDeleteRoom GotDeleteRoom
        |> Lobby.members presences
        |> Lobby.rooms rooms
        |> Lobby.view device
