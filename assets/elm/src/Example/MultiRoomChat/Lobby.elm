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
import Types exposing (Presence, Room, RoomInvitation, User, decodeMetas, decodeRoomInvitation, decodeRooms, decodeUser, initRoom, initUser, toPresences)
import View.MultiRoomChat.Lobby as Lobby exposing (roomInvitations)



{- Model -}


type Model
    = Model
        { phoenix : Phoenix.Model
        , user : User
        , presences : List Presence
        , rooms : List Room
        , showRoomMembers : Maybe Room
        , roomInvites : List RoomInvitation
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

        GotShowRoomMembers maybeRoom ->
            ( Model { model | showRoomMembers = maybeRoom }, Cmd.none )

        GotAcceptRoomInvite invite ->
            ( Model model, Cmd.none )

        GotDeclineRoomInvite invite ->
            ( Model
                { model | roomInvites = List.filter (\invite_ -> invite_ /= invite) model.roomInvites }
            , Cmd.none
            )

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

                PresenceEvent (Phoenix.State "example:lobby" state) ->
                    ( Model { newModel | presences = toPresences state }, cmd )

                _ ->
                    ( Model newModel, cmd )


updatePhoenixWith : (Phoenix.Msg -> Msg) -> Model -> ( Phoenix.Model, Cmd Phoenix.Msg ) -> ( Model, Cmd Msg )
updatePhoenixWith toMsg (Model model) ( phoenix, phoenixCmd ) =
    ( Model { model | phoenix = phoenix }, Cmd.map toMsg phoenixCmd )



{- Subscriptions -}


subscriptions : (Msg -> msg) -> Model -> Sub msg
subscriptions toMsg (Model { phoenix }) =
    Phoenix.subscriptions phoenix
        |> Sub.map PhoenixMsg
        |> Sub.map toMsg



{- View -}


view : Device -> Model -> Element Msg
view device (Model { presences, rooms, user, showRoomMembers, roomInvites }) =
    Lobby.init
        |> Lobby.user user
        |> Lobby.onCreateRoom GotCreateRoom
        |> Lobby.onEnterRoom GotEnterRoom
        |> Lobby.onDeleteRoom GotDeleteRoom
        |> Lobby.onMouseEnterRoom GotShowRoomMembers
        |> Lobby.onAcceptRoomInvite GotAcceptRoomInvite
        |> Lobby.onDeclineRoomInvite GotDeclineRoomInvite
        |> Lobby.showRoomMembers showRoomMembers
        |> Lobby.members presences
        |> Lobby.rooms rooms
        |> Lobby.roomInvitations roomInvites
        |> Lobby.view device
