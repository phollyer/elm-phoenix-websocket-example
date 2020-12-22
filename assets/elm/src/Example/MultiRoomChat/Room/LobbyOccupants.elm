module Example.MultiRoomChat.Room.LobbyOccupants exposing (..)

import Element as El exposing (Attribute, Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Json.Decode as JD
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), PresenceEvent(..), pushConfig)
import Tuple3
import Type.Presence as Presence exposing (Presence)
import Type.Room exposing (Room)
import Type.RoomInvite as RoomInvite exposing (RoomInvite)
import Type.User exposing (User)
import UI.Padding as Padding
import Utils exposing (updatePhoenixWith)
import View.Panel as Panel



{- Model -}


type Model
    = Model
        { phoenix : Phoenix.Model
        , occupants : List Presence
        , user : User
        , room : Room
        , sentInvites : List RoomInvite
        }


init : Phoenix.Model -> User -> Room -> List Presence -> Model
init phoenix user room occupants =
    Model
        { phoenix = phoenix
        , occupants = occupants
        , user = user
        , room = room
        , sentInvites = []
        }



{- Update -}


type Msg
    = PhoenixMsg Phoenix.Msg
    | GotInviteUser User


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        GotInviteUser user ->
            Phoenix.push
                { pushConfig
                    | topic = "example:lobby"
                    , event = "room_invite"
                    , payload =
                        JE.object
                            [ ( "to_id", JE.string user.id )
                            , ( "room_id", JE.string model.room.id )
                            ]
                }
                model.phoenix
                |> updatePhoenixWith PhoenixMsg model
                |> Tuple.mapFirst Model

        PhoenixMsg subMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                ChannelResponse (PushOk "example:lobby" "room_invite" _ payload) ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            if List.member invite newModel.sentInvites then
                                Phoenix.push
                                    { pushConfig
                                        | topic = "example:lobby"
                                        , event = "revoke_invite"
                                        , payload =
                                            JE.object
                                                [ ( "to_id", JE.string invite.to_id )
                                                , ( "room_id", JE.string invite.room_id )
                                                ]
                                    }
                                    newModel.phoenix
                                    |> updatePhoenixWith PhoenixMsg newModel
                                    |> Tuple.mapFirst Model

                            else
                                ( Model { newModel | sentInvites = invite :: newModel.sentInvites }, cmd )

                        Err e ->
                            ( Model newModel, cmd )

                ChannelResponse (PushError "example:lobby" "room_invite" _ payload) ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            ( Model { newModel | sentInvites = invite :: newModel.sentInvites }, cmd )

                        Err _ ->
                            ( Model newModel, cmd )

                ChannelEvent "example:lobby" "invite_declined" payload ->
                    case RoomInvite.decode payload of
                        Ok invite ->
                            if invite.from.id == newModel.user.id then
                                ( Model
                                    { newModel | sentInvites = List.filter (\invite_ -> invite_ /= invite) newModel.sentInvites }
                                , cmd
                                )

                            else
                                ( Model newModel, cmd )

                        Err e ->
                            ( Model newModel, cmd )

                PresenceEvent (State "example:lobby" presences) ->
                    ( Model { newModel | occupants = Presence.decodeList presences }
                    , cmd
                    )

                _ ->
                    ( Model newModel, cmd )


updateRoom : Room -> Model -> Model
updateRoom room (Model model) =
    Model { model | room = room }


updateOccupants : List Presence -> Model -> Model
updateOccupants occupants (Model model) =
    Model { model | occupants = occupants }



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Phoenix.subscriptions model.phoenix
        |> Sub.map PhoenixMsg



{- View -}


view : Device -> Model -> Element Msg
view device (Model model) =
    El.column
        [ El.height El.fill
        , El.width El.fill
        ]
        [ Panel.init
            |> Panel.title "Lobby Occupants"
            |> Panel.description
                [ [ El.text "Select a user to invite them into the room." ] ]
            |> Panel.element
                (occupantsView device model.sentInvites model.room model.occupants)
            |> Panel.view device
        ]


occupantsView : Device -> List RoomInvite -> Room -> List Presence -> Element Msg
occupantsView device sentInvites room occupants =
    El.column
        [ padding device
        , spacing device
        , El.width El.fill
        ]
        (List.filter (\presence -> not <| List.member presence.user room.members) occupants
            |> List.map (occupantView device sentInvites)
        )


occupantView : Device -> List RoomInvite -> Presence -> Element Msg
occupantView device sentInvites { user } =
    El.el
        [ padding device
        , roundedBorders device
        , Background.color user.backgroundColor
        , Border.color user.foregroundColor
        , Border.width 1
        , El.mouseOver
            [ Border.color user.backgroundColor
            , Border.shadow
                { size = 1
                , blur = 5
                , color = user.backgroundColor
                , offset = ( 0, 0 )
                }
            ]
        , El.pointer
        , El.width El.fill
        , Event.onClick (GotInviteUser user)
        , Font.color user.foregroundColor
        ]
        (El.text <|
            user.username
                ++ (case isInvited user sentInvites of
                        True ->
                            " (Invited)"

                        False ->
                            ""
                   )
        )


isInvited : User -> List RoomInvite -> Bool
isInvited user sentInvites =
    case List.filter (\invite -> invite.to_id == user.id) sentInvites of
        [] ->
            False

        _ ->
            True



{- Attributes -}


padding : Device -> Attribute Msg
padding { class } =
    El.padding <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


roundedBorders : Device -> Attribute Msg
roundedBorders { class } =
    Border.rounded <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


spacing : Device -> Attribute Msg
spacing { class } =
    El.spacing <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10
