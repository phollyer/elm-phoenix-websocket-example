module Example.MultiRoomChat.Room.LobbyOccupants exposing
    ( Model
    , Msg
    , init
    , inviteAccepted
    , inviteDeclined
    , inviteSent
    , revokeInvite
    , subscriptions
    , update
    , updateOccupants
    , updateRoom
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), PresenceEvent(..), pushConfig)
import Type.Presence exposing (Presence)
import Type.Room exposing (Room)
import Type.RoomInvite as RoomInvite exposing (RoomInvite)
import Type.User exposing (User)
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
            let
                invite =
                    { from = model.user
                    , toId = user.id
                    , roomId = model.room.id
                    }

                event =
                    if List.member invite model.sentInvites then
                        "revoke_invite"

                    else
                        "room_invite"
            in
            Phoenix.push
                { pushConfig
                    | topic = "example:lobby"
                    , event = event
                    , payload = RoomInvite.encode invite
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
                _ ->
                    ( Model newModel, cmd )


updateRoom : Room -> Model -> Model
updateRoom room (Model model) =
    Model { model | room = room }


updateOccupants : List Presence -> Model -> Model
updateOccupants occupants (Model model) =
    Model { model | occupants = occupants }


inviteSent : RoomInvite -> Model -> Model
inviteSent invite (Model model) =
    if List.member invite model.sentInvites then
        Model model

    else
        Model { model | sentInvites = invite :: model.sentInvites }


inviteAccepted : RoomInvite -> Model -> Model
inviteAccepted invite (Model model) =
    if invite.from.id == model.user.id then
        Model { model | sentInvites = List.filter (\invite_ -> invite_ /= invite) model.sentInvites }

    else
        Model model


inviteDeclined : RoomInvite -> Model -> Model
inviteDeclined invite (Model model) =
    if invite.from.id == model.user.id then
        Model { model | sentInvites = List.filter (\invite_ -> invite_ /= invite) model.sentInvites }

    else
        Model model


revokeInvite : RoomInvite -> Model -> Model
revokeInvite invite (Model model) =
    if invite.from.id == model.user.id then
        Model { model | sentInvites = List.filter (\invite_ -> invite_ /= invite) model.sentInvites }

    else
        Model model



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
                [ [ El.text "Click on a user to invite them into the room. "
                  , El.text "Click them again to revoke the invitation."
                  ]
                ]
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
    El.paragraph
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
        [ El.text <|
            user.username
                ++ (if isInvited user sentInvites then
                        " (Invited)"

                    else
                        ""
                   )
        ]


isInvited : User -> List RoomInvite -> Bool
isInvited user sentInvites =
    case List.filter (\invite -> invite.toId == user.id) sentInvites of
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
