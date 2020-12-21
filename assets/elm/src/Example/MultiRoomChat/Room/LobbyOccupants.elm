module Example.MultiRoomChat.Room.LobbyOccupants exposing (..)

import Element as El exposing (Attribute, Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), PresenceEvent(..), pushConfig)
import Tuple3
import Types exposing (Presence, Room, User, decodeUser, encodeRoom, encodeUser, initUser, toPresences)
import UI.Padding as Padding
import Utils
import View.Panel as Panel



{- Model -}


type Model
    = Model
        { phoenix : Phoenix.Model
        , occupants : List Presence
        , user : User
        , room : Room
        }


init : Phoenix.Model -> User -> Room -> List Presence -> Model
init phoenix user room occupants =
    Model
        { phoenix = phoenix
        , occupants = occupants
        , user = user
        , room = room
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
                ( phoenix, phoenixCmd ) =
                    Phoenix.push
                        { pushConfig
                            | topic = "example:lobby"
                            , event = "room_invite"
                            , payload =
                                JE.object
                                    [ ( "to", JE.string user.id )
                                    , ( "room", JE.string model.room.id )
                                    ]
                        }
                        model.phoenix
            in
            ( Model { model | phoenix = phoenix }
            , Cmd.map PhoenixMsg phoenixCmd
            )

        PhoenixMsg subMsg ->
            let
                ( phoenix, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
                        |> Tuple3.mapFirst Model
            in
            case phoenixMsg of
                PresenceEvent (State "example:lobby" presences) ->
                    ( Model { model | occupants = toPresences presences }
                    , Cmd.none
                    )

                _ ->
                    ( Model model, Cmd.none )



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
            |> Panel.title "All Users"
            |> Panel.description
                [ [ El.text "Select a user to invite them into the room." ] ]
            |> Panel.element
                (occupantsView device model.user model.occupants)
            |> Panel.view device
        ]


occupantsView : Device -> User -> List Presence -> Element Msg
occupantsView device user occupants =
    El.column
        [ padding device
        , spacing device
        , El.width El.fill
        ]
        (List.filter (\presence -> user /= presence.user) occupants
            |> List.map (occupantView device)
        )


occupantView : Device -> Presence -> Element Msg
occupantView device { user } =
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
        (El.text user.username)



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
