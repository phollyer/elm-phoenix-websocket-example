module Example.MultiRoomChat.Room exposing
    ( Model
    , Msg
    , OutMsg(..)
    , enter
    , init
    , inviteAccepted
    , inviteDeclined
    , inviteSent
    , lobbyPresenceState
    , messageList
    , occupantIsTyping
    , occupantStoppedTyping
    , owner
    , presenceState
    , revokeInvite
    , subscriptions
    , toId
    , toPhoenix
    , update
    , view
    )

import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Example.MultiRoomChat.Room.LobbyOccupants as LobbyOccupants
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), PresenceEvent(..))
import Task
import Type.ChatMessage exposing (ChatMessage)
import Type.Presence exposing (Presence)
import Type.Room as Room exposing (Room)
import Type.RoomInvite exposing (RoomInvite)
import Type.User as User exposing (User)
import UI.Padding as Padding
import View.MultiRoomChat.Room.Chat as Chat
import View.Panel as Panel



{- Model -}


type Model
    = Model
        { phoenix : Phoenix.Model
        , layoutHeight : Float
        , message : String
        , messages : List ChatMessage
        , occupantsTyping : List String
        , room : Room
        , user : User
        , lobbyOccupants : LobbyOccupants.Model
        }


init : Phoenix.Model -> Model
init phoenix =
    Model
        { phoenix = phoenix
        , layoutHeight = 0
        , message = ""
        , messages = []
        , occupantsTyping = []
        , room = Room.init
        , user = User.init
        , lobbyOccupants = LobbyOccupants.init phoenix User.init Room.init []
        }


enter : Phoenix.Model -> List Presence -> User -> Room -> ( Model, Cmd Msg )
enter phoenix occupants user room =
    ( Model
        { phoenix = phoenix
        , layoutHeight = 0
        , message = ""
        , messages = []
        , occupantsTyping = []
        , room = room
        , user = user
        , lobbyOccupants = LobbyOccupants.init phoenix user room occupants
        }
    , getLayoutHeight
    )


owner : Model -> User
owner (Model { user }) =
    user


toPhoenix : Model -> Phoenix.Model
toPhoenix (Model model) =
    model.phoenix


toId : Model -> String
toId (Model model) =
    model.room.id



{- Update -}


type OutMsg
    = Empty
    | SendMessage String Room
    | OccupantStartedTyping User Room
    | OccupantStoppedTyping User Room


type Msg
    = LobbyOccupantsMsg LobbyOccupants.Msg
    | OnResize Int Int
    | LayoutHeight (Result Dom.Error Dom.Element)
    | GotMessageChange String
    | GotMemberStartedTyping User Room
    | GotMemberStoppedTyping User Room
    | GotSendMessage


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg (Model model) =
    case msg of
        LobbyOccupantsMsg subMsg ->
            let
                ( lobbyOccupants, loCmd ) =
                    LobbyOccupants.update subMsg model.lobbyOccupants
            in
            ( Model { model | lobbyOccupants = lobbyOccupants }, Cmd.map LobbyOccupantsMsg loCmd, Empty )

        OnResize _ _ ->
            ( Model model, getLayoutHeight, Empty )

        LayoutHeight result ->
            case result of
                Ok { element } ->
                    ( Model { model | layoutHeight = element.height }, Cmd.none, Empty )

                _ ->
                    ( Model model, Cmd.none, Empty )

        GotMessageChange message ->
            ( Model { model | message = message }, Cmd.none, Empty )

        GotSendMessage ->
            ( Model { model | message = "" }, Cmd.none, SendMessage model.message model.room )

        GotMemberStartedTyping user room ->
            ( Model model, Cmd.none, OccupantStartedTyping user room )

        GotMemberStoppedTyping user room ->
            ( Model model, Cmd.none, OccupantStoppedTyping user room )


inviteSent : RoomInvite -> Model -> Model
inviteSent invite (Model model) =
    Model { model | lobbyOccupants = LobbyOccupants.inviteSent invite model.lobbyOccupants }


inviteAccepted : RoomInvite -> Model -> Model
inviteAccepted invite (Model model) =
    Model { model | lobbyOccupants = LobbyOccupants.inviteAccepted invite model.lobbyOccupants }


inviteDeclined : RoomInvite -> Model -> Model
inviteDeclined invite (Model model) =
    Model { model | lobbyOccupants = LobbyOccupants.inviteDeclined invite model.lobbyOccupants }


revokeInvite : RoomInvite -> Model -> Model
revokeInvite invite (Model model) =
    Model { model | lobbyOccupants = LobbyOccupants.revokeInvite invite model.lobbyOccupants }


occupantIsTyping : String -> Model -> Model
occupantIsTyping username (Model model) =
    if username /= model.user.username && (not <| List.member username model.occupantsTyping) then
        Model { model | occupantsTyping = username :: model.occupantsTyping }

    else
        Model model


occupantStoppedTyping : String -> Model -> Model
occupantStoppedTyping username (Model model) =
    Model { model | occupantsTyping = List.filter (\name -> name /= username) model.occupantsTyping }


lobbyPresenceState : List Presence -> Model -> Model
lobbyPresenceState state (Model model) =
    Model
        { model
            | lobbyOccupants =
                LobbyOccupants.updateOccupants state model.lobbyOccupants
        }


messageList : List ChatMessage -> Model -> Model
messageList messages (Model model) =
    Model { model | messages = messages }


presenceState : List Presence -> Model -> Model
presenceState state (Model model) =
    let
        room =
            Room.updateMembers
                (List.map .user state)
                model.room
    in
    Model
        { model
            | room = room
            , lobbyOccupants =
                LobbyOccupants.updateRoom room model.lobbyOccupants
        }


getLayoutHeight : Cmd Msg
getLayoutHeight =
    Task.attempt LayoutHeight (Dom.getElement "layout")



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions (Model { lobbyOccupants }) =
    Sub.batch
        [ onResize OnResize
        , LobbyOccupants.subscriptions lobbyOccupants
            |> Sub.map LobbyOccupantsMsg
        ]



{- View -}


view : Device -> Model -> Element Msg
view device (Model model) =
    container device
        [ El.el
            [ chatWidth device ]
            (Chat.init
                |> Chat.user model.user
                |> Chat.room model.room
                |> Chat.messages model.messages
                |> Chat.messagesContainerMaxHeight (maxHeight model.layoutHeight)
                |> Chat.membersTyping model.occupantsTyping
                |> Chat.userText model.message
                |> Chat.onChange GotMessageChange
                |> Chat.onFocus (GotMemberStartedTyping model.user model.room)
                |> Chat.onLoseFocus (GotMemberStoppedTyping model.user model.room)
                |> Chat.onSubmit GotSendMessage
                |> Chat.view device
            )
        , panelContainer device
            [ roomOccupants device model.room.members
            , LobbyOccupants.view device model.lobbyOccupants
                |> El.map LobbyOccupantsMsg
            ]
        ]


container : Device -> List (Element Msg) -> Element Msg
container { class, orientation } =
    case ( class, orientation ) of
        ( Phone, _ ) ->
            El.column
                [ El.width El.fill
                , El.spacing 10
                ]

        ( Tablet, Portrait ) ->
            El.column
                [ El.width El.fill
                , El.spacing 10
                ]

        _ ->
            El.row
                [ El.height El.fill
                , El.width El.fill
                , El.spacing 10
                , Padding.right 10
                ]


panelContainer : Device -> List (Element Msg) -> Element Msg
panelContainer ({ class, orientation } as device) =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            El.column
                [ El.width El.fill
                , El.spacing 10
                ]

        _ ->
            El.row
                [ panelWidth device
                , El.height El.fill
                , El.spacing 10
                ]


roomOccupants : Device -> List User -> Element Msg
roomOccupants device occupants =
    Panel.init
        |> Panel.title "Room Occupants"
        |> Panel.element
            (El.column
                [ padding device
                , spacing device
                , El.width El.fill
                ]
                (List.map (toRoomOccupant device) occupants)
            )
        |> Panel.view device


toRoomOccupant : Device -> User -> Element Msg
toRoomOccupant device occupant =
    El.paragraph
        [ padding device
        , roundedBorders device
        , Background.color occupant.backgroundColor
        , Border.color occupant.foregroundColor
        , Border.width 1
        , El.width El.fill
        , Font.color occupant.foregroundColor
        ]
        [ El.text occupant.username ]


maxHeight : Float -> Int
maxHeight layoutHeight =
    floor <|
        (layoutHeight - 20)



{- Attributes -}


chatWidth : Device -> Attribute Msg
chatWidth { class } =
    El.width <|
        case class of
            Phone ->
                El.fill

            _ ->
                El.fillPortion 3


panelWidth : Device -> Attribute Msg
panelWidth { class } =
    El.width <|
        case class of
            Phone ->
                El.fill

            _ ->
                El.fillPortion 2


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
