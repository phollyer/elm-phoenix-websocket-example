module Example.MultiRoomChat.Room exposing
    ( Model
    , Msg
    , OutMsg(..)
    , enter
    , init
    , lobbyPresenceState
    , occupantIsTyping
    , occupantStoppedTyping
    , owner
    , presenceState
    , subscriptions
    , toId
    , toPhoenix
    , update
    , view
    )

import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Configs exposing (pushConfig)
import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Example.MultiRoomChat.Room.LobbyOccupants as LobbyOccupants
import Json.Decode as JD
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), PresenceEvent(..))
import Session exposing (device)
import Task
import Type.ChatMessage as ChatMessage exposing (ChatMessage)
import Type.Presence as Presence exposing (Presence)
import Type.Room as Room exposing (Room)
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
    | LeaveRoom


type Msg
    = NoOp
    | PhoenixMsg Phoenix.Msg
    | LobbyOccupantsMsg LobbyOccupants.Msg
    | OnResize Int Int
    | LayoutHeight (Result Dom.Error Dom.Element)
    | GotMessageChange String
    | GotMemberStartedTyping User Room
    | GotMemberStoppedTyping User Room
    | GotSendMessage


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg (Model model) =
    case msg of
        NoOp ->
            ( Model model, Cmd.none, Empty )

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
            updatePhoenixWith PhoenixMsg (Model { model | message = "" }) <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:room:" ++ model.room.id
                        , event = "new_message"
                        , payload =
                            JE.object
                                [ ( "message", JE.string model.message ) ]
                    }
                    model.phoenix

        GotMemberStartedTyping user room ->
            updatePhoenixWith PhoenixMsg (Model model) <|
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
            updatePhoenixWith PhoenixMsg (Model model) <|
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
                ChannelEvent _ "message_list" payload ->
                    case ChatMessage.decodeList payload of
                        Ok messages_ ->
                            ( Model { newModel | messages = messages_ }
                            , Cmd.batch
                                [ cmd
                                , scrollToBottom "message-list"
                                ]
                            , Empty
                            )

                        Err _ ->
                            ( Model newModel, cmd, Empty )

                ChannelEvent _ "room_closed" payload ->
                    case Room.decode payload of
                        Ok room ->
                            if room.id == newModel.room.id then
                                ( Model newModel, cmd, LeaveRoom )

                            else
                                ( Model newModel, cmd, Empty )

                        Err _ ->
                            ( Model newModel, cmd, Empty )

                ChannelEvent _ "room_deleted" payload ->
                    case Room.decode payload of
                        Ok room ->
                            if room.id == newModel.room.id then
                                ( Model newModel, cmd, LeaveRoom )

                            else
                                ( Model newModel, cmd, Empty )

                        Err _ ->
                            ( Model newModel, cmd, Empty )

                _ ->
                    ( Model newModel, cmd, Empty )


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


presenceState : String -> List Presence -> Model -> Model
presenceState roomId state (Model model) =
    if roomId == model.room.id then
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

    else
        Model model


updatePhoenixWith : (Phoenix.Msg -> Msg) -> Model -> ( Phoenix.Model, Cmd Phoenix.Msg ) -> ( Model, Cmd Msg, OutMsg )
updatePhoenixWith toMsg (Model model) ( phoenix, phoenixCmd ) =
    ( Model { model | phoenix = phoenix }, Cmd.map toMsg phoenixCmd, Empty )


getLayoutHeight : Cmd Msg
getLayoutHeight =
    Task.attempt LayoutHeight (Dom.getElement "layout")


scrollToBottom : String -> Cmd Msg
scrollToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\{ scene } -> Dom.setViewportOf id 0 scene.height)
        |> Task.attempt (\_ -> NoOp)



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions (Model { phoenix, lobbyOccupants }) =
    Sub.batch
        [ onResize OnResize
        , Phoenix.subscriptions phoenix
            |> Sub.map PhoenixMsg
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
