module Example.MultiRoomChat.Room exposing
    ( Model
    , Msg
    , OutMsg(..)
    , enter
    , init
    , owner
    , subscriptions
    , toPhoenix
    , update
    , view
    )

import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Configs exposing (pushConfig)
import Element as El exposing (Device, DeviceClass(..), Element)
import Example.MultiRoomChat.Room.LobbyOccupants as LobbyOccupants
import Json.Decode as JD
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..))
import Task
import Types exposing (Message, Presence, Room, User, decodeMessages, decodeRoom, initRoom, initUser)
import UI.Padding as Padding
import View.MultiRoomChat.Room.Chat as Chat



{- Model -}


type Model
    = Model
        { phoenix : Phoenix.Model
        , layoutHeight : Float
        , message : String
        , messages : List Message
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
        , room = initRoom
        , user = initUser
        , lobbyOccupants = LobbyOccupants.init phoenix initUser initRoom []
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
                ChannelEvent _ "member_started_typing" payload ->
                    case JD.decodeValue (JD.field "username" JD.string) payload of
                        Ok username ->
                            if username /= newModel.user.username && (not <| List.member username newModel.occupantsTyping) then
                                ( Model { newModel | occupantsTyping = username :: newModel.occupantsTyping }, cmd, Empty )

                            else
                                ( Model newModel, cmd, Empty )

                        Err _ ->
                            ( Model newModel, cmd, Empty )

                ChannelEvent _ "member_stopped_typing" payload ->
                    case JD.decodeValue (JD.field "username" JD.string) payload of
                        Ok username ->
                            ( Model
                                { newModel
                                    | occupantsTyping =
                                        List.filter (\name -> name /= username) newModel.occupantsTyping
                                }
                            , cmd
                            , Empty
                            )

                        Err _ ->
                            ( Model newModel, cmd, Empty )

                ChannelEvent _ "message_list" payload ->
                    case decodeMessages payload of
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
                    case decodeRoom payload of
                        Ok room ->
                            if room.id == newModel.room.id then
                                ( Model newModel, cmd, LeaveRoom )

                            else
                                ( Model newModel, cmd, Empty )

                        Err _ ->
                            ( Model newModel, cmd, Empty )

                ChannelEvent _ "room_deleted" payload ->
                    case decodeRoom payload of
                        Ok room ->
                            if room.id == newModel.room.id then
                                ( Model newModel, cmd, LeaveRoom )

                            else
                                ( Model newModel, cmd, Empty )

                        Err _ ->
                            ( Model newModel, cmd, Empty )

                _ ->
                    ( Model newModel, cmd, Empty )


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
        [ Chat.init
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
        , LobbyOccupants.view device model.lobbyOccupants
            |> El.map LobbyOccupantsMsg
        ]


container : Device -> List (Element Msg) -> Element Msg
container { class } =
    case class of
        Phone ->
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


maxHeight : Float -> Int
maxHeight layoutHeight =
    floor <|
        (layoutHeight - 20)



{- Attributes -}
