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
import Element exposing (Device, Element)
import Json.Decode as JD
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..))
import Task
import Types exposing (Message, Room, User, decodeMessages, decodeRoom, initRoom, initUser)
import View.MultiRoomChat.Room as Room



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
        }


enter : User -> Room -> Phoenix.Model -> ( Model, Cmd Msg )
enter user room phoenix =
    ( Model
        { phoenix = phoenix
        , layoutHeight = 0
        , message = ""
        , messages = []
        , occupantsTyping = []
        , room = room
        , user = user
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
                                , scrollToBottom "layout"
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
subscriptions (Model { phoenix }) =
    Sub.batch
        [ onResize OnResize
        , Phoenix.subscriptions phoenix
            |> Sub.map PhoenixMsg
        ]



{- View -}


view : Device -> Model -> Element Msg
view device (Model model) =
    Room.init
        |> Room.user model.user
        |> Room.room model.room
        |> Room.messages model.messages
        |> Room.messagesContainerMaxHeight (maxHeight model.layoutHeight)
        |> Room.membersTyping model.occupantsTyping
        |> Room.userText model.message
        |> Room.onChange GotMessageChange
        |> Room.onFocus (GotMemberStartedTyping model.user model.room)
        |> Room.onLoseFocus (GotMemberStoppedTyping model.user model.room)
        |> Room.onSubmit GotSendMessage
        |> Room.view device


maxHeight : Float -> Int
maxHeight layoutHeight =
    floor <|
        (layoutHeight - 20)
