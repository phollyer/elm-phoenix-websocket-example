module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Element exposing (Device, Element, classifyDevice)
import Html
import Page
import Page.Blank as Blank
import Page.ChatRooms as ChatRooms
import Page.ControlTheSocketConnection as ControlTheSocketConnection
import Page.Home as Home
import Page.JoinAndLeaveChannels as JoinAndLeaveChannels
import Page.NotFound as NotFound
import Page.SendAndReceive as SendAndReceive
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)



{- Init -}


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect <|
            Session.init flags.vsn navKey (classifyDevice flags)
        )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg

        Just Route.ChatRooms ->
            ChatRooms.init session
                |> updateWith ChatRooms GotChatRoomsMsg

        Just Route.ControlTheSocketConnection ->
            ControlTheSocketConnection.init session
                |> updateWith ControlTheSocketConnection GotControlTheSocketConnectionMsg

        Just Route.JoinAndLeaveChannels ->
            JoinAndLeaveChannels.init session
                |> updateWith JoinAndLeaveChannels GotJoinAndLeaveChannelsMsg

        Just Route.SendAndReceive ->
            SendAndReceive.init session
                |> updateWith SendAndReceive GotSendAndReceiveMsg


type alias Flags =
    { height : Int
    , width : Int
    , vsn : String
    }



{- Model -}


type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | ChatRooms ChatRooms.Model
    | ControlTheSocketConnection ControlTheSocketConnection.Model
    | JoinAndLeaveChannels JoinAndLeaveChannels.Model
    | SendAndReceive SendAndReceive.Model



{- Update -}


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | WindowResized Int Int
    | GotHomeMsg Home.Msg
    | GotChatRoomsMsg ChatRooms.Msg
    | GotControlTheSocketConnectionMsg ControlTheSocketConnection.Msg
    | GotJoinAndLeaveChannelsMsg JoinAndLeaveChannels.Msg
    | GotSendAndReceiveMsg SendAndReceive.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( WindowResized width height, _ ) ->
            let
                session =
                    Session.updateDevice
                        (classifyDevice { width = width, height = height })
                        (toSession model)
            in
            ( updateSession session model
            , Cmd.none
            )

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> updateWith Home GotHomeMsg

        ( GotChatRoomsMsg subMsg, ChatRooms subModel ) ->
            ChatRooms.update subMsg subModel
                |> updateWith ChatRooms GotChatRoomsMsg

        ( GotControlTheSocketConnectionMsg subMsg, ControlTheSocketConnection subModel ) ->
            ControlTheSocketConnection.update subMsg subModel
                |> updateWith ControlTheSocketConnection GotControlTheSocketConnectionMsg

        ( GotJoinAndLeaveChannelsMsg subMsg, JoinAndLeaveChannels subModel ) ->
            JoinAndLeaveChannels.update subMsg subModel
                |> updateWith JoinAndLeaveChannels GotJoinAndLeaveChannelsMsg

        ( GotSendAndReceiveMsg subMsg, SendAndReceive subModel ) ->
            SendAndReceive.update subMsg subModel
                |> updateWith SendAndReceive GotSendAndReceiveMsg

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



{- Session -}


toSession : Model -> Session
toSession model =
    case model of
        Redirect session ->
            session

        NotFound session ->
            session

        Home session ->
            session

        ChatRooms subModel ->
            ChatRooms.toSession subModel

        ControlTheSocketConnection subModel ->
            ControlTheSocketConnection.toSession subModel

        JoinAndLeaveChannels subModel ->
            JoinAndLeaveChannels.toSession subModel

        SendAndReceive subModel ->
            SendAndReceive.toSession subModel


updateSession : Session -> Model -> Model
updateSession session model =
    case model of
        Redirect _ ->
            Redirect session

        NotFound _ ->
            NotFound session

        Home _ ->
            Home session

        ChatRooms subModel ->
            ChatRooms <|
                ChatRooms.updateSession session subModel

        ControlTheSocketConnection subModel ->
            ControlTheSocketConnection <|
                ControlTheSocketConnection.updateSession session subModel

        JoinAndLeaveChannels subModel ->
            JoinAndLeaveChannels <|
                JoinAndLeaveChannels.updateSession session subModel

        SendAndReceive subModel ->
            SendAndReceive <|
                SendAndReceive.updateSession session subModel



{- Device -}


toDevice : Model -> Device
toDevice model =
    Session.device (toSession model)



{- Version -}


toVersion : Model -> String
toVersion model =
    Session.vsn (toSession model)



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Redirect _ ->
            onResize WindowResized

        NotFound _ ->
            onResize WindowResized

        Home subModel ->
            Sub.batch
                [ Sub.map GotHomeMsg <|
                    Home.subscriptions subModel
                , onResize WindowResized
                ]

        ChatRooms subModel ->
            Sub.batch
                [ Sub.map GotChatRoomsMsg <|
                    ChatRooms.subscriptions subModel
                , onResize WindowResized
                ]

        ControlTheSocketConnection subModel ->
            Sub.batch
                [ Sub.map GotControlTheSocketConnectionMsg <|
                    ControlTheSocketConnection.subscriptions subModel
                , onResize WindowResized
                ]

        JoinAndLeaveChannels subModel ->
            Sub.batch
                [ Sub.map GotJoinAndLeaveChannelsMsg <|
                    JoinAndLeaveChannels.subscriptions subModel
                , onResize WindowResized
                ]

        SendAndReceive subModel ->
            Sub.batch
                [ Sub.map GotSendAndReceiveMsg <|
                    SendAndReceive.subscriptions subModel
                , onResize WindowResized
                ]



{- View -}


view : Model -> Document Msg
view model =
    let
        device =
            toDevice model

        vsn =
            toVersion model
    in
    case model of
        Redirect _ ->
            Page.view vsn device Blank.view

        NotFound _ ->
            Page.view vsn device (NotFound.view device)

        Home _ ->
            viewPage vsn device GotHomeMsg (Home.view device)

        ChatRooms subModel ->
            viewPage vsn device GotChatRoomsMsg (ChatRooms.view subModel)

        ControlTheSocketConnection subModel ->
            viewPage vsn device GotControlTheSocketConnectionMsg (ControlTheSocketConnection.view subModel)

        JoinAndLeaveChannels subModel ->
            viewPage vsn device GotJoinAndLeaveChannelsMsg (JoinAndLeaveChannels.view subModel)

        SendAndReceive subModel ->
            viewPage vsn device GotSendAndReceiveMsg (SendAndReceive.view subModel)


viewPage : String -> Device -> (msg -> Msg) -> { title : String, content : Element msg } -> Document Msg
viewPage vsn device toMsg pageConfig =
    let
        { title, body } =
            Page.view vsn device pageConfig
    in
    { title = title
    , body = List.map (Html.map toMsg) body
    }



{- Program -}


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
