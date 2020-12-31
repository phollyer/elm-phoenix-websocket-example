module Example.PushOneEvent exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Element as El exposing (Device, DeviceClass(..), Element, Orientation(..))
import Extra.String as String
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), pushConfig)
import Type.Example exposing (Example(..))
import Utils exposing (updatePhoenixWith)
import View.Example.Example as Example exposing (Control(..), Response(..))



{- Types -}


type alias Model =
    { phoenix : Phoenix.Model
    , responses : List Response
    }



{- Build -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix = phoenix
    , responses = []
    }



{- Update -}


type Msg
    = GotPush
    | GotLeave
    | GotDisconnect
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPush ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:send_and_receive"
                        , event = "example_push"
                        , ref = Just "custom_ref"
                    }
                    model.phoenix

        GotLeave ->
            Phoenix.leave "example:send_and_receive" model.phoenix
                |> updatePhoenixWith PhoenixMsg model

        GotDisconnect ->
            Phoenix.disconnectAndReset (Just 1000) model.phoenix
                |> updatePhoenixWith PhoenixMsg model

        PhoenixMsg phxMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update phxMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                ChannelResponse response ->
                    ( { newModel | responses = Channel response :: newModel.responses }, cmd )

                ChannelEvent topic event payload ->
                    ( { newModel
                        | responses =
                            Event
                                { topic = topic
                                , event = event
                                , payload = payload
                                }
                                :: newModel.responses
                      }
                    , cmd
                    )

                SocketMessage response ->
                    ( { newModel | responses = Socket response :: newModel.responses }, cmd )

                _ ->
                    ( newModel, cmd )



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PhoenixMsg <|
        Phoenix.subscriptions model.phoenix



{- View -}


view : Device -> Model -> Element Msg
view device { responses, phoenix } =
    Example.init PushOneEvent
        |> Example.description
            [ [ El.text "Push an event to a Channel." ] ]
        |> Example.controls
            [ Push GotPush True
            , Leave GotLeave (Phoenix.channelJoined "example:send_and_receive" phoenix)
            , Disconnect GotDisconnect (Phoenix.isConnected phoenix)
            ]
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.push"
            , "Phoenix.leave"
            , "Phoenix.disconnectAndReset"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.isConnected", Phoenix.isConnected phoenix |> String.printBool )
            , ( "Phoenix.channelJoined", Phoenix.channelJoined "example:send_and_receive" phoenix |> String.printBool )
            , ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList )
            ]
        |> Example.view device
