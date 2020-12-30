module Example.ReceiveEvents exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Configs exposing (joinConfig, pushConfig)
import Element as El exposing (Device, DeviceClass(..), Element, Orientation(..))
import Extra.String as String
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..))
import Utils exposing (updatePhoenixWith)
import View.Example as Example exposing (Response(..))



{- Types -}


type alias Model =
    { phoenix : Phoenix.Model
    , responses : List Response
    }


type Action
    = Push
    | Leave



{- Build -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix =
        Phoenix.setJoinConfig
            { joinConfig
                | topic = "example:send_and_receive"
                , events = [ "receive_event_1", "receive_event_2" ]
            }
            phoenix
    , responses = []
    }



{- Update -}


type Msg
    = GotControlClick Action
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotControlClick action ->
            case action of
                Push ->
                    model.phoenix
                        |> Phoenix.push
                            { pushConfig
                                | topic = "example:send_and_receive"
                                , event = "receive_events"
                                , ref = Just "custom_ref"
                            }
                        |> updatePhoenixWith PhoenixMsg model

                Leave ->
                    Phoenix.leave "example:send_and_receive" model.phoenix
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
    Example.init
        |> Example.description
            [ [ El.text "Receive two events from the Channel after a push." ] ]
        |> Example.controls
            [ Example.Push (GotControlClick Push) True
            , Example.Leave (GotControlClick Leave) (Phoenix.channelJoined "example:send_and_receive" phoenix)
            ]
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.setJoinConfig"
            , "Phoenix.push"
            , "Phoenix.leave"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.channelJoined", Phoenix.channelJoined "example:send_and_receive" phoenix |> String.printBool )
            , ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList )
            ]
        |> Example.view device
