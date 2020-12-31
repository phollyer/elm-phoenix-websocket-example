module Example.PushMultipleEvents exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Configs exposing (pushConfig)
import Element as El exposing (Device, DeviceClass(..), Element, Orientation(..))
import Extra.String as String
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..))
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
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPush ->
            let
                defaultConfig =
                    { pushConfig
                        | topic = "example:send_and_receive"
                        , event = "example_push"
                    }
            in
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.batchWithParams
                    [ ( Phoenix.push
                      , [ { defaultConfig | ref = Just "1" }
                        , { defaultConfig | ref = Just "2" }
                        , { defaultConfig | ref = Just "3" }
                        ]
                      )
                    ]
                    model.phoenix

        GotLeave ->
            Phoenix.leave "example:send_and_receive" model.phoenix
                |> updatePhoenixWith PhoenixMsg model

        PhoenixMsg phxMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update phxMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                Phoenix.ChannelResponse response ->
                    ( { newModel | responses = Channel response :: newModel.responses }, cmd )

                Phoenix.ChannelEvent topic event payload ->
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
    Example.init PushMultipleEvents
        |> Example.description
            [ [ El.text "Push multiple events to a Channel. "
              , El.text "This example will make 3 simultaneous pushes."
              ]
            ]
        |> Example.controls
            [ Push GotPush True
            , Leave GotLeave (Phoenix.channelJoined "example:send_and_receive" phoenix)
            ]
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.batchParams"
            , "Phoenix.push"
            , "Phoenix.leave"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.channelJoined", Phoenix.channelJoined "example:send_and_receive" phoenix |> String.printBool )
            , ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList )
            ]
        |> Example.view device
