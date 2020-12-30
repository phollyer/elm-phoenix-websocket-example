module Example.PushWithTimeout exposing
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
import Type.Group as Group
import UI.FontColor as FontColor
import View.Example as Example exposing (Response(..))
import View.RadioSelection as RadioSelection



{- Types -}


type alias Model =
    { phoenix : Phoenix.Model
    , pushConfig : Phoenix.PushConfig
    , retryStrategy : Phoenix.RetryStrategy
    , pushSent : Bool
    , responses : List Response
    }


type Action
    = Push
    | CancelRetry
    | CancelPush



{- Build -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix = phoenix
    , pushConfig = pushConfig
    , retryStrategy = Phoenix.Drop
    , pushSent = False
    , responses = []
    }



{- Update -}


type Msg
    = GotControlClick Action
    | GotRetryStrategy Phoenix.RetryStrategy
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotControlClick action ->
            case action of
                Push ->
                    let
                        config =
                            { pushConfig
                                | topic = "example:send_and_receive"
                                , event = "push_with_timeout"
                                , ref = Just "timeout_push"
                                , retryStrategy = model.retryStrategy
                            }

                        ( phoenix, phoenixCmd ) =
                            Phoenix.push config model.phoenix
                    in
                    ( { model
                        | phoenix = phoenix
                        , pushConfig = config
                        , pushSent = True
                      }
                    , Cmd.map PhoenixMsg phoenixCmd
                    )

                CancelRetry ->
                    ( { model
                        | phoenix =
                            Phoenix.dropTimeoutPush
                                (\push_ -> push_.ref == Just "timeout_push")
                                model.phoenix
                        , pushSent = False
                      }
                    , Cmd.none
                    )

                CancelPush ->
                    ( { model
                        | phoenix =
                            Phoenix.dropPush
                                (\push_ -> push_.ref == Just "timeout_push")
                                model.phoenix
                        , pushSent = False
                      }
                    , Cmd.none
                    )

        GotRetryStrategy retryStrategy ->
            ( { model | retryStrategy = retryStrategy }, Cmd.none )

        PhoenixMsg phxMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update phxMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                ChannelResponse (PushTimeout topic event ref payload) ->
                    if model.pushConfig.retryStrategy == Phoenix.Drop then
                        ( { newModel
                            | responses = Channel (PushTimeout topic event ref payload) :: newModel.responses
                            , pushSent = False
                          }
                        , cmd
                        )

                    else
                        ( { newModel | responses = Channel (PushTimeout topic event ref payload) :: newModel.responses }, cmd )

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
view device { responses, phoenix, pushSent, retryStrategy } =
    Example.init PushWithTimeout
        |> Example.description
            [ [ El.text "Push an event that results in a timeout - receiving feedback until the next try." ] ]
        |> Example.controls
            [ Example.Push (GotControlClick Push) (not <| pushSent)
            , Example.CancelRetry (GotControlClick CancelRetry) <|
                Phoenix.pushTimedOut (\push_ -> push_.ref == Just "timeout_push") phoenix
            , Example.CancelPush (GotControlClick CancelPush) pushSent
            ]
        |> Example.controlsGroup
            (Group.init
                |> Group.layouts [ ( Phone, Portrait, [ 1, 2 ] ) ]
            )
        |> Example.subControls
            (RadioSelection.init
                |> RadioSelection.onChange GotRetryStrategy
                |> RadioSelection.selected retryStrategy
                |> RadioSelection.label "Select a retry strategy"
                |> RadioSelection.options
                    [ ( Phoenix.Drop, "Drop" )
                    , ( Phoenix.Every 5, "Every 5" )
                    , ( Phoenix.Backoff [ 1, 2, 3, 4 ] (Just 5), "Backoff [ 1, 2, 3, 4 ] (Just 5)" )
                    ]
                |> RadioSelection.view device
            )
        |> Example.status (timeoutCountdown phoenix pushSent)
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.push"
            , "Phoenix.pushTimeoutCountdown"
            , "Phoenix.dropTimeoutPush"
            , "Phoenix.dropPush"
            , "Phoenix.pushTimedOut"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.channelJoined", Phoenix.channelJoined "example:send_and_receive" phoenix |> String.printBool )
            , ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList )
            ]
        |> Example.view device


timeoutCountdown : Phoenix.Model -> Bool -> Element Msg
timeoutCountdown phoenix pushSent =
    if not pushSent then
        El.none

    else
        let
            ( label, countdown ) =
                case Phoenix.pushTimeoutCountdown (\push_ -> push_.ref == Just "timeout_push") phoenix of
                    Nothing ->
                        ( "Push Sent", "" )

                    Just count ->
                        ( "Time to next try:"
                        , String.fromInt count ++ " s"
                        )
        in
        El.row
            [ El.width El.fill
            , El.spacing 20
            ]
            [ El.el
                [ FontColor.default ]
                (El.text label)
            , El.el
                [ FontColor.value ]
                (El.text countdown)
            ]
