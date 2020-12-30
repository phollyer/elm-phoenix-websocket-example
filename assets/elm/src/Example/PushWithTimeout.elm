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
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), PushConfig, RetryStrategy(..))
import Type.Example exposing (Example(..))
import Type.Group as Group
import UI.FontColor as FontColor
import Utils exposing (updatePhoenixWith)
import View.Example as Example exposing (Control(..), Response(..))
import View.RadioSelection as RadioSelection



{- Types -}


type alias Model =
    { phoenix : Phoenix.Model
    , retryStrategy : RetryStrategy
    , responses : List Response
    }



{- Build -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix = phoenix
    , retryStrategy = Drop
    , responses = []
    }



{- Update -}


type Msg
    = GotPush
    | GotCancelRetry
    | GotRetryStrategy RetryStrategy
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPush ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.push
                    { pushConfig
                        | topic = "example:send_and_receive"
                        , event = "push_with_timeout"
                        , ref = Just "timeout_push"
                        , retryStrategy = model.retryStrategy
                    }
                    model.phoenix

        GotCancelRetry ->
            ( { model
                | phoenix =
                    Phoenix.dropTimeoutPush
                        (\push_ -> push_.ref == Just "timeout_push")
                        model.phoenix
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
view device { responses, phoenix, retryStrategy } =
    Example.init PushWithTimeout
        |> Example.description
            [ [ El.text "Push an event that results in a timeout - receiving feedback until the next try." ] ]
        |> Example.controls
            [ Push GotPush (not <| Phoenix.pushWaiting currentPush phoenix)
            , CancelRetry GotCancelRetry <|
                Phoenix.pushTimedOut currentPush phoenix
            ]
        |> Example.controlsGroup
            (Group.init
                |> Group.layouts [ ( Phone, Portrait, [ 2 ] ) ]
            )
        |> Example.subControls
            (RadioSelection.init
                |> RadioSelection.onChange GotRetryStrategy
                |> RadioSelection.selected retryStrategy
                |> RadioSelection.label "Select a retry strategy"
                |> RadioSelection.options
                    [ ( Drop, "Drop" )
                    , ( Every 5, "Every 5" )
                    , ( Backoff [ 1, 2, 3, 4 ] (Just 5), "Backoff [ 1, 2, 3, 4 ] (Just 5)" )
                    ]
                |> RadioSelection.view device
            )
        |> Example.status (timeoutCountdown phoenix)
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.push"
            , "Phoenix.pushTimeoutCountdown"
            , "Phoenix.dropTimeoutPush"
            , "Phoenix.dropPush"
            , "Phoenix.pushTimedOut"
            , "Phoenix.pushWaiting"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.channelJoined", Phoenix.channelJoined "example:send_and_receive" phoenix |> String.printBool )
            , ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList )
            ]
        |> Example.view device


timeoutCountdown : Phoenix.Model -> Element Msg
timeoutCountdown phoenix =
    if Phoenix.pushWaiting currentPush phoenix then
        case Phoenix.pushTimeoutCountdown currentPush phoenix of
            Nothing ->
                El.el
                    [ FontColor.default ]
                    (El.text "Push Sent")

            Just count ->
                El.row
                    [ El.width El.fill
                    , El.spacing 20
                    ]
                    [ El.el
                        [ FontColor.default ]
                        (El.text "Time to next try:")
                    , El.el
                        [ FontColor.value ]
                        (El.text <| String.fromInt count ++ " s")
                    ]

    else
        El.none


currentPush : PushConfig -> Bool
currentPush =
    \push -> push.ref == Just "timeout_push"
