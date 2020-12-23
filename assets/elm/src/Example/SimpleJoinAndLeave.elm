module Example.SimpleJoinAndLeave exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Element as El exposing (Device, DeviceClass(..), Element, Orientation(..))
import Extra.String as String
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), SocketMessage(..), SocketState(..))
import Utils exposing (updatePhoenixWith)
import View.Example as Example
import View.Example.ApplicableFunctions as ApplicableFunctions
import View.Example.Controls as Controls
import View.Example.Feedback as Feedback
import View.Example.Feedback.Content as FeedbackContent
import View.Example.Feedback.Info as FeedbackInfo
import View.Example.Feedback.Panel as FeedbackPanel
import View.Example.UsefulFunctions as UsefulFunctions
import View.Group as Group



{- Init -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix = phoenix
    , responses = []
    }



{- Model -}


type alias Model =
    { phoenix : Phoenix.Model
    , responses : List Response
    }


type Response
    = Socket SocketMessage
    | Channel ChannelResponse


type Action
    = Connect
    | Disconnect
    | Join
    | Leave



{- Update -}


type Msg
    = GotControlClick Action
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotControlClick action ->
            case action of
                Connect ->
                    Phoenix.connect model.phoenix
                        |> updatePhoenixWith PhoenixMsg model

                Disconnect ->
                    Phoenix.disconnect (Just 1000) model.phoenix
                        |> updatePhoenixWith PhoenixMsg model

                Join ->
                    Phoenix.join "example:join_and_leave_channels" model.phoenix
                        |> updatePhoenixWith PhoenixMsg model

                Leave ->
                    Phoenix.leave "example:join_and_leave_channels" model.phoenix
                        |> updatePhoenixWith PhoenixMsg model

        PhoenixMsg subMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                SocketMessage (StateChange state) ->
                    ( { newModel | responses = Socket (StateChange state) :: newModel.responses }, cmd )

                ChannelResponse response ->
                    ( { newModel | responses = Channel response :: newModel.responses }, cmd )

                _ ->
                    ( newModel, cmd )



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PhoenixMsg <|
        Phoenix.subscriptions model.phoenix



{- View -}


view : Device -> Model -> Element Msg
view device model =
    Example.init
        |> Example.description description
        |> Example.controls (controls device model)
        |> Example.feedback (feedback device model)
        |> Example.view device



{- Description -}


description : List (List (Element msg))
description =
    [ [ El.text "A simple Join to a Channel without sending any params. "
      , El.text ""
      ]
    ]



{- Controls -}


controls : Device -> Model -> Element Msg
controls device { phoenix } =
    Controls.init
        |> Controls.controls
            [ Controls.Connect (GotControlClick Connect) (not <| Phoenix.isConnected phoenix)
            , Controls.Join (GotControlClick Join) (not <| Phoenix.channelJoined "example:join_and_leave_channels" phoenix)
            , Controls.Leave (GotControlClick Leave) (Phoenix.channelJoined "example:join_and_leave_channels" phoenix)
            , Controls.Disconnect (GotControlClick Disconnect) (Phoenix.isConnected phoenix)
            ]
        |> Controls.group
            (Group.init
                |> Group.layouts
                    [ ( Phone, Portrait, [ 2, 2 ] ) ]
                |> Group.order
                    [ ( Phone, Portrait, [ 1, 3, 4, 2 ] ) ]
            )
        |> Controls.view device



{- Feedback -}


feedback : Device -> Model -> Element Msg
feedback device { phoenix, responses } =
    Feedback.init
        |> Feedback.elements
            [ FeedbackPanel.init
                |> FeedbackPanel.title "Info"
                |> FeedbackPanel.scrollable (responsesView device responses)
                |> FeedbackPanel.view device
            , FeedbackPanel.init
                |> FeedbackPanel.title "Applicable Functions"
                |> FeedbackPanel.scrollable [ applicableFunctions device ]
                |> FeedbackPanel.view device
            , FeedbackPanel.init
                |> FeedbackPanel.title "Useful Functions"
                |> FeedbackPanel.scrollable [ usefulFunctions device phoenix ]
                |> FeedbackPanel.view device
            ]
        |> Feedback.view device


responsesView : Device -> List Response -> List (Element Msg)
responsesView device responses =
    List.map (responseView device) responses


responseView : Device -> Response -> Element Msg
responseView device response =
    case response of
        Socket (StateChange state) ->
            FeedbackContent.init
                |> FeedbackContent.title (Just "SocketMessage")
                |> FeedbackContent.label "StateChange"
                |> FeedbackContent.element
                    (El.text (socketStateToString state))
                |> FeedbackContent.view device

        Channel (JoinOk topic payload) ->
            FeedbackContent.init
                |> FeedbackContent.title (Just "ChannelResponse")
                |> FeedbackContent.label "JoinOk"
                |> FeedbackContent.element
                    (FeedbackInfo.init
                        |> FeedbackInfo.topic topic
                        |> FeedbackInfo.payload payload
                        |> FeedbackInfo.view device
                    )
                |> FeedbackContent.view device

        Channel (LeaveOk topic) ->
            FeedbackContent.init
                |> FeedbackContent.title (Just "ChannelResponse")
                |> FeedbackContent.label "LeaveOk"
                |> FeedbackContent.element
                    (FeedbackInfo.init
                        |> FeedbackInfo.topic topic
                        |> FeedbackInfo.view device
                    )
                |> FeedbackContent.view device

        _ ->
            El.none


socketStateToString : SocketState -> String
socketStateToString state =
    case state of
        Connecting ->
            "Connecting"

        Connected ->
            "Connected"

        Disconnecting ->
            "Disconnecting"

        Disconnected _ ->
            "Disconnected"


applicableFunctions : Device -> Element Msg
applicableFunctions device =
    ApplicableFunctions.init
        |> ApplicableFunctions.functions
            [ "Phoenix.connect"
            , "Phoenix.disconnect"
            , "Phoenix.join"
            , "Phoenix.leave"
            ]
        |> ApplicableFunctions.view device


usefulFunctions : Device -> Phoenix.Model -> Element Msg
usefulFunctions device phoenix =
    UsefulFunctions.init
        |> UsefulFunctions.functions
            [ ( "Phoenix.isConnected", Phoenix.isConnected phoenix |> String.printBool )
            , ( "Phoenix.channelJoined", Phoenix.channelJoined "example:join_and_leave_channels" phoenix |> String.printBool )
            , ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList )
            ]
        |> UsefulFunctions.view device
