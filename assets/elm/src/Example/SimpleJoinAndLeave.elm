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
import Type.Group as Group
import Utils exposing (updatePhoenixWith)
import View.Example as Example exposing (Response(..))



{- Types -}


type alias Model =
    { phoenix : Phoenix.Model
    , responses : List Response
    }


type Action
    = Connect
    | Disconnect
    | Join
    | Leave



{- Build -}


init : Phoenix.Model -> Model
init phoenix =
    { phoenix = phoenix
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
view device { responses, phoenix } =
    Example.init
        |> Example.description
            [ [ El.text "A simple Join to a Channel without sending any params. " ] ]
        |> Example.controls
            [ Example.Connect (GotControlClick Connect) (not <| Phoenix.isConnected phoenix)
            , Example.Join (GotControlClick Join) (not <| Phoenix.channelJoined "example:join_and_leave_channels" phoenix)
            , Example.Leave (GotControlClick Leave) (Phoenix.channelJoined "example:join_and_leave_channels" phoenix)
            , Example.Disconnect (GotControlClick Disconnect) (Phoenix.isConnected phoenix)
            ]
        |> Example.controlsGroup
            (Group.init
                |> Group.layouts
                    [ ( Phone, Portrait, [ 2, 2 ] ) ]
                |> Group.order
                    [ ( Phone, Portrait, [ 1, 3, 4, 2 ] ) ]
            )
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.connect"
            , "Phoenix.disconnect"
            , "Phoenix.join"
            , "Phoenix.leave"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.isConnected", Phoenix.isConnected phoenix |> String.printBool )
            , ( "Phoenix.channelJoined", Phoenix.channelJoined "example:join_and_leave_channels" phoenix |> String.printBool )
            , ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList )
            ]
        |> Example.view device
