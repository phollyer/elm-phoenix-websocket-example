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
    = GotJoin
    | GotLeave
    | GotDisconnect
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJoin ->
            Phoenix.join "example:join_and_leave_channels" model.phoenix
                |> updatePhoenixWith PhoenixMsg model

        GotLeave ->
            Phoenix.leave "example:join_and_leave_channels" model.phoenix
                |> updatePhoenixWith PhoenixMsg model

        GotDisconnect ->
            Phoenix.disconnect (Just 1000) model.phoenix
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
    Example.init SimpleJoinAndLeave
        |> Example.description
            [ [ El.text "A simple Join to a Channel without sending any params. " ] ]
        |> Example.controls
            [ Join GotJoin (not <| Phoenix.channelJoined "example:join_and_leave_channels" phoenix)
            , Leave GotLeave (Phoenix.channelJoined "example:join_and_leave_channels" phoenix)
            , Disconnect GotDisconnect (Phoenix.isConnected phoenix)
            ]
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.join"
            , "Phoenix.leave"
            , "Phoenix.disconnect"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.isConnected", Phoenix.isConnected phoenix |> String.printBool )
            , ( "Phoenix.channelJoined", Phoenix.channelJoined "example:join_and_leave_channels" phoenix |> String.printBool )
            , ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList )
            ]
        |> Example.view device
