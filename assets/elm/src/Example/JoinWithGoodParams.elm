module Example.JoinWithGoodParams exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Element as El exposing (Device, Element)
import Extra.String as String
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), SocketMessage(..), joinConfig)
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
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJoin ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.join "example:join_and_leave_channels" <|
                    Phoenix.setJoinConfig
                        { joinConfig
                            | topic = "example:join_and_leave_channels"
                            , payload =
                                JE.object
                                    [ ( "username", JE.string "good" )
                                    , ( "password", JE.string "good" )
                                    ]
                        }
                        model.phoenix

        GotLeave ->
            Phoenix.leave "example:join_and_leave_channels" model.phoenix
                |> updatePhoenixWith PhoenixMsg model

        PhoenixMsg subMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                ChannelResponse response ->
                    ( { newModel | responses = Channel response :: newModel.responses }, cmd )

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
    Example.init JoinWithGoodParams
        |> Example.description
            [ [ El.text "Join a Channel, providing auth params that are accepted." ] ]
        |> Example.controls
            [ Join GotJoin (not <| Phoenix.channelJoined "example:join_and_leave_channels" phoenix)
            , Leave GotLeave (Phoenix.channelJoined "example:join_and_leave_channels" phoenix)
            ]
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.setJoinConfig"
            , "Phoenix.join"
            , "Phoenix.leave"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.isConnected", Phoenix.isConnected phoenix |> String.printBool )
            , ( "Phoenix.channelJoined", Phoenix.channelJoined "example:join_and_leave_channels" phoenix |> String.printBool )
            , ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList )
            ]
        |> Example.view device
