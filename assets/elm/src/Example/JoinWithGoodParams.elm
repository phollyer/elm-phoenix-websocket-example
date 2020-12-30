module Example.JoinWithGoodParams exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Configs exposing (joinConfig)
import Element as El exposing (Device, Element)
import Extra.String as String
import Json.Encode as JE
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), SocketMessage(..))
import Type.Example exposing (Example(..))
import Utils exposing (updatePhoenixWith)
import View.Example as Example exposing (Response(..))



{- Types -}


type alias Model =
    { phoenix : Phoenix.Model
    , responses : List Response
    }


type Action
    = Join
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
                Join ->
                    model.phoenix
                        |> Phoenix.setJoinConfig
                            { joinConfig
                                | topic = "example:join_and_leave_channels"
                                , payload =
                                    JE.object
                                        [ ( "username", JE.string "good" )
                                        , ( "password", JE.string "good" )
                                        ]
                            }
                        |> Phoenix.join "example:join_and_leave_channels"
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
            [ Example.Join (GotControlClick Join) (not <| Phoenix.channelJoined "example:join_and_leave_channels" phoenix)
            , Example.Leave (GotControlClick Leave) (Phoenix.channelJoined "example:join_and_leave_channels" phoenix)
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
