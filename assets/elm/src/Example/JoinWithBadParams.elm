module Example.JoinWithBadParams exposing
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
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..))
import Utils exposing (batch, updatePhoenixWith)
import View.Example as Example exposing (Response(..))



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
    = GotControlClick
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotControlClick ->
            model.phoenix
                |> Phoenix.setJoinConfig
                    { joinConfig
                        | topic = "example:join_and_leave_channels"
                        , payload =
                            JE.object
                                [ ( "username", JE.string "bad" )
                                , ( "password", JE.string "bad" )
                                ]
                    }
                |> Phoenix.join "example:join_and_leave_channels"
                |> updatePhoenixWith PhoenixMsg model

        PhoenixMsg subMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                ChannelResponse response ->
                    case response of
                        Phoenix.JoinError _ _ ->
                            -- Leave the Channel after a JoinError to stop
                            -- PhoenixJS from constantly retrying
                            Phoenix.leave "example:join_and_leave_channels" newModel.phoenix
                                |> updatePhoenixWith PhoenixMsg
                                    { newModel | responses = Channel response :: newModel.responses }
                                |> batch [ cmd ]

                        _ ->
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
            [ [ El.text "Join a Channel, providing auth params that are not accepted." ] ]
        |> Example.controls
            [ Example.Join GotControlClick True ]
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.setJoinConfig"
            , "Phoenix.join"
            , "Phoenix.leave"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.channelJoined", Phoenix.channelJoined "example:join_and_leave_channels" phoenix |> String.printBool )
            , ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList )
            ]
        |> Example.view device
