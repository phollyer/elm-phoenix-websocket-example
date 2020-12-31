module Example.JoinWithBadParams exposing
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
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), joinConfig)
import Type.Example exposing (Example(..))
import UI.Link as Link
import Utils exposing (batch, updatePhoenixWith)
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
                                    [ ( "username", JE.string "bad" )
                                    , ( "password", JE.string "bad" )
                                    ]
                        }
                        model.phoenix

        PhoenixMsg subMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                ChannelResponse response ->
                    case response of
                        JoinError _ _ ->
                            -- Leave the Channel after a JoinError to stop
                            -- PhoenixJS from constantly retrying to join
                            Phoenix.leave "example:join_and_leave_channels" newModel.phoenix
                                |> updatePhoenixWith PhoenixMsg
                                    { newModel | responses = Channel response :: newModel.responses }
                                |> batch [ cmd ]

                        _ ->
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
    Example.init JoinWithBadParams
        |> Example.description
            [ [ El.text "Join a Channel, providing auth params that are not accepted. " ]
            , [ El.text "After we receive a "
              , Link.type_ ( "ChannelResponse", "JoinError" )
              , El.text ", we then call "
              , Link.function "Phoenix.leave"
              , El.text " to stop PhoenixJS from retrying to join with bad params."
              ]
            ]
        |> Example.controls
            [ Join GotJoin True ]
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
