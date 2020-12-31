module Example.JoinMultipleChannels exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Element as El exposing (Device, Element)
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
    = GotJoin
    | GotLeave
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJoin ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.batch
                    [ Phoenix.join "example:join_channel_number_1"
                    , Phoenix.join "example:join_channel_number_2"
                    , Phoenix.join "example:join_channel_number_3"
                    ]
                    model.phoenix

        GotLeave ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.batch
                    [ Phoenix.leave "example:join_channel_number_1"
                    , Phoenix.leave "example:join_channel_number_2"
                    , Phoenix.leave "example:join_channel_number_3"
                    ]
                    model.phoenix

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
    Example.init JoinMultipleChannels
        |> Example.description
            [ [ El.text "Join multiple Channels with a single command." ] ]
        |> Example.controls
            [ Join GotJoin ((Phoenix.joinedChannels phoenix |> List.length) == 0)
            , Leave GotLeave ((Phoenix.joinedChannels phoenix |> List.length) == 3)
            ]
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.batch"
            , "Phoenix.join"
            , "Phoenix.leave"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.isConnected", Phoenix.isConnected phoenix |> String.printBool )
            , ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList )
            ]
        |> Example.view device
