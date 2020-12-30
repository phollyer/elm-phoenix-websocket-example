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
                    let
                        joins =
                            List.range 0 3
                                |> List.map
                                    (\index -> Phoenix.join ("example:join_channel_number_" ++ String.fromInt index))
                    in
                    Phoenix.batch joins model.phoenix
                        |> updatePhoenixWith PhoenixMsg model

                Leave ->
                    let
                        leaves =
                            List.range 0 3
                                |> List.map
                                    (\index -> Phoenix.leave ("example:join_channel_number_" ++ String.fromInt index))
                    in
                    Phoenix.batch leaves model.phoenix
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
            [ Example.Join (GotControlClick Join) ((Phoenix.joinedChannels phoenix |> List.length) == 0)
            , Example.Leave (GotControlClick Leave) ((Phoenix.joinedChannels phoenix |> List.length) > 0)
            ]
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.batch"
            , "Phoenix.join"
            , "Phoenix.leave"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.joinedChannels", Phoenix.joinedChannels phoenix |> String.printList ) ]
        |> Example.view device
