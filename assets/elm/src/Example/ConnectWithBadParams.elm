module Example.ConnectWithBadParams exposing
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
import Phoenix exposing (PhoenixMsg(..), SocketMessage(..))
import Type.Example exposing (Example(..))
import Utils exposing (updatePhoenixWith)
import View.Example as Example exposing (Control(..), Response(..))



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
    = GotConnect
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotConnect ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.connect <|
                    Phoenix.setConnectParams
                        (JE.object
                            [ ( "good_params", JE.bool False ) ]
                        )
                        model.phoenix

        PhoenixMsg subMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                SocketMessage (SocketError error) ->
                    ( { newModel | responses = Socket (SocketError error) :: newModel.responses }, cmd )

                SocketMessage (StateChange state) ->
                    ( { newModel | responses = Socket (StateChange state) :: newModel.responses }, cmd )

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
    Example.init ConnectWithBadParams
        |> Example.description
            [ [ El.text "Try to connect to the Socket with authentication params that are not accepted, causing the connection to be denied." ] ]
        |> Example.controls
            [ Connect GotConnect (not <| Phoenix.isConnected phoenix) ]
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.setConnectParams"
            , "Phoenix.connect"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.socketState", Phoenix.socketStateToString phoenix )
            , ( "Phoenix.connectionState", Phoenix.connectionState phoenix |> String.printQuoted )
            , ( "Phoenix.isConnected", Phoenix.isConnected phoenix |> String.printBool )
            ]
        |> Example.view device
