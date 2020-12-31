module Example.ConnectWithGoodParams exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Element as El exposing (Device, DeviceClass(..), Element, Orientation(..))
import Extra.String as String
import Json.Encode as JE
import Phoenix exposing (PhoenixMsg(..), SocketMessage(..))
import Type.Example exposing (Example(..))
import Type.Group as Group
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
    = GotConnect
    | GotDisconnect
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotConnect ->
            updatePhoenixWith PhoenixMsg model <|
                Phoenix.connect <|
                    Phoenix.setConnectParams
                        (JE.object
                            [ ( "good_params", JE.bool True ) ]
                        )
                        model.phoenix

        GotDisconnect ->
            Phoenix.disconnect Nothing model.phoenix
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
    Example.init ConnectWithGoodParams
        |> Example.description
            [ [ El.text "Connect to the Socket with authentication params that are accepted." ] ]
        |> Example.controls
            [ Connect GotConnect (not <| Phoenix.isConnected phoenix)
            , Disconnect GotDisconnect (Phoenix.isConnected phoenix)
            ]
        |> Example.controlsGroup
            (Group.init
                |> Group.layouts
                    [ ( Phone, Portrait, [ 2 ] ) ]
            )
        |> Example.responses responses
        |> Example.applicableFunctions
            [ "Phoenix.setConnectParams"
            , "Phoenix.connect"
            , "Phoenix.disconnect"
            ]
        |> Example.usefulFunctions
            [ ( "Phoenix.socketState", Phoenix.socketStateToString phoenix )
            , ( "Phoenix.connectionState", Phoenix.connectionState phoenix |> String.printQuoted )
            , ( "Phoenix.isConnected", Phoenix.isConnected phoenix |> String.printBool )
            ]
        |> Example.view device
