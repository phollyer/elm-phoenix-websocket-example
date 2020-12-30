module Page.ControlTheSocketConnection exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , updateSession
    , view
    )

import Element as El exposing (Device, DeviceClass(..), Element, Orientation(..))
import Example.ConnectWithBadParams as ConnectWithBadParams
import Example.ConnectWithGoodParams as ConnectWithGoodParams
import Example.SimpleConnect as SimpleConnect
import Phoenix
import Route
import Session exposing (Session)
import Type.Group as Group
import Utils exposing (updatePhoenixSessionWith)
import View.Example.Menu as Menu
import View.Example.Page as ExamplePage
import View.Layout as Layout



{- Init -}


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , example =
            SimpleConnect <|
                SimpleConnect.init
                    (Session.phoenix session)
      }
    , Cmd.none
    )



{- Model -}


type alias Model =
    { session : Session
    , example : Example
    }


type Example
    = SimpleConnect SimpleConnect.Model
    | ConnectWithGoodParams ConnectWithGoodParams.Model
    | ConnectWithBadParams ConnectWithBadParams.Model



{- Update -}


type Msg
    = GotHomeBtnClick
    | GotMenuItem String
    | GotSimpleConnectMsg SimpleConnect.Msg
    | GotConnectWithGoodParamsMsg ConnectWithGoodParams.Msg
    | GotConnectWithBadParamsMsg ConnectWithBadParams.Msg
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        phoenix =
            Session.phoenix model.session
    in
    case ( msg, model.example ) of
        ( GotHomeBtnClick, _ ) ->
            ( model
            , Route.pushUrl
                (Session.navKey model.session)
                Route.Home
            )

        ( GotMenuItem example_, _ ) ->
            Phoenix.disconnect (Just 1000) phoenix
                |> updatePhoenix model
                |> updateExample example_

        ( PhoenixMsg subMsg, _ ) ->
            Phoenix.update subMsg phoenix
                |> updatePhoenixSessionWith PhoenixMsg model

        ( GotSimpleConnectMsg subMsg, SimpleConnect subModel ) ->
            SimpleConnect.update subMsg subModel
                |> updateWith SimpleConnect GotSimpleConnectMsg model

        ( GotConnectWithGoodParamsMsg subMsg, ConnectWithGoodParams subModel ) ->
            ConnectWithGoodParams.update subMsg subModel
                |> updateWith ConnectWithGoodParams GotConnectWithGoodParamsMsg model

        ( GotConnectWithBadParamsMsg subMsg, ConnectWithBadParams subModel ) ->
            ConnectWithBadParams.update subMsg subModel
                |> updateWith ConnectWithBadParams GotConnectWithBadParamsMsg model

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> Example) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toExample toMsg model ( subModel, cmd ) =
    ( { model | example = toExample subModel }
    , Cmd.map toMsg cmd
    )


updatePhoenix : Model -> ( Phoenix.Model, Cmd Phoenix.Msg ) -> ( Model, Cmd Msg )
updatePhoenix model ( phoenix, phoenixCmd ) =
    ( { model
        | session = Session.updatePhoenix phoenix model.session
      }
    , Cmd.map PhoenixMsg phoenixCmd
    )


updateExample : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateExample selectedExample ( model, cmd ) =
    let
        example_ =
            case selectedExample of
                "Simple Connect" ->
                    SimpleConnect <|
                        SimpleConnect.init
                            (Session.phoenix model.session)

                "Connect With Good Params" ->
                    ConnectWithGoodParams <|
                        ConnectWithGoodParams.init
                            (Session.phoenix model.session)

                "Connect With Bad Params" ->
                    ConnectWithBadParams <|
                        ConnectWithBadParams.init
                            (Session.phoenix model.session)

                _ ->
                    SimpleConnect <|
                        SimpleConnect.init
                            (Session.phoenix model.session)
    in
    ( { model
        | example = example_
      }
    , cmd
    )



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        exampleSub =
            case model.example of
                SimpleConnect subModel ->
                    Sub.map GotSimpleConnectMsg <|
                        SimpleConnect.subscriptions subModel

                ConnectWithGoodParams subModel ->
                    Sub.map GotConnectWithGoodParamsMsg <|
                        ConnectWithGoodParams.subscriptions subModel

                ConnectWithBadParams subModel ->
                    Sub.map GotConnectWithBadParamsMsg <|
                        ConnectWithBadParams.subscriptions subModel
    in
    Sub.batch
        [ exampleSub
        , Sub.map PhoenixMsg <|
            Phoenix.subscriptions (Session.phoenix model.session)
        ]



{- Session -}


toSession : Model -> Session
toSession model =
    model.session


updateSession : Session -> Model -> Model
updateSession session model =
    { model | session = session }



{- View -}


view : Model -> { title : String, content : Element Msg }
view model =
    let
        device =
            Session.device model.session
    in
    { title = "Control The Socket Connection"
    , content =
        Layout.init
            |> Layout.homeMsg (Just GotHomeBtnClick)
            |> Layout.title "Control The Socket Connection"
            |> Layout.body
                (ExamplePage.init
                    |> ExamplePage.introduction introduction
                    |> ExamplePage.menu (menu device model)
                    |> ExamplePage.example (viewExample device model)
                    |> ExamplePage.view device
                )
            |> Layout.view device
    }


{-| Introduction
-}
introduction : List (List (Element Msg))
introduction =
    [ [ El.text "Connecting to the Socket is taken care of automatically when a request to join a Channel is made, or when a Channel is pushed to. "
      , El.text "However, if you want to take manual control, here's a few examples."
      ]
    ]


{-| Examples Menu
-}
menu : Device -> Model -> Element Msg
menu device { example } =
    let
        selected =
            case example of
                SimpleConnect _ ->
                    "Simple Connect"

                ConnectWithGoodParams _ ->
                    "Connect With Good Params"

                ConnectWithBadParams _ ->
                    "Connect With Bad Params"
    in
    Menu.init
        |> Menu.options
            [ "Simple Connect"
            , "Connect With Good Params"
            , "Connect With Bad Params"
            ]
        |> Menu.selected selected
        |> Menu.onClick (Just GotMenuItem)
        |> Menu.group
            (Group.init
                |> Group.layouts
                    [ ( Phone, Landscape, [ 1, 2 ] )
                    , ( Tablet, Portrait, [ 1, 2 ] )
                    ]
            )
        |> Menu.view device



{- Example -}


viewExample : Device -> Model -> Element Msg
viewExample device { example } =
    case example of
        SimpleConnect subModel ->
            SimpleConnect.view device subModel
                |> El.map GotSimpleConnectMsg

        ConnectWithGoodParams subModel ->
            ConnectWithGoodParams.view device subModel
                |> El.map GotConnectWithGoodParamsMsg

        ConnectWithBadParams subModel ->
            ConnectWithBadParams.view device subModel
                |> El.map GotConnectWithBadParamsMsg
