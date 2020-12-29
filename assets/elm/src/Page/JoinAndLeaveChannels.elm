module Page.JoinAndLeaveChannels exposing
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
import Example.JoinMultipleChannels as JoinMultipleChannels
import Example.JoinWithBadParams as JoinWithBadParams
import Example.JoinWithGoodParams as JoinWithGoodParams
import Example.SimpleJoinAndLeave as SimpleJoinAndLeave
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
            SimpleJoinAndLeave <|
                SimpleJoinAndLeave.init
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
    = SimpleJoinAndLeave SimpleJoinAndLeave.Model
    | JoinWithGoodParams JoinWithGoodParams.Model
    | JoinWithBadParams JoinWithBadParams.Model
    | JoinMultipleChannels JoinMultipleChannels.Model



{- Update -}


type Msg
    = GotHomeBtnClick
    | GotMenuItem String
    | GotSimpleJoinAndLeaveMsg SimpleJoinAndLeave.Msg
    | GotJoinWithGoodParamsMsg JoinWithGoodParams.Msg
    | GotJoinWithBadParamsMsg JoinWithBadParams.Msg
    | GotJoinMultipleChannelsMsg JoinMultipleChannels.Msg
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
            Phoenix.disconnectAndReset Nothing phoenix
                |> updatePhoenix model
                |> updateExample example_

        ( PhoenixMsg subMsg, _ ) ->
            Phoenix.update subMsg phoenix
                |> updatePhoenixSessionWith PhoenixMsg model

        ( GotSimpleJoinAndLeaveMsg subMsg, SimpleJoinAndLeave subModel ) ->
            SimpleJoinAndLeave.update subMsg subModel
                |> updateWith SimpleJoinAndLeave GotSimpleJoinAndLeaveMsg model

        ( GotJoinWithGoodParamsMsg subMsg, JoinWithGoodParams subModel ) ->
            JoinWithGoodParams.update subMsg subModel
                |> updateWith JoinWithGoodParams GotJoinWithGoodParamsMsg model

        ( GotJoinWithBadParamsMsg subMsg, JoinWithBadParams subModel ) ->
            JoinWithBadParams.update subMsg subModel
                |> updateWith JoinWithBadParams GotJoinWithBadParamsMsg model

        ( GotJoinMultipleChannelsMsg subMsg, JoinMultipleChannels subModel ) ->
            JoinMultipleChannels.update subMsg subModel
                |> updateWith JoinMultipleChannels GotJoinMultipleChannelsMsg model

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
        phoenix =
            Session.phoenix model.session
    in
    ( { model
        | example =
            case selectedExample of
                "Simple Join And Leave" ->
                    SimpleJoinAndLeave <|
                        SimpleJoinAndLeave.init phoenix

                "Join With Good Params" ->
                    JoinWithGoodParams <|
                        JoinWithGoodParams.init phoenix

                "Join With Bad Params" ->
                    JoinWithBadParams <|
                        JoinWithBadParams.init phoenix

                "Join Multiple Channels" ->
                    JoinMultipleChannels <|
                        JoinMultipleChannels.init phoenix

                _ ->
                    SimpleJoinAndLeave <|
                        SimpleJoinAndLeave.init phoenix
      }
    , cmd
    )



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        exampleSub =
            case model.example of
                SimpleJoinAndLeave subModel ->
                    Sub.map GotSimpleJoinAndLeaveMsg <|
                        SimpleJoinAndLeave.subscriptions subModel

                JoinWithGoodParams subModel ->
                    Sub.map GotJoinWithGoodParamsMsg <|
                        JoinWithGoodParams.subscriptions subModel

                JoinWithBadParams subModel ->
                    Sub.map GotJoinWithBadParamsMsg <|
                        JoinWithBadParams.subscriptions subModel

                JoinMultipleChannels subModel ->
                    Sub.map GotJoinMultipleChannelsMsg <|
                        JoinMultipleChannels.subscriptions subModel
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
    { title = "Join And Leave Channels"
    , content =
        Layout.init
            |> Layout.homeMsg (Just GotHomeBtnClick)
            |> Layout.title "Join And Leave Channels"
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
    [ [ El.text "When a Join is requested, the Socket will connect automatically if it is required. "
      ]
    , [ El.text "Clicking on a function will take you to its documentation." ]
    ]


{-| Examples Menu
-}
menu : Device -> Model -> Element Msg
menu device { example } =
    let
        selected =
            case example of
                SimpleJoinAndLeave _ ->
                    "Simple Join And Leave"

                JoinWithGoodParams _ ->
                    "Join With Good Params"

                JoinWithBadParams _ ->
                    "Join With Bad Params"

                JoinMultipleChannels _ ->
                    "Join Multiple Channels"
    in
    Menu.init
        |> Menu.options
            [ "Simple Join And Leave"
            , "Join With Good Params"
            , "Join With Bad Params"
            , "Join Multiple Channels"
            ]
        |> Menu.selected selected
        |> Menu.onClick (Just GotMenuItem)
        |> Menu.group
            (Group.init
                |> Group.layouts
                    [ ( Phone, Landscape, [ 1, 2, 1 ] )
                    , ( Tablet, Portrait, [ 1, 2, 1 ] )
                    ]
            )
        |> Menu.view device



{- Example -}


viewExample : Device -> Model -> Element Msg
viewExample device { example } =
    case example of
        SimpleJoinAndLeave subModel ->
            SimpleJoinAndLeave.view device subModel
                |> El.map GotSimpleJoinAndLeaveMsg

        JoinWithGoodParams subModel ->
            JoinWithGoodParams.view device subModel
                |> El.map GotJoinWithGoodParamsMsg

        JoinWithBadParams subModel ->
            JoinWithBadParams.view device subModel
                |> El.map GotJoinWithBadParamsMsg

        JoinMultipleChannels subModel ->
            JoinMultipleChannels.view device subModel
                |> El.map GotJoinMultipleChannelsMsg
