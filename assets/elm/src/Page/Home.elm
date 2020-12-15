module Page.Home exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , updateSession
    , view
    )

import Element as El exposing (Device, Element)
import Phoenix
import Route exposing (Route(..))
import Session exposing (Session)
import View.Home as Home
import View.Layout as Layout
import View.Panel as Panel


init : Session -> ( Model, Cmd Msg )
init session =
    let
        ( phx, phxCmd ) =
            Phoenix.disconnectAndReset Nothing <|
                Session.phoenix session
    in
    ( Session.updatePhoenix phx session
    , Cmd.map PhoenixMsg phxCmd
    )


type alias Model =
    Session


type Msg
    = PhoenixMsg Phoenix.Msg
    | NavigateTo Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PhoenixMsg phoenixMsg ->
            let
                ( phx, phxCmd, _ ) =
                    Phoenix.update phoenixMsg <|
                        Session.phoenix model
            in
            ( Session.updatePhoenix phx model
            , Cmd.map PhoenixMsg phxCmd
            )

        NavigateTo route ->
            ( model
            , Route.pushUrl (Session.navKey model) route
            )


toSession : Model -> Session
toSession model =
    model


updateSession : Session -> Model
updateSession session =
    session


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PhoenixMsg <|
        Phoenix.subscriptions
            (Session.phoenix model)


view : Device -> { title : String, content : Element Msg }
view device =
    { title = "Home"
    , content =
        Layout.init
            |> Layout.title "Elm-Phoenix-WebSocket Examples"
            |> Layout.body
                (Home.init
                    |> Home.introduction introduction
                    |> Home.socket (socketExamples device)
                    |> Home.channels (channelsExamples device)
                    |> Home.presence (presenceExamples device)
                    |> Home.view device
                )
            |> Layout.view device
    }


introduction : List (List (Element Msg))
introduction =
    [ [ El.text "Welcome, please try the examples below. " ]
    , [ El.text "The Socket and Channels examples provide details of the functions used, useful functions and links to docs."
      ]
    ]


socketExamples : Device -> List (Element Msg)
socketExamples device =
    [ Panel.init
        |> Panel.title "Control the Connection (3)"
        |> Panel.description
            [ [ El.text "Manually connect and disconnect, receiving feedback on the current state of the Socket." ]
            , [ El.text "Connect with good params that are accepted, and bad params that cause the request to be deined." ]
            ]
        |> Panel.onClick (Just (NavigateTo ControlTheSocketConnection))
        |> Panel.view device
    ]


channelsExamples : Device -> List (Element Msg)
channelsExamples device =
    [ Panel.init
        |> Panel.title "Joining and Leaving (4)"
        |> Panel.description
            [ [ El.text "Manually join and leave a Channel." ]
            , [ El.text "Join with good and bad params, and multiple Channels at once." ]
            ]
        |> Panel.onClick (Just (NavigateTo JoinAndLeaveChannels))
        |> Panel.view device
    , Panel.init
        |> Panel.title "Sending and Receiving (4)"
        |> Panel.description
            [ [ El.text "Send and receive events." ]
            , [ El.text "Handle a Push that results in a timeout. Choose the retry strategy and see the countdown until the next attempt." ]
            ]
        |> Panel.onClick (Just (NavigateTo SendAndReceive))
        |> Panel.view device
    ]


presenceExamples : Device -> List (Element Msg)
presenceExamples device =
    [ Panel.init
        |> Panel.title "Multi-Room Chat"
        |> Panel.description
            [ [ El.text "Create, delete and enter multiple rooms. Chat in each of them." ] ]
        |> Panel.onClick (Just (NavigateTo ChatRooms))
        |> Panel.view device
    ]
