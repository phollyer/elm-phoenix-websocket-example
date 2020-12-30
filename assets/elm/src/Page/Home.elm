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

import Element as El exposing (Device, DeviceClass(..), Element, Orientation(..))
import Element.Font as Font
import Phoenix
import Route exposing (Route(..))
import Session exposing (Session)
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import UI.FontSize as FontSize
import UI.Padding as Padding
import View.Layout as Layout
import View.Panel as Panel



{- Types -}


type alias Model =
    Session



{- Build -}


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


updateSession : Session -> Model
updateSession session =
    session



{- Update -}


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



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PhoenixMsg <|
        Phoenix.subscriptions
            (Session.phoenix model)



{- View -}


view : Device -> { title : String, content : Element Msg }
view device =
    { title = "Home"
    , content =
        Layout.init
            |> Layout.title "Elm-Phoenix-WebSocket Examples"
            |> Layout.body
                (El.column
                    [ El.spacing 20
                    , El.width El.fill
                    ]
                    [ introductionView device
                    , examplesContainer device
                        [ exampleContainer device "Socket Examples" <|
                            [ container
                                (Panel.init
                                    |> Panel.title "Control the Connection (3)"
                                    |> Panel.description
                                        [ [ El.text "Manually connect and disconnect, receiving feedback on the current state of the Socket." ]
                                        , [ El.text "Connect with good params that are accepted, and bad params that cause the request to be denied." ]
                                        ]
                                    |> Panel.onClick (Just (NavigateTo ControlTheSocketConnection))
                                    |> Panel.view device
                                )
                            ]
                        , exampleContainer device "Channels Examples" <|
                            [ container
                                (Panel.init
                                    |> Panel.title "Joining and Leaving (4)"
                                    |> Panel.description
                                        [ [ El.text "Manually join and leave a Channel." ]
                                        , [ El.text "Join with good and bad params, and multiple Channels at once." ]
                                        ]
                                    |> Panel.onClick (Just (NavigateTo JoinAndLeaveChannels))
                                    |> Panel.view device
                                )
                            , container
                                (Panel.init
                                    |> Panel.title "Sending and Receiving (4)"
                                    |> Panel.description
                                        [ [ El.text "Send and receive events." ]
                                        , [ El.text "Handle a Push that results in a timeout. Choose the retry strategy and see the countdown until the next attempt." ]
                                        ]
                                    |> Panel.onClick (Just (NavigateTo SendAndReceive))
                                    |> Panel.view device
                                )
                            ]
                        , exampleContainer device "Presence Example" <|
                            [ container
                                (Panel.init
                                    |> Panel.title "Multi-Room Chat"
                                    |> Panel.description
                                        [ [ El.text "Create, delete and enter multiple rooms. Chat in each of them." ] ]
                                    |> Panel.onClick (Just (NavigateTo ChatRooms))
                                    |> Panel.view device
                                )
                            ]
                        ]
                    ]
                )
            |> Layout.view device
    }



{- Introduction -}


introductionView : Device -> Element msg
introductionView ({ class, orientation } as device) =
    El.column
        [ El.width <|
            case ( class, orientation ) of
                ( Phone, Portrait ) ->
                    El.fill

                ( Phone, Landscape ) ->
                    El.maximum 400 El.fill

                _ ->
                    El.maximum 800 El.fill
        , El.centerX
        , El.spacing 10
        , Font.center
        , FontFamily.default
        , FontSize.default device
        ]
        [ El.paragraph
            [ El.width El.fill ]
            [ El.text "Welcome, please try the examples below. " ]
        , El.paragraph
            [ El.width El.fill ]
            [ El.text "The Socket and Channels examples provide details of the functions used, useful functions and links to docs." ]
        ]



{- Containers -}


examplesContainer : Device -> List (Element msg) -> Element msg
examplesContainer { class, orientation } examples =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            El.column
                [ El.width El.fill
                , El.spacing 20
                , Padding.bottom 10
                ]
                examples

        ( Phone, Landscape ) ->
            El.column
                [ El.centerX
                , El.spacing 20
                , Padding.bottom 10
                ]
                examples

        ( Tablet, _ ) ->
            El.column
                [ El.centerX
                , El.spacing 30
                , Padding.bottom 20
                ]
                examples

        _ ->
            El.row
                [ El.centerX
                , El.spacing 40
                ]
                examples


exampleContainer : Device -> String -> List (Element msg) -> Element msg
exampleContainer device title panels =
    El.column
        [ El.alignTop
        , El.centerX
        , El.spacing 10
        ]
        [ El.el
            [ El.centerX
            , FontColor.title
            , FontSize.title device
            ]
            (El.text title)
        , panelsContainer device panels
        ]


panelsContainer : Device -> List (Element msg) -> Element msg
panelsContainer { class, orientation } =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            El.column
                [ El.spacing 10
                , El.width El.fill
                ]

        ( Phone, Landscape ) ->
            El.wrappedRow
                [ El.spacing 10
                , El.width El.fill
                ]

        _ ->
            El.wrappedRow
                [ El.centerX
                , El.spacing 10
                ]


container : Element Msg -> Element Msg
container =
    El.el
        [ El.height <|
            El.maximum 300 El.fill
        , El.width <|
            El.maximum 250 El.fill
        ]
