module View.Example.Example exposing
    ( Config
    , Control(..)
    , Response(..)
    , applicableFunctions
    , controls
    , controlsGroup
    , description
    , feedback
    , init
    , responses
    , status
    , subControls
    , usefulFunctions
    , view
    )

import Colors.Opaque as Color
import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Border as Border
import Element.Font as Font
import Json.Encode as JE exposing (Value)
import List.Extra as List
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), SocketMessage(..))
import Type.Example exposing (Example)
import Type.Group as Group exposing (Group)
import UI.BackgroundColor as BackgroundColor
import UI.BorderColor as BorderColor
import UI.BorderWidth as BorderWidth
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import UI.FontSize as FontSize
import UI.Link as Link
import UI.Padding as Padding
import View.Button as Button



{- Types -}


type Config msg
    = Config
        { example : Example
        , description : List (List (Element msg))
        , controls : List (Control msg)
        , controlsGroup : Group
        , subControls : Element msg
        , status : Element msg
        , responses : List Response
        , applicableFunctions : List String
        , usefulFunctions : List ( String, String )
        , feedback : Element msg
        }


type Control msg
    = Connect msg Bool
    | Disconnect msg Bool
    | Join msg Bool
    | Leave msg Bool
    | Push msg Bool
    | CancelRetry msg Bool


type Response
    = Socket SocketMessage
    | Channel ChannelResponse
    | Event
        { topic : String
        , event : String
        , payload : Value
        }



{- Build -}


init : Example -> Config msg
init example =
    Config
        { example = example
        , description = []
        , controls = []
        , controlsGroup = Group.init
        , subControls = El.none
        , status = El.none
        , responses = []
        , applicableFunctions = []
        , usefulFunctions = []
        , feedback = El.none
        }



-- Description --


description : List (List (Element msg)) -> Config msg -> Config msg
description desc (Config config) =
    Config { config | description = desc }



-- Controls --


controls : List (Control msg) -> Config msg -> Config msg
controls cntrls (Config config) =
    Config { config | controls = cntrls }


controlsGroup : Group -> Config msg -> Config msg
controlsGroup group (Config config) =
    Config { config | controlsGroup = group }


subControls : Element msg -> Config msg -> Config msg
subControls subCntrls (Config config) =
    Config { config | subControls = subCntrls }



-- Status --


status : Element msg -> Config msg -> Config msg
status el (Config config) =
    Config { config | status = el }



-- Responses --


responses : List Response -> Config msg -> Config msg
responses list (Config config) =
    Config { config | responses = list }


feedback : Element msg -> Config msg -> Config msg
feedback feedback_ (Config config) =
    Config { config | feedback = feedback_ }



-- Applicable Functions --


applicableFunctions : List String -> Config msg -> Config msg
applicableFunctions functions (Config config) =
    Config { config | applicableFunctions = functions }



-- Useful Functions --


usefulFunctions : List ( String, String ) -> Config msg -> Config msg
usefulFunctions functions (Config config) =
    Config { config | usefulFunctions = functions }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.column
        [ El.spacing 10
        , El.height El.fill
        , El.width El.fill
        , FontColor.default
        , FontFamily.default
        , FontSize.default device
        , Padding.bottom 10
        ]
        [ descriptionView device config.example config.description
        , controlsView device (Config config)
        , feedbackView device (Config config)
        ]



{- Description -}


descriptionView : Device -> Example -> List (List (Element msg)) -> Element msg
descriptionView device example paragraphs =
    El.column
        [ El.width El.fill
        , Font.justify
        , spacing device.class
        ]
    <|
        List.append
            (List.map toParagraph paragraphs)
            [ El.paragraph
                [ El.width El.fill
                , FontSize.small device
                ]
                [ El.text "The main source code for this example can be found "
                , Link.srcLink example
                , El.text ". Clicking on a function will take you to its documentation."
                ]
            ]


toParagraph : List (Element msg) -> Element msg
toParagraph paragraph =
    El.paragraph
        [ El.width El.fill ]
        paragraph



{- Controls -}


controlsView : Device -> Config msg -> Element msg
controlsView device (Config config) =
    let
        elements =
            List.map (toButton device) config.controls
    in
    case Group.layoutForDevice device config.controlsGroup of
        Nothing ->
            column
                [ El.column
                    [ El.width El.fill
                    , El.spacing 20
                    ]
                    [ config.subControls
                    , toRow <|
                        Group.orderForDevice device elements config.controlsGroup
                    ]
                ]

        Just layout ->
            column
                [ El.column
                    [ El.width El.fill
                    , El.spacing 20
                    ]
                    [ config.subControls
                    , El.column
                        [ El.width El.fill
                        , El.spacing 10
                        ]
                      <|
                        toRows layout <|
                            Group.orderForDevice device elements config.controlsGroup
                    ]
                ]


toButton : Device -> Control msg -> Element msg
toButton device control =
    case control of
        Connect msg enabled ->
            Button.init
                |> Button.setLabel "Connect"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device

        Disconnect msg enabled ->
            Button.init
                |> Button.setLabel "Disconnect"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device

        Join msg enabled ->
            Button.init
                |> Button.setLabel "Join"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device

        Leave msg enabled ->
            Button.init
                |> Button.setLabel "Leave"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device

        Push msg enabled ->
            Button.init
                |> Button.setLabel "Push"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device

        CancelRetry msg enabled ->
            Button.init
                |> Button.setLabel "Cancel Retry"
                |> Button.setOnPress (Just msg)
                |> Button.setEnabled enabled
                |> Button.view device


column : List (Element msg) -> Element msg
column =
    El.column
        [ Border.widthXY 0 1
        , BorderColor.seperatorLight
        , El.paddingXY 0 10
        , El.spacing 10
        , El.width El.fill
        ]


toRows : List Int -> List (Element msg) -> List (Element msg)
toRows layout elements_ =
    List.groupsOfVarying layout elements_
        |> List.map
            (\elements__ ->
                El.wrappedRow
                    [ El.spacing 10
                    , El.centerX
                    ]
                    [ toRow elements__ ]
            )


toRow : List (Element msg) -> Element msg
toRow elements_ =
    El.row
        [ El.width El.fill
        , El.spacing 10
        ]
        elements_



{- Feedback -}


feedbackView : Device -> Config msg -> Element msg
feedbackView ({ class, orientation } as device) (Config config) =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            El.column
                [ El.height El.fill
                , El.width El.fill
                , El.spacing 20
                ]
                [ infoView device config.status config.responses
                , applicableFunctionsView device config.applicableFunctions
                , usefulFunctionsView device config.usefulFunctions
                ]

        ( Phone, Landscape ) ->
            El.column
                [ El.height El.fill
                , El.centerX
                , El.spacing 10
                ]
                [ El.row
                    [ El.spacing 10 ]
                    [ infoView device config.status config.responses
                    , applicableFunctionsView device config.applicableFunctions
                    ]
                , usefulFunctionsView device config.usefulFunctions
                ]

        ( Tablet, _ ) ->
            El.column
                [ El.height El.fill
                , El.centerX
                , El.spacing 10
                ]
                [ El.row
                    [ El.spacing 10 ]
                    [ infoView device config.status config.responses
                    , applicableFunctionsView device config.applicableFunctions
                    ]
                , usefulFunctionsView device config.usefulFunctions
                ]

        _ ->
            El.row
                [ El.height El.fill
                , El.centerX
                , El.spacing 20
                ]
                [ infoView device config.status config.responses
                , applicableFunctionsView device config.applicableFunctions
                , usefulFunctionsView device config.usefulFunctions
                ]


titleView : Device -> String -> Element msg
titleView device title_ =
    El.el
        [ BorderWidth.bottom 1
        , El.centerX
        , El.width El.fill
        , Font.bold
        , Font.center
        , FontColor.title
        , FontSize.panelHeader device
        ]
        (El.text title_)



-- Info View --


infoView : Device -> Element msg -> List Response -> Element msg
infoView device status_ responseList =
    panel device
        [ titleView device "Info"
        , statusView status_
        , responsesView device responseList
        ]



-- Status View --


statusView : Element msg -> Element msg
statusView status_ =
    if status_ == El.none then
        El.none

    else
        El.el
            [ BorderWidth.bottom 1
            , El.paddingXY 0 5
            , El.width El.fill
            ]
            status_



-- Responses -


responsesView : Device -> List Response -> Element msg
responsesView device responseList =
    case responseList of
        [] ->
            El.none

        _ ->
            El.column
                [ El.paddingXY 0 10
                , El.spacing 15
                , El.clipY
                , El.scrollbarY
                , El.height El.fill
                , El.width El.fill
                ]
                (List.map (toResponse device) responseList
                    |> List.filter (\r -> r /= El.none)
                    |> List.intersperse seperator
                )


toResponse : Device -> Response -> Element msg
toResponse device response =
    case response of
        Socket (StateChange state) ->
            socketResponse "StateChange" <|
                socketStateToString state

        Socket (SocketError error) ->
            socketResponse "SocketError" error

        Channel (JoinOk topic payload) ->
            channelResponse device "JoinOk" <|
                { channelInfo
                    | topic = topic
                    , payload = payload
                }

        Channel (JoinError topic payload) ->
            channelResponse device "JoinError" <|
                { channelInfo
                    | topic = topic
                    , payload = payload
                }

        Channel (LeaveOk topic) ->
            channelResponse device "LeaveOk" <|
                { channelInfo | topic = topic }

        Channel (PushOk topic event ref payload) ->
            channelResponse device "PushOk" <|
                { channelInfo
                    | topic = topic
                    , event = Just event
                    , ref = ref
                    , payload = payload
                }

        Channel (PushTimeout topic event ref payload) ->
            channelResponse device "PushTimeout" <|
                { channelInfo
                    | topic = topic
                    , event = Just event
                    , ref = ref
                    , payload = payload
                }

        Event event ->
            channelEvent device "" event

        _ ->
            El.none



-- Socket Response --


socketResponse : String -> String -> Element msg
socketResponse title response =
    El.column
        [ El.spacing 10
        , FontFamily.code
        ]
        [ El.el
            [ Font.color Color.darkslateblue
            , Font.bold
            ]
            (El.text "SocketMessage")
        , El.el
            [ El.alignTop
            , Font.color Color.darkslateblue
            , Font.bold
            ]
            (El.text title)
        , El.el
            [ El.width El.fill ]
            (El.text response)
        ]


socketStateToString : Phoenix.SocketState -> String
socketStateToString state =
    case state of
        Phoenix.Connecting ->
            "Connecting"

        Phoenix.Connected ->
            "Connected"

        Phoenix.Disconnecting ->
            "Disconnecting"

        Phoenix.Disconnected _ ->
            "Disconnected"


seperator : Element msg
seperator =
    El.el
        [ BorderColor.seperatorDark
        , BorderWidth.bottom 1
        , El.width El.fill
        ]
        El.none



-- ChannelResponse --


channelResponse : Device -> String -> ChannelInfo -> Element msg
channelResponse device title info =
    El.column
        [ El.spacing 10
        , FontFamily.code
        ]
        [ El.el
            [ Font.color Color.darkslateblue
            , Font.bold
            ]
            (El.text "ChannelResponse")
        , El.el
            [ El.alignTop
            , Font.color Color.darkslateblue
            , Font.bold
            ]
            (El.text title)
        , El.column
            [ El.alignLeft
            , El.spacing 10
            , El.width El.fill
            , FontFamily.code
            , Padding.bottom 10
            ]
            [ field device "Topic: " info.topic
            , maybe (field device) "Event: " info.event
            , field device "Payload: " (JE.encode 2 info.payload)
            , maybe (field device) "Join Ref: " info.joinRef
            , maybe (field device) "Ref: " info.ref
            ]
        ]


field : Device -> String -> String -> Element msg
field device label value =
    El.wrappedRow
        [ FontSize.panelContent device
        , spacing device.class
        , El.width El.fill
        ]
        [ El.el
            [ El.alignTop
            , FontColor.default
            ]
            (El.text label)
        , El.el
            [ FontColor.value ]
            (El.text value)
        ]


maybe : (String -> String -> Element msg) -> String -> Maybe String -> Element msg
maybe toField label maybeValue =
    case maybeValue of
        Just value ->
            toField label value

        Nothing ->
            El.none


type alias ChannelInfo =
    { topic : String
    , event : Maybe String
    , payload : Value
    , joinRef : Maybe String
    , ref : Maybe String
    }


channelInfo : ChannelInfo
channelInfo =
    { topic = ""
    , event = Nothing
    , payload = JE.null
    , joinRef = Nothing
    , ref = Nothing
    }



-- Channel Event --


channelEvent : Device -> String -> { topic : String, event : String, payload : Value } -> Element msg
channelEvent device title { topic, event, payload } =
    El.column
        [ El.spacing 10
        , FontFamily.code
        ]
        [ El.el
            [ Font.color Color.darkslateblue
            , Font.bold
            ]
            (El.text "ChannelEvent")
        , El.el
            [ El.alignTop
            , Font.color Color.darkslateblue
            , Font.bold
            ]
            (El.text title)
        , El.column
            [ El.alignLeft
            , El.spacing 10
            , El.width El.fill
            , FontFamily.code
            , Padding.bottom 10
            ]
            [ field device "Topic: " topic
            , field device "Event: " event
            , field device "Payload: " (JE.encode 2 payload)
            ]
        ]



-- Applicable Functions --


applicableFunctionsView : Device -> List String -> Element msg
applicableFunctionsView device functions =
    panel device
        [ titleView device "Applicable Functions"
        , El.column
            [ spacing device.class
            , El.width El.fill
            , Padding.top 10
            ]
            (List.map Link.function functions)
        ]



-- Useful Functions --


usefulFunctionsView : Device -> List ( String, String ) -> Element msg
usefulFunctionsView device functions =
    panel device
        [ titleView device "Useful Functions"
        , El.column
            [ spacing device.class
            , El.width El.fill
            , Padding.top 10
            ]
          <|
            El.wrappedRow
                [ El.width El.fill
                , Font.bold
                , FontColor.default
                ]
                [ El.el
                    []
                    (El.text "Function")
                , El.el
                    [ El.alignRight ]
                    (El.text "Current Value")
                ]
                :: toFunctions device functions
        ]


toFunctions : Device -> List ( String, String ) -> List (Element msg)
toFunctions ({ class } as device) functions =
    case class of
        Phone ->
            List.map (toFunction device) functions

        _ ->
            [ El.column
                [ El.spacing 16
                , El.width El.fill
                , El.height El.fill
                ]
                (List.map (toFunction device) functions)
            ]


toFunction : Device -> ( String, String ) -> Element msg
toFunction device ( function, currentValue ) =
    El.wrappedRow
        [ spacing device.class
        , El.width El.fill
        , El.height El.fill
        , El.clipX
        , El.scrollbarX
        ]
        [ El.el
            [ El.alignTop ]
            (Link.function function)
        , El.el
            [ El.alignRight ]
            (El.text currentValue)
        ]



-- Panel --


panel : Device -> List (Element msg) -> Element msg
panel { class, orientation } =
    El.column
        [ BackgroundColor.examplePanel
        , Border.width 1
        , BorderColor.examplePanel

        --, El.centerX
        , El.padding 10
        , El.height <|
            El.maximum 350 El.fill
        , El.width <|
            case ( class, orientation ) of
                ( Phone, _ ) ->
                    El.maximum 500 El.fill

                ( Tablet, _ ) ->
                    El.maximum 700 El.fill

                ( Desktop, _ ) ->
                    El.maximum 600 El.fill

                ( BigDesktop, _ ) ->
                    El.maximum 700 El.fill
        ]



{- Attributes -}


spacing : DeviceClass -> Attribute msg
spacing class =
    case class of
        Phone ->
            El.spacing 5

        _ ->
            El.spacing 10
