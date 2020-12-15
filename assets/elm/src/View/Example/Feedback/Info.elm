module View.Example.Feedback.Info exposing
    ( Config
    , event
    , init
    , joinRef
    , payload
    , ref
    , topic
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Json.Encode as JE exposing (Value)
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import UI.FontSize as FontSize
import UI.Padding as Padding



{- Model -}


type Config msg
    = Config
        { topic : String
        , event : Maybe String
        , payload : Value
        , joinRef : Maybe String
        , ref : Maybe String
        }


init : Config msg
init =
    Config
        { topic = ""
        , event = Nothing
        , payload = JE.null
        , joinRef = Nothing
        , ref = Nothing
        }


topic : String -> Config msg -> Config msg
topic topic_ (Config config) =
    Config { config | topic = topic_ }


event : String -> Config msg -> Config msg
event event_ (Config config) =
    Config
        { config
            | event =
                if event_ == "" then
                    Nothing

                else
                    Just event_
        }


payload : Value -> Config msg -> Config msg
payload payload_ (Config config) =
    Config { config | payload = payload_ }


joinRef : Maybe String -> Config msg -> Config msg
joinRef joinRef_ (Config config) =
    Config { config | joinRef = joinRef_ }


ref : Maybe String -> Config msg -> Config msg
ref ref_ (Config config) =
    Config { config | ref = ref_ }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.column
        [ El.alignLeft
        , El.spacing 10
        , El.width El.fill
        , FontFamily.code
        , Padding.bottom 10
        ]
        [ field device "Topic: " config.topic
        , maybe (field device) "Event: " config.event
        , field device "Payload: " (JE.encode 2 config.payload)
        , maybe (field device) "Join Ref: " config.joinRef
        , maybe (field device) "Ref: " config.ref
        ]


field : Device -> String -> String -> Element msg
field device label topic_ =
    El.wrappedRow
        [ FontSize.panelContent device
        , spacing device
        , El.width El.fill
        ]
        [ El.el
            [ El.alignTop
            , FontColor.default
            ]
            (El.text label)
        , El.el
            [ FontColor.value ]
            (El.text topic_)
        ]


maybe : (String -> String -> Element msg) -> String -> Maybe String -> Element msg
maybe toField label maybeValue =
    case maybeValue of
        Just value ->
            toField label value

        Nothing ->
            El.none



{- Attributes -}


spacing : Device -> Attribute msg
spacing { class, orientation } =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            El.spacing 5

        _ ->
            El.spacing 10
