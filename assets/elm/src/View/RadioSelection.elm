module View.RadioSelection exposing
    ( Config
    , init
    , label
    , onChange
    , options
    , selected
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Font as Font
import Element.Input as Input
import UI.Padding as Padding
import UI.Spacing as Spacing



{- Model -}


type Config value msg
    = Config
        { label : String
        , onChange : Maybe (value -> msg)
        , selected : Maybe value
        , options : List ( value, String )
        }


init : Config value msg
init =
    Config
        { label = ""
        , onChange = Nothing
        , options = []
        , selected = Nothing
        }


label : String -> Config value msg -> Config value msg
label label_ (Config config) =
    Config { config | label = label_ }


onChange : (value -> msg) -> Config value msg -> Config value msg
onChange toMsg (Config config) =
    Config { config | onChange = Just toMsg }


selected : value -> Config value msg -> Config value msg
selected selected_ (Config config) =
    Config { config | selected = Just selected_ }


options : List ( value, String ) -> Config value msg -> Config value msg
options options_ (Config config) =
    Config { config | options = options_ }



{- View -}


view : Device -> Config value msg -> Element msg
view device (Config config) =
    case config.onChange of
        Nothing ->
            El.none

        Just toMsg ->
            El.el
                [ El.centerX ]
            <|
                radio device
                    { onChange = toMsg
                    , selected = config.selected
                    , label = toLabel device config.label
                    , options = List.map (toOption device) config.options
                    }


type alias RadioConfig value msg =
    { label : Input.Label msg
    , onChange : value -> msg
    , selected : Maybe value
    , options : List (Input.Option value msg)
    }


radio : Device -> (RadioConfig value msg -> Element msg)
radio ({ class, orientation } as device) =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            Input.radio
                [ Spacing.small device ]

        _ ->
            Input.radioRow
                [ Spacing.large device ]


toLabel : Device -> String -> Input.Label msg
toLabel device text =
    Input.labelAbove
        [ El.centerX
        , Padding.bottomSmall device
        ]
        (El.text text)


toOption : Device -> ( value, String ) -> Input.Option value msg
toOption { class, orientation } ( value, text ) =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            Input.option value <|
                El.paragraph
                    [ El.width El.fill
                    , Font.alignLeft
                    ]
                    [ El.text text ]

        _ ->
            Input.option value <|
                El.el
                    [ El.width El.fill ]
                    (El.text text)
