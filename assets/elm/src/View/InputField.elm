module View.InputField exposing
    ( init
    , label
    , multiline
    , onChange
    , onFocus
    , onLoseFocus
    , text
    , view
    )

import Element as El exposing (Attribute, Device, Element)
import Element.Border as Border
import Element.Events as Event
import Element.Input as Input
import UI.Padding as Padding
import UI.RoundedBorder as RoundedBorder
import Utils exposing (andMaybeEventWith)



{- Model -}


type Config msg
    = Config
        { label : String
        , text : String
        , multiline : Bool
        , onChange : Maybe (String -> msg)
        , onFocus : Maybe msg
        , onLoseFocus : Maybe msg
        }


init : Config msg
init =
    Config
        { label = ""
        , text = ""
        , multiline = False
        , onChange = Nothing
        , onFocus = Nothing
        , onLoseFocus = Nothing
        }


label : String -> Config msg -> Config msg
label label_ (Config config) =
    Config { config | label = label_ }


multiline : Bool -> Config msg -> Config msg
multiline bool (Config config) =
    Config { config | multiline = bool }


text : String -> Config msg -> Config msg
text name (Config config) =
    Config { config | text = name }


onChange : Maybe (String -> msg) -> Config msg -> Config msg
onChange maybeToMsg (Config config) =
    Config { config | onChange = maybeToMsg }


onFocus : Maybe msg -> Config msg -> Config msg
onFocus maybeMsg (Config config) =
    Config { config | onFocus = maybeMsg }


onLoseFocus : Maybe msg -> Config msg -> Config msg
onLoseFocus maybeMsg (Config config) =
    Config { config | onLoseFocus = maybeMsg }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    case config.onChange of
        Nothing ->
            El.text config.text

        Just onChange_ ->
            if config.multiline then
                multi device onChange_ (Config config)

            else
                single device onChange_ (Config config)



{- Multiline -}


multi : Device -> (String -> msg) -> Config msg -> Element msg
multi device onChange_ (Config config) =
    Input.multiline
        (multilineAttrs device
            |> andMaybeEventWith config.onFocus Event.onFocus
            |> andMaybeEventWith config.onLoseFocus Event.onLoseFocus
        )
        { onChange = onChange_
        , text = config.text
        , placeholder = placeholder config.label
        , label = Input.labelHidden config.label
        , spellcheck = True
        }


multilineAttrs : Device -> List (Attribute msg)
multilineAttrs device =
    [ El.height <|
        El.maximum 150 El.fill
    , El.width El.fill
    , Padding.small device
    , RoundedBorder.small device
    ]



{- Single Line -}


single : Device -> (String -> msg) -> Config msg -> Element msg
single device onChange_ (Config config) =
    Input.text
        (singleLineAttrs device
            |> andMaybeEventWith config.onFocus Event.onFocus
            |> andMaybeEventWith config.onLoseFocus Event.onLoseFocus
        )
        { onChange = onChange_
        , text = config.text
        , placeholder = placeholder config.label
        , label = Input.labelHidden config.label
        }


singleLineAttrs : Device -> List (Attribute msg)
singleLineAttrs device =
    [ El.width El.fill
    , Padding.small device
    , RoundedBorder.small device
    ]



{- Common -}


placeholder : String -> Maybe (Input.Placeholder msg)
placeholder text_ =
    Just (Input.placeholder [] (El.text text_))
