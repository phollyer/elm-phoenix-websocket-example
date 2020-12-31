module View.Example.Menu exposing
    ( Config
    , group
    , init
    , onClick
    , options
    , selected
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Border as Border
import Element.Events as Event
import List.Extra as List
import Type.Group as Group exposing (Group)
import UI.BorderColor as BorderColor
import UI.BorderWidth as BorderWidth
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily
import UI.Padding as Padding
import UI.Spacing as Spacing
import Utils exposing (andMaybeEventWithArg)



{- Types -}


type Config msg
    = Config
        { options : List String
        , selected : String
        , onClick : Maybe (String -> msg)
        , layout : Maybe (List Int)
        , group : Group
        }



{- Build -}


init : Config msg
init =
    Config
        { options = []
        , selected = ""
        , onClick = Nothing
        , layout = Nothing
        , group = Group.init
        }


options : List String -> Config msg -> Config msg
options options_ (Config config) =
    Config { config | options = options_ }


selected : String -> Config msg -> Config msg
selected selected_ (Config config) =
    Config { config | selected = selected_ }


onClick : Maybe (String -> msg) -> Config msg -> Config msg
onClick msg (Config config) =
    Config { config | onClick = msg }


group : Group -> Config msg -> Config msg
group group_ (Config config) =
    Config { config | group = group_ }



{- View -}


view : Device -> Config msg -> Element msg
view ({ class, orientation } as device) (Config config) =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            column device <|
                List.map (toStackedOption config.selected config.onClick) config.options

        _ ->
            case Group.layoutForDevice device config.group of
                Nothing ->
                    row device <|
                        List.map (toInlineOption config.selected config.onClick) config.options

                Just sizes ->
                    column device
                        (Group.orderForDevice device config.options config.group
                            |> List.groupsOfVarying sizes
                            |> List.map (toStackedRow device config.selected config.onClick)
                        )


toInlineOption : String -> Maybe (String -> msg) -> String -> Element msg
toInlineOption selected_ maybeOnClick option =
    El.el
        (List.append
            (optionAttrs selected_ option maybeOnClick)
            [ Border.widthXY 0 4
            , El.paddingXY 0 5
            ]
        )
        (El.text option)


toStackedOption : String -> Maybe (String -> msg) -> String -> Element msg
toStackedOption selected_ maybeOnClick option =
    El.el
        (List.append
            (optionAttrs selected_ option maybeOnClick)
            [ BorderWidth.bottom 4 ]
        )
        (El.text option)


toStackedRow : Device -> String -> Maybe (String -> msg) -> List String -> Element msg
toStackedRow device selected_ maybeOnClick options_ =
    El.row
        [ El.width El.fill
        , Spacing.large device
        ]
        (List.map (toStackedOption selected_ maybeOnClick) options_)



{- Containers -}


column : Device -> List (Element msg) -> Element msg
column device =
    El.column
        (paddingEach device
            :: containerAttrs device
        )


row : Device -> List (Element msg) -> Element msg
row device =
    El.row
        (Padding.xSmall device
            :: containerAttrs device
        )



{- Attributes -}


containerAttrs : Device -> List (Attribute msg)
containerAttrs device =
    [ Border.widthXY 0 1
    , BorderColor.seperatorLight
    , El.width El.fill
    , FontColor.default
    , FontFamily.default
    , Spacing.large device
    ]


optionAttrs : String -> String -> Maybe (String -> msg) -> List (Attribute msg)
optionAttrs selected_ option maybeOnClick =
    if selected_ == option then
        [ BorderColor.seperatorLight
        , El.centerX
        ]

    else
        [ BorderColor.none
        , El.centerX
        , El.pointer
        , El.mouseOver
            [ BorderColor.mouseOverMenuItem ]
        ]
            |> andMaybeEventWithArg maybeOnClick option Event.onClick


paddingEach : Device -> Attribute msg
paddingEach { class, orientation } =
    case ( class, orientation ) of
        ( Phone, _ ) ->
            El.paddingEach
                { left = 5
                , top = 16
                , right = 5
                , bottom = 8
                }

        ( Tablet, Portrait ) ->
            El.paddingEach
                { left = 5
                , top = 16
                , right = 5
                , bottom = 8
                }

        _ ->
            El.paddingEach
                { left = 5
                , top = 10
                , right = 5
                , bottom = 0
                }
