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
import Utils exposing (andMaybeEventWithArg)



{- Model -}


type Config msg
    = Config
        { options : List String
        , selected : String
        , onClick : Maybe (String -> msg)
        , layout : Maybe (List Int)
        , group : Group
        }


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
            El.column
                (paddingEach device
                    :: containerAttrs device
                )
            <|
                List.map (stackItem config.selected config.onClick) config.options

        _ ->
            case Group.layoutForDevice device config.group of
                Nothing ->
                    El.row
                        (El.paddingXY 5 0
                            :: containerAttrs device
                        )
                    <|
                        List.map (rowItem config.selected config.onClick) config.options

                Just layout ->
                    El.column
                        (paddingEach device
                            :: containerAttrs device
                        )
                    <|
                        (Group.orderForDevice device config.options config.group
                            |> List.groupsOfVarying layout
                            |> toRows config.selected config.onClick
                        )


toRows : String -> Maybe (String -> msg) -> List (List String) -> List (Element msg)
toRows selected_ maybeOnClick options_ =
    List.map (toRow selected_ maybeOnClick) options_


toRow : String -> Maybe (String -> msg) -> List String -> Element msg
toRow selected_ maybeOnClick options_ =
    El.wrappedRow
        [ El.width El.fill
        , El.spacing 20
        ]
        (List.map (stackItem selected_ maybeOnClick) options_)


stackItem : String -> Maybe (String -> msg) -> String -> Element msg
stackItem selected_ maybeOnClick item =
    El.el
        (List.append
            (itemAttrs selected_ item maybeOnClick)
            [ BorderWidth.bottom 4 ]
        )
        (El.text item)


rowItem : String -> Maybe (String -> msg) -> String -> Element msg
rowItem selected_ maybeOnClick item =
    El.el
        (List.append
            (itemAttrs selected_ item maybeOnClick)
            [ Border.widthXY 0 4
            , El.paddingXY 0 5
            ]
        )
        (El.text item)



{- Attributes -}


containerAttrs : Device -> List (Attribute msg)
containerAttrs device =
    [ spacing device
    , Border.widthXY 0 1
    , BorderColor.seperatorLight
    , El.width El.fill
    , FontColor.default
    , FontFamily.default
    ]


itemAttrs : String -> String -> Maybe (String -> msg) -> List (Attribute msg)
itemAttrs selected_ item maybeOnClick =
    if selected_ == item then
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
            |> andMaybeEventWithArg maybeOnClick item Event.onClick


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


spacing : Device -> Attribute msg
spacing { class } =
    case class of
        Phone ->
            El.spacing 10

        Tablet ->
            El.spacing 10

        _ ->
            El.spacing 20
