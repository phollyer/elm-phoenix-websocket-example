module View.Example.Controls exposing
    ( Config
    , elements
    , group
    , init
    , options
    , userId
    , view
    )

import Colors.Opaque as Color
import Element as El exposing (Device, DeviceClass(..), Element, Orientation(..))
import Element.Border as Border
import Element.Font as Font
import List.Extra as List
import View.Group as Group



{- Model -}


type Config msg
    = Config
        { userId : Maybe String
        , elements : List (Element msg)
        , layout : Maybe (List Int)
        , options : Element msg
        , group : Group.Config
        }


init : Config msg
init =
    Config
        { userId = Nothing
        , elements = []
        , layout = Nothing
        , options = El.none
        , group = Group.init
        }


userId : Maybe String -> Config msg -> Config msg
userId maybeUserId_ (Config config) =
    Config { config | userId = maybeUserId_ }


elements : List (Element msg) -> Config msg -> Config msg
elements list (Config config) =
    Config { config | elements = list }


options : Element msg -> Config msg -> Config msg
options element (Config config) =
    Config { config | options = element }


group : Group.Config -> Config msg -> Config msg
group group_ (Config config) =
    Config { config | group = group_ }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    case Group.layoutForDevice device config.group of
        Nothing ->
            column
                [ maybeUserId config.userId
                , El.column
                    [ El.width El.fill
                    , El.spacing 20
                    ]
                    [ config.options
                    , toRow <|
                        Group.orderForDevice device config.elements config.group
                    ]
                ]

        Just layout ->
            column
                [ maybeUserId config.userId
                , El.column
                    [ El.width El.fill
                    , El.spacing 20
                    ]
                    [ config.options
                    , El.column
                        [ El.width El.fill
                        , El.spacing 10
                        ]
                      <|
                        toRows layout <|
                            Group.orderForDevice device config.elements config.group
                    ]
                ]


column : List (Element msg) -> Element msg
column =
    El.column
        [ Border.color Color.aliceblue
        , Border.widthEach
            { left = 0
            , top = 1
            , right = 0
            , bottom = 1
            }
        , El.paddingXY 0 10
        , El.scrollbarY
        , El.spacing 10
        , El.width El.fill
        ]


maybeUserId : Maybe String -> Element msg
maybeUserId maybeId_ =
    case maybeId_ of
        Nothing ->
            El.none

        Just id ->
            El.paragraph
                [ Font.family
                    [ Font.typeface "Varela Round" ]
                ]
                [ El.el
                    [ Font.color Color.lavender ]
                    (El.text "User ID: ")
                , El.el
                    [ Font.color Color.powderblue ]
                    (El.text id)
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
