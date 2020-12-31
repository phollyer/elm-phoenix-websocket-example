module UI.Padding exposing
    ( bottom
    , bottomSmall
    , large
    , medium
    , small
    , top
    , topLarge
    , topSmall
    , x
    , xLarge
    , xSmall
    , y
    , yMedium
    , ySmall
    )

import Element as El exposing (Attribute, Device, DeviceClass(..))



{- Types -}


type alias PaddingEach =
    { left : Int
    , top : Int
    , right : Int
    , bottom : Int
    }



{- Helpers -}


paddingEach : PaddingEach
paddingEach =
    { left = 0
    , top = 0
    , right = 0
    , bottom = 0
    }



{- Build -}


small : Device -> Attribute msg
small { class } =
    El.padding <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


medium : Device -> Attribute msg
medium { class } =
    El.padding <|
        case class of
            Phone ->
                7

            Tablet ->
                10

            _ ->
                15


large : Device -> Attribute msg
large { class } =
    El.padding <|
        case class of
            Phone ->
                10

            Tablet ->
                15

            _ ->
                20


bottom : Int -> Attribute msg
bottom amount =
    El.paddingEach
        { paddingEach | bottom = amount }


bottomSmall : Device -> Attribute msg
bottomSmall { class } =
    bottom <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


top : Int -> Attribute msg
top amount =
    El.paddingEach
        { paddingEach | top = amount }


topSmall : Device -> Attribute msg
topSmall { class } =
    top <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


topLarge : Device -> Attribute msg
topLarge { class } =
    top <|
        case class of
            Phone ->
                10

            Tablet ->
                15

            _ ->
                20


x : Int -> Attribute msg
x amount =
    El.paddingXY amount 0


xSmall : Device -> Attribute msg
xSmall { class } =
    x <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


xLarge : Device -> Attribute msg
xLarge { class } =
    x <|
        case class of
            Phone ->
                10

            Tablet ->
                15

            _ ->
                20


y : Int -> Attribute msg
y amount =
    El.paddingXY 0 amount


ySmall : Device -> Attribute msg
ySmall { class } =
    y <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


yMedium : Device -> Attribute msg
yMedium { class } =
    y <|
        case class of
            Phone ->
                7

            Tablet ->
                10

            _ ->
                15
