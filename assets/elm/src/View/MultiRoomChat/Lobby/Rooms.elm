module View.MultiRoomChat.Lobby.Rooms exposing
    ( init
    , onClick
    , onDelete
    , onMouseEnter
    , rooms
    , showRoomMembers
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Json.Decode.Extra exposing (combine)
import List.Extra as List
import Type.Room exposing (Room)
import Type.User as User exposing (RegisteredUser, User)
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import View.Button as Button
import View.Tag as Tag



{- Model -}


type Config msg
    = Config
        { rooms : List Room
        , user : RegisteredUser
        , onClick : Maybe (Room -> msg)
        , onDelete : Maybe (Room -> msg)
        , onMouseEnter : Maybe (Maybe Room -> msg)
        , showRoomMembers : Maybe Room
        }


init : RegisteredUser -> Config msg
init user =
    Config
        { rooms = []
        , user = user
        , onClick = Nothing
        , onDelete = Nothing
        , onMouseEnter = Nothing
        , showRoomMembers = Nothing
        }


rooms : List Room -> Config msg -> Config msg
rooms rooms_ (Config config) =
    Config { config | rooms = rooms_ }


onClick : Maybe (Room -> msg) -> Config msg -> Config msg
onClick toMsg (Config config) =
    Config { config | onClick = toMsg }


onDelete : Maybe (Room -> msg) -> Config msg -> Config msg
onDelete toMsg (Config config) =
    Config { config | onDelete = toMsg }


onMouseEnter : Maybe (Maybe Room -> msg) -> Config msg -> Config msg
onMouseEnter toMsg (Config config) =
    Config { config | onMouseEnter = toMsg }


showRoomMembers : Maybe Room -> Config msg -> Config msg
showRoomMembers maybeRoom (Config config) =
    Config { config | showRoomMembers = maybeRoom }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.column
        [ BackgroundColor.panel
        , Border.rounded 10
        , El.padding 10
        , El.spacing 10
        , El.width El.fill
        , FontColor.panel
        ]
        [ El.el
            [ El.centerX
            , FontColor.subTitle
            ]
            (El.text "Room List")
        , El.column
            [ El.width El.fill
            , El.spacing 10
            ]
            (roomsView device (Config config))
        ]


roomsView : Device -> Config msg -> List (Element msg)
roomsView device (Config config) =
    List.partition (\room -> room.owner == config.user) config.rooms
        |> Tuple.mapFirst (toRoomList device (Config config))
        |> Tuple.mapSecond sortOtherRooms
        |> Tuple.mapSecond (toRows device (Config config))
        |> combine


toRows : Device -> Config msg -> List (List Room) -> List (Element msg)
toRows device config rooms_ =
    List.map (toRoomList device config) rooms_


combine : ( a, List a ) -> List a
combine ( a, b ) =
    a :: b


toRoomList : Device -> Config msg -> List Room -> Element msg
toRoomList device (Config config) rooms_ =
    case rooms_ of
        [] ->
            El.none

        room :: _ ->
            El.column
                [ El.spacing 10
                , El.width El.fill
                ]
                [ El.el [] (Tag.view device config.user room.owner)
                , List.map (toRoom device (Config config)) rooms_
                    |> El.wrappedRow
                        [ El.spacing 10
                        , El.width El.fill
                        ]
                ]


occupantsView : Device -> RegisteredUser -> Maybe Room -> Room -> List (Attribute msg)
occupantsView device currentUser maybeForRoom room =
    case maybeForRoom of
        Nothing ->
            []

        Just room_ ->
            if room_ == room then
                [ El.inFront <|
                    El.el
                        [ El.width El.fill
                        , El.above <|
                            occupantsList device currentUser room
                        ]
                        El.none
                ]

            else
                []


sortOtherRooms : List Room -> List (List Room)
sortOtherRooms rooms_ =
    List.sortWith byOwner rooms_
        |> List.groupWhile (\roomA roomB -> roomA.owner == roomB.owner)
        |> List.map (\( a, b ) -> a :: b)
        |> List.map (\a -> List.sortWith mostMembers a)


toRoom : Device -> Config msg -> Room -> Element msg
toRoom device (Config config) room =
    El.row
        (List.append defaultAttrs (roomAttrs room config.onMouseEnter)
            |> List.append (occupantsView device config.user config.showRoomMembers room)
        )
        [ El.text <|
            "Occupants: "
                ++ (List.length room.members
                        |> String.fromInt
                   )
        , El.row
            [ El.alignRight
            , El.spacing 10
            ]
            [ maybeEnterBtn device config.onClick config.user room
            , maybeDeleteBtn device config.onDelete config.user room
            ]
        ]


occupantsList : Device -> RegisteredUser -> Room -> Element msg
occupantsList device currentUser room =
    case room.members of
        [] ->
            El.none

        _ ->
            El.wrappedRow
                [ padding device
                , roundedBorders device
                , Background.color (User.bgColor room.owner)
                , Border.color (User.fgColor room.owner)
                , Border.width 1
                ]
                [ User.sortByUsername room.members
                    |> List.map (Tag.view device currentUser)
                    |> El.wrappedRow
                        [ El.spacing 10
                        , El.width El.fill
                        ]
                    |> El.el [ El.width El.fill ]
                ]



{- Sort -}


byOwner : Room -> Room -> Order
byOwner roomA roomB =
    case compare (User.username roomA.owner) (User.username roomB.owner) of
        LT ->
            LT

        EQ ->
            EQ

        GT ->
            GT


mostMembers : Room -> Room -> Order
mostMembers roomA roomB =
    case compare (List.length roomA.members) (List.length roomB.members) of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT



{- Buttons -}


maybeDeleteBtn : Device -> Maybe (Room -> msg) -> RegisteredUser -> Room -> Element msg
maybeDeleteBtn device maybeToOnDelete currentUser room =
    if User.match currentUser room.owner then
        case maybeToOnDelete of
            Nothing ->
                El.none

            Just onDelete_ ->
                Button.init
                    |> Button.setLabel "Delete"
                    |> Button.setOnPress (Just (onDelete_ room))
                    |> Button.setType (Button.User room.owner)
                    |> Button.view device

    else
        El.none


maybeEnterBtn : Device -> Maybe (Room -> msg) -> RegisteredUser -> Room -> Element msg
maybeEnterBtn device maybeToOnClick currentUser room =
    if User.match currentUser room.owner || List.member room.owner room.members then
        case maybeToOnClick of
            Nothing ->
                El.none

            Just onClick_ ->
                Button.init
                    |> Button.setLabel "Enter"
                    |> Button.setOnPress (Just (onClick_ room))
                    |> Button.setType (Button.User room.owner)
                    |> Button.view device

    else
        El.none



{- Attributes -}


defaultAttrs : List (Attribute msg)
defaultAttrs =
    [ Border.rounded 10
    , Border.width 1
    , El.padding 10
    , El.spacing 10
    , El.width El.fill
    ]


roomAttrs : Room -> Maybe (Maybe Room -> msg) -> List (Attribute msg)
roomAttrs room maybeOnMouseEnter =
    case maybeOnMouseEnter of
        Nothing ->
            []

        Just onMouseEnter_ ->
            [ Background.color (User.bgColor room.owner)
            , Border.color (User.fgColor room.owner)
            , El.mouseOver
                [ Border.color (User.bgColor room.owner)
                , Border.shadow
                    { size = 1
                    , blur = 5
                    , color = User.bgColor room.owner
                    , offset = ( 0, 0 )
                    }
                ]
            , El.width El.fill
            , Event.onMouseEnter (onMouseEnter_ (Just room))
            , Event.onMouseLeave (onMouseEnter_ Nothing)
            , Font.color (User.fgColor room.owner)
            ]


padding : Device -> Attribute msg
padding { class } =
    El.padding <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


roundedBorders : Device -> Attribute msg
roundedBorders { class } =
    Border.rounded <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10
