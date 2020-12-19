module View.MultiRoomChat.Lobby.Rooms exposing
    ( init
    , onClick
    , onDelete
    , rooms
    , user
    , view
    )

import Element as El exposing (Attribute, Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Types exposing (Room, User, initUser)
import UI.BackgroundColor as BackgroundColor
import UI.BorderColor as BorderColor
import UI.FontColor as FontColor
import UI.Shadow as Shadow
import View.Button as Button



{- Model -}


type Config msg
    = Config
        { rooms : List Room
        , user : User
        , onClick : Maybe (Room -> msg)
        , onDelete : Maybe (Room -> msg)
        }


init : Config msg
init =
    Config
        { rooms = []
        , user = initUser
        , onClick = Nothing
        , onDelete = Nothing
        }


rooms : List Room -> Config msg -> Config msg
rooms rooms_ (Config config) =
    Config { config | rooms = rooms_ }


user : User -> Config msg -> Config msg
user user_ (Config config) =
    Config { config | user = user_ }


onClick : Maybe (Room -> msg) -> Config msg -> Config msg
onClick toMsg (Config config) =
    Config { config | onClick = toMsg }


onDelete : Maybe (Room -> msg) -> Config msg -> Config msg
onDelete toMsg (Config config) =
    Config { config | onDelete = toMsg }



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
    <|
        List.append
            [ El.el
                [ El.centerX
                , FontColor.subTitle
                ]
                (El.text "Room List")
            ]
            (orderRooms config.user config.rooms
                |> List.map (toRoom device (Config config))
            )


orderRooms : User -> List Room -> List Room
orderRooms currentUser roomList =
    let
        ( ownersRooms, others ) =
            List.partition (\room -> currentUser == room.owner) roomList
    in
    List.append ownersRooms <|
        List.sortWith
            (\room1 room2 ->
                case compare room1.owner.username room2.owner.username of
                    LT ->
                        LT

                    EQ ->
                        EQ

                    GT ->
                        GT
            )
            others


toRoom : Device -> Config msg -> Room -> Element msg
toRoom device (Config config) room =
    El.row
        (List.append defaultAttrs <|
            roomAttrs room
        )
        [ El.column
            [ El.width El.fill
            , El.clipX
            ]
            [ owner config.user room
            , occupantsList config.user room
            ]
        , El.row
            [ El.spacing 10 ]
            [ maybeEnterBtn device config.onClick config.user room
            , maybeDeleteBtn device config.onDelete config.user room
            ]
        ]


maybeDeleteBtn : Device -> Maybe (Room -> msg) -> User -> Room -> Element msg
maybeDeleteBtn device maybeToOnDelete currentUser room =
    if currentUser == room.owner then
        case maybeToOnDelete of
            Nothing ->
                El.none

            Just onDelete_ ->
                Button.init
                    |> Button.setLabel "Delete"
                    |> Button.setOnPress (Just (onDelete_ room))
                    |> Button.view device

    else
        El.none


maybeEnterBtn : Device -> Maybe (Room -> msg) -> User -> Room -> Element msg
maybeEnterBtn device maybeToOnClick currentUser room =
    if currentUser == room.owner || List.member room.owner room.members then
        case maybeToOnClick of
            Nothing ->
                El.none

            Just onClick_ ->
                Button.init
                    |> Button.setLabel "Enter"
                    |> Button.setOnPress (Just (onClick_ room))
                    |> Button.view device

    else
        El.none


owner : User -> Room -> Element msg
owner currentUser room =
    let
        username =
            if currentUser == room.owner then
                "You"

            else
                room.owner.username
    in
    El.row
        [ El.spacing 10
        , El.width El.fill
        , El.clipX
        ]
        [ El.text "Owner:"
        , El.text username
        ]


occupantsList : User -> Room -> Element msg
occupantsList currentUser room =
    if currentUser == room.owner then
        El.none

    else
        El.paragraph
            [ El.width El.fill
            , Font.alignLeft
            ]
            [ El.text "Members: "
            , List.map .username room.members
                |> List.intersperse ", "
                |> String.concat
                |> El.text
            ]



{- Attributes -}


defaultAttrs : List (Attribute msg)
defaultAttrs =
    [ Border.rounded 10
    , Border.width 1
    , El.padding 10
    , El.spacing 10
    , El.width El.fill
    ]


roomAttrs : Room -> List (Attribute msg)
roomAttrs room =
    [ Background.color room.owner.backgroundColor
    , Border.color room.owner.foregroundColor
    , El.mouseOver
        [ Border.color room.owner.backgroundColor
        , Border.shadow
            { size = 1
            , blur = 5
            , color = room.owner.backgroundColor
            , offset = ( 0, 0 )
            }
        ]
    , Font.color room.owner.foregroundColor
    ]
