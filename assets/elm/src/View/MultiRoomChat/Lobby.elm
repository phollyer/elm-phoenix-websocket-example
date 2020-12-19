module View.MultiRoomChat.Lobby exposing
    ( init
    , members
    , onCreateRoom
    , onDeleteRoom
    , onEnterRoom
    , rooms
    , user
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Types exposing (Presence, Room, User, initUser)
import UI.Align as Align
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import View.Button as Button
import View.MultiRoomChat.Lobby.Occupants as Occupants
import View.MultiRoomChat.Lobby.Rooms as Rooms
import View.MultiRoomChat.User as User



{- Model -}


type Config msg
    = Config
        { user : User
        , members : List Presence
        , onCreateRoom : Maybe msg
        , onDeleteRoom : Maybe (Room -> msg)
        , onEnterRoom : Maybe (Room -> msg)
        , rooms : List Room
        }


init : Config msg
init =
    Config
        { user = initUser
        , members = []
        , onCreateRoom = Nothing
        , onDeleteRoom = Nothing
        , onEnterRoom = Nothing
        , rooms = []
        }


members : List Presence -> Config msg -> Config msg
members members_ (Config config) =
    Config { config | members = members_ }


onCreateRoom : msg -> Config msg -> Config msg
onCreateRoom msg (Config config) =
    Config { config | onCreateRoom = Just msg }


onDeleteRoom : (Room -> msg) -> Config msg -> Config msg
onDeleteRoom msg (Config config) =
    Config { config | onDeleteRoom = Just msg }


onEnterRoom : (Room -> msg) -> Config msg -> Config msg
onEnterRoom msg (Config config) =
    Config { config | onEnterRoom = Just msg }


rooms : List Room -> Config msg -> Config msg
rooms rooms_ (Config config) =
    Config { config | rooms = rooms_ }


user : User -> Config msg -> Config msg
user user_ (Config config) =
    Config { config | user = user_ }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.column
        [ El.width El.fill
        , El.spacing 15
        ]
        [ container device
            [ El.width El.fill
            , El.spacing 15
            ]
            [ El.column
                [ BackgroundColor.panel
                , Border.rounded 10
                , El.padding 20
                , El.spacing 20
                , El.width El.fill
                , FontColor.panel
                ]
                [ userView device config.user
                , createRoomBtn device config.onCreateRoom
                ]
            ]
        , occupantsView device config.user config.members
        , roomsView device (Config config)
        ]


container : Device -> List (Attribute msg) -> List (Element msg) -> Element msg
container { class, orientation } =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            El.column

        _ ->
            El.wrappedRow



{- Lobby User -}


userView : Device -> User -> Element msg
userView device { username, id } =
    El.column
        [ alignFont device
        , El.alignTop
        , El.spacing 20
        , El.width El.fill
        ]
        [ User.init
            |> User.username username
            |> User.userId id
            |> User.view device
        , El.column
            [ El.spacing 10
            , El.width El.fill
            ]
            [ El.el
                [ El.width El.fill
                , Font.bold
                , FontColor.subTitle
                ]
                (El.text "Rooms")
            , paragraph
                [ El.text "- A Room is opened when the owner of the Room enters it." ]
            , paragraph
                [ El.text "- A Room can only be entered by a guest after it has been opened by the owner." ]
            , paragraph
                [ El.text "- When the owner leaves a room it will close and all occupants will return to the lobby. " ]
            , paragraph
                [ El.text "- When a room closes, the messages will be retained until the room is deleted by the owner." ]
            , paragraph
                [ El.text "- A room is deleted when it's owner leaves this Example, or when the Delete button is clicked." ]
            ]
        ]


paragraph : List (Element msg) -> Element msg
paragraph text =
    El.paragraph
        [ El.spacing 5
        , El.width El.fill
        ]
        text



{- Create Room Button -}


createRoomBtn : Device -> Maybe msg -> Element msg
createRoomBtn ({ class, orientation } as device) maybeMsg =
    Button.init
        |> Button.setLabel "Create A Room"
        |> Button.setOnPress maybeMsg
        |> Button.setAlignX
            (case ( class, orientation ) of
                ( Phone, Portrait ) ->
                    Align.Center

                ( Phone, Landscape ) ->
                    Align.Left

                _ ->
                    Align.Center
            )
        |> Button.view device



{- Lobby Members -}


occupantsView : Device -> User -> List Presence -> Element msg
occupantsView device currentUser presences =
    El.el
        [ El.alignTop
        , El.width El.fill
        ]
        (Occupants.init
            |> Occupants.all (toUsers presences)
            |> Occupants.currentUser currentUser
            |> Occupants.view device
        )


toUsers : List Presence -> List User
toUsers presences =
    List.map .user presences
        |> List.sortBy .username



{- Rooms -}


roomsView : Device -> Config msg -> Element msg
roomsView device (Config config) =
    if List.isEmpty config.rooms then
        El.none

    else
        El.el
            [ El.alignTop
            , El.width El.fill
            ]
            (Rooms.init
                |> Rooms.rooms config.rooms
                |> Rooms.user config.user
                |> Rooms.onClick config.onEnterRoom
                |> Rooms.onDelete config.onDeleteRoom
                |> Rooms.view device
            )



{- Attributes -}


alignFont : Device -> Attribute msg
alignFont { class, orientation } =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            Font.center

        _ ->
            Font.alignLeft
