module View.MultiRoomChat.Lobby exposing
    ( init
    , inviteError
    , members
    , onAcceptRoomInvite
    , onCreateRoom
    , onDeclineRoomInvite
    , onDeleteRoom
    , onEnterRoom
    , onInviteErrorOk
    , onMouseEnterRoom
    , roomInvites
    , rooms
    , showRoomMembers
    , user
    , view
    )

import Colors.Alpha as Color
import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Type.ErrorMessage exposing (ErrorMessage(..))
import Type.Presence exposing (Presence)
import Type.Room exposing (Room)
import Type.RoomInvite exposing (RoomInvite)
import Type.User as User exposing (User)
import UI.Align as Align
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import View.Button as Button
import View.MultiRoomChat.Lobby.Occupants as Occupants
import View.MultiRoomChat.Lobby.Rooms as Rooms
import View.MultiRoomChat.User as UserView



{- Model -}


type Config msg
    = Config
        { user : User
        , members : List Presence
        , onCreateRoom : Maybe msg
        , onDeleteRoom : Maybe (Room -> msg)
        , onEnterRoom : Maybe (Room -> msg)
        , onMouseEnterRoom : Maybe (Maybe Room -> msg)
        , onAcceptRoomInvite : Maybe (RoomInvite -> msg)
        , onDeclineRoomInvite : Maybe (RoomInvite -> msg)
        , onInviteErrorOk : Maybe (RoomInvite -> msg)
        , rooms : List Room
        , showRoomMembers : Maybe Room
        , roomInvites : List RoomInvite
        , inviteError : Maybe ErrorMessage
        }


init : Config msg
init =
    Config
        { user = User.init
        , members = []
        , onCreateRoom = Nothing
        , onDeleteRoom = Nothing
        , onEnterRoom = Nothing
        , onMouseEnterRoom = Nothing
        , onAcceptRoomInvite = Nothing
        , onDeclineRoomInvite = Nothing
        , onInviteErrorOk = Nothing
        , rooms = []
        , showRoomMembers = Nothing
        , roomInvites = []
        , inviteError = Nothing
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


onMouseEnterRoom : (Maybe Room -> msg) -> Config msg -> Config msg
onMouseEnterRoom msg (Config config) =
    Config { config | onMouseEnterRoom = Just msg }


onAcceptRoomInvite : (RoomInvite -> msg) -> Config msg -> Config msg
onAcceptRoomInvite msg (Config config) =
    Config { config | onAcceptRoomInvite = Just msg }


onDeclineRoomInvite : (RoomInvite -> msg) -> Config msg -> Config msg
onDeclineRoomInvite msg (Config config) =
    Config { config | onDeclineRoomInvite = Just msg }


onInviteErrorOk : (RoomInvite -> msg) -> Config msg -> Config msg
onInviteErrorOk msg (Config config) =
    Config { config | onInviteErrorOk = Just msg }


rooms : List Room -> Config msg -> Config msg
rooms rooms_ (Config config) =
    Config { config | rooms = rooms_ }


roomInvites : List RoomInvite -> Config msg -> Config msg
roomInvites invites (Config config) =
    Config { config | roomInvites = invites }


inviteError : Maybe ErrorMessage -> Config msg -> Config msg
inviteError maybeError (Config config) =
    Config { config | inviteError = maybeError }


showRoomMembers : Maybe Room -> Config msg -> Config msg
showRoomMembers maybeRoom (Config config) =
    Config { config | showRoomMembers = maybeRoom }


user : User -> Config msg -> Config msg
user user_ (Config config) =
    Config { config | user = user_ }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.column
        [ El.width El.fill
        , El.spacing 15
        , El.inFront <|
            roomInvite device (Config config)
        , El.inFront <|
            inviteErrorView device config.inviteError config.onInviteErrorOk
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


roomInvite : Device -> Config msg -> Element msg
roomInvite device (Config config) =
    case ( config.roomInvites, config.inviteError ) of
        ( invite :: _, Nothing ) ->
            case ( config.onAcceptRoomInvite, config.onDeclineRoomInvite ) of
                ( Just acceptMsg, Just declineMsg ) ->
                    El.el
                        [ roundedBorder device
                        , El.height El.fill
                        , El.width El.fill
                        , Background.color (Color.white 0.5)
                        ]
                        (El.column
                            [ padding device
                            , spacing device
                            , roundedBorder device
                            , Background.color invite.from.backgroundColor
                            , Border.color invite.from.foregroundColor
                            , Border.width 1
                            , El.centerX
                            , El.centerY
                            , Font.color invite.from.foregroundColor
                            ]
                            [ El.el
                                [ El.centerX ]
                                (El.text "Room Invite")
                            , El.paragraph
                                []
                                [ El.text "You have received an invitation from "
                                , El.text invite.from.username
                                , El.text " to join them in their room."
                                ]
                            , El.row
                                [ spacing device
                                , El.centerX
                                ]
                                [ Button.init
                                    |> Button.setLabel "Decline"
                                    |> Button.setOnPress (Just (declineMsg invite))
                                    |> Button.setType (Button.User invite.from)
                                    |> Button.view device
                                , Button.init
                                    |> Button.setLabel "Accept"
                                    |> Button.setOnPress (Just (acceptMsg invite))
                                    |> Button.setType (Button.User invite.from)
                                    |> Button.view device
                                ]
                            ]
                        )

                _ ->
                    El.none

        _ ->
            El.none


inviteErrorView : Device -> Maybe ErrorMessage -> Maybe (RoomInvite -> msg) -> Element msg
inviteErrorView device maybeError maybeMsg =
    case ( maybeError, maybeMsg ) of
        ( Just (RoomClosed invite), Just toMsg ) ->
            El.el
                [ roundedBorder device
                , El.height El.fill
                , El.width El.fill
                , Background.color (Color.white 0.5)
                ]
                (El.column
                    [ padding device
                    , spacing device
                    , roundedBorder device
                    , Background.color invite.from.backgroundColor
                    , Border.color invite.from.foregroundColor
                    , Border.width 1
                    , El.centerX
                    , El.centerY
                    , Font.color invite.from.foregroundColor
                    ]
                    [ El.el
                        [ El.centerX ]
                        (El.text "Error")
                    , El.paragraph
                        []
                        [ El.text "Sorry, the room has now closed." ]
                    , Button.init
                        |> Button.setLabel "OK"
                        |> Button.setOnPress (Just (toMsg invite))
                        |> Button.setType (Button.User invite.from)
                        |> Button.view device
                    ]
                )

        _ ->
            El.none



{- Lobby User -}


userView : Device -> User -> Element msg
userView device { username, id } =
    El.column
        [ alignFont device
        , El.alignTop
        , El.spacing 20
        , El.width El.fill
        ]
        [ UserView.init
            |> UserView.username username
            |> UserView.userId id
            |> UserView.view device
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
                |> Rooms.onMouseEnter config.onMouseEnterRoom
                |> Rooms.showRoomMembers config.showRoomMembers
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


roundedBorder : Device -> Attribute msg
roundedBorder { class } =
    Border.rounded <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10


spacing : Device -> Attribute msg
spacing { class } =
    El.spacing <|
        case class of
            Phone ->
                10

            Tablet ->
                15

            _ ->
                20
