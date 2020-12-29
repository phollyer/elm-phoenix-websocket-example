module View.MultiRoomChat.Lobby exposing
    ( init
    , onAcceptRoomInvite
    , onCreateRoom
    , onDeclineRoomInvite
    , onDeleteRoom
    , onEnterRoom
    , onInviteErrorOk
    , onMouseEnterRoom
    , view
    )

import Colors.Alpha as Color
import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Type.ErrorMessage as ErrorMessage exposing (ErrorMessage(..))
import Type.Lobby exposing (Lobby)
import Type.Room exposing (Room)
import Type.User as User exposing (InviteState(..), RegisteredUser, RoomInvite, User(..))
import UI.Align as Align
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import UI.RoundedBorder as RoundedBorder
import View.Button as Button
import View.MultiRoomChat.User as UserView
import View.Tag as Tag



{- Model -}


type Config msg
    = Config
        { user : RegisteredUser
        , lobby : Lobby
        , onCreateRoom : Maybe msg
        , onDeleteRoom : Maybe (Room -> msg)
        , onEnterRoom : Maybe (Room -> msg)
        , onMouseEnterRoom : Maybe (Maybe Room -> msg)
        , onAcceptRoomInvite : Maybe (RoomInvite -> msg)
        , onDeclineRoomInvite : Maybe (RoomInvite -> msg)
        , onInviteErrorOk : Maybe (RegisteredUser -> RoomInvite -> msg)
        }


init : RegisteredUser -> Lobby -> Config msg
init user lobby =
    Config
        { user = user
        , lobby = lobby
        , onCreateRoom = Nothing
        , onDeleteRoom = Nothing
        , onEnterRoom = Nothing
        , onMouseEnterRoom = Nothing
        , onAcceptRoomInvite = Nothing
        , onDeclineRoomInvite = Nothing
        , onInviteErrorOk = Nothing
        }


onCreateRoom : Maybe msg -> Config msg -> Config msg
onCreateRoom msg (Config config) =
    Config { config | onCreateRoom = msg }


onDeleteRoom : Maybe (Room -> msg) -> Config msg -> Config msg
onDeleteRoom msg (Config config) =
    Config { config | onDeleteRoom = msg }


onEnterRoom : Maybe (Room -> msg) -> Config msg -> Config msg
onEnterRoom msg (Config config) =
    Config { config | onEnterRoom = msg }


onMouseEnterRoom : (Maybe Room -> msg) -> Config msg -> Config msg
onMouseEnterRoom msg (Config config) =
    Config { config | onMouseEnterRoom = Just msg }


onAcceptRoomInvite : (RoomInvite -> msg) -> Config msg -> Config msg
onAcceptRoomInvite msg (Config config) =
    Config { config | onAcceptRoomInvite = Just msg }


onDeclineRoomInvite : (RoomInvite -> msg) -> Config msg -> Config msg
onDeclineRoomInvite msg (Config config) =
    Config { config | onDeclineRoomInvite = Just msg }


onInviteErrorOk : (RegisteredUser -> RoomInvite -> msg) -> Config msg -> Config msg
onInviteErrorOk toMsg (Config config) =
    Config { config | onInviteErrorOk = Just toMsg }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.column
        [ El.width El.fill
        , El.spacing 15
        , El.inFront <|
            if User.hasInvites config.user then
                roomInvite device (Config config)

            else
                El.none
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
        , occupantsView device config.user config.lobby.occupants
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
    case User.invitesReceived config.user of
        ( Expired, invite ) :: _ ->
            inviteErrorView device config.onInviteErrorOk invite config.user

        ( state, invite ) :: _ ->
            inviteView device state invite (Config config)

        [] ->
            El.none


inviteView : Device -> InviteState -> RoomInvite -> Config msg -> Element msg
inviteView device state invite (Config config) =
    case ( config.onAcceptRoomInvite, config.onDeclineRoomInvite ) of
        ( Just onAccept, Just onDecline ) ->
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
                    , Background.color (User.bgColor invite.from)
                    , Border.color (User.fgColor invite.from)
                    , Border.width 1
                    , El.centerX
                    , El.centerY
                    , Font.color (User.fgColor invite.from)
                    ]
                    [ El.el
                        [ El.centerX ]
                        (El.text "Room Invite")
                    , El.paragraph
                        []
                        [ El.text "You have received an invitation from "
                        , El.text (User.username invite.from)
                        , El.text " to join them in their room."
                        ]
                    , El.row
                        [ spacing device
                        , El.centerX
                        ]
                        [ case state of
                            Declining ->
                                El.text "Declining Invite..."

                            _ ->
                                Button.init
                                    |> Button.setLabel "Decline"
                                    |> Button.setOnPress (Just (onDecline invite))
                                    |> Button.setType (Button.User invite.from)
                                    |> Button.view device
                        , case state of
                            Accepting ->
                                El.text "Accepting Invite..."

                            _ ->
                                Button.init
                                    |> Button.setLabel "Accept"
                                    |> Button.setOnPress (Just (onAccept invite))
                                    |> Button.setType (Button.User invite.from)
                                    |> Button.view device
                        ]
                    ]
                )

        _ ->
            El.none


inviteErrorView : Device -> Maybe (RegisteredUser -> RoomInvite -> msg) -> RoomInvite -> RegisteredUser -> Element msg
inviteErrorView device maybeMsg invite user =
    case maybeMsg of
        Just toMsg ->
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
                    , Background.color (User.bgColor user)
                    , Border.color (User.fgColor user)
                    , Border.width 1
                    , El.centerX
                    , El.centerY
                    , Font.color (User.fgColor user)
                    ]
                    [ El.el
                        [ El.centerX ]
                        (El.text "Error")
                    , El.paragraph
                        []
                        [ El.text "The invite has expired." ]
                    , Button.init
                        |> Button.setLabel "OK"
                        |> Button.setOnPress (Just (toMsg user invite))
                        |> Button.setType (Button.User user)
                        |> Button.view device
                    ]
                )

        _ ->
            El.none



{- Lobby User -}


userView : Device -> RegisteredUser -> Element msg
userView device currentUser =
    El.column
        [ alignFont device
        , El.alignTop
        , El.spacing 20
        , El.width El.fill
        ]
        [ UserView.init
            |> UserView.username (User.username currentUser)
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
    El.el
        [ El.height <|
            El.px <|
                case class of
                    Phone ->
                        20

                    Tablet ->
                        30

                    _ ->
                        40
        , El.width El.fill
        ]
    <|
        case maybeMsg of
            Nothing ->
                El.el
                    [ case ( class, orientation ) of
                        ( Phone, Landscape ) ->
                            El.alignLeft

                        _ ->
                            El.centerX
                    , El.centerY
                    ]
                    (El.text "Creating Room...")

            Just _ ->
                Button.init
                    |> Button.setLabel "Create A Room"
                    |> Button.setOnPress maybeMsg
                    |> Button.setAlignX
                        (case ( class, orientation ) of
                            ( Phone, Landscape ) ->
                                Align.Left

                            _ ->
                                Align.Center
                        )
                    |> Button.view device



{- Lobby Members -}


occupantsView : Device -> RegisteredUser -> List RegisteredUser -> Element msg
occupantsView device currentUser occupants =
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
            (El.text "Occupants")
        , El.wrappedRow
            [ El.spacing 10 ]
            (List.map (toOccupant device currentUser) occupants)
        ]


toOccupant : Device -> RegisteredUser -> RegisteredUser -> Element msg
toOccupant device currentUser user =
    El.row
        []
        [ Tag.view device currentUser user ]



{- Rooms -}


roomsView : Device -> Config msg -> Element msg
roomsView device ((Config c) as config) =
    case c.lobby.rooms of
        ( [], [] ) ->
            El.none

        ( currentUserRooms, [] ) ->
            roomsContainer
                [ toRoomList device config ( c.user, currentUserRooms ) ]

        ( [], othersRooms ) ->
            roomsContainer <|
                List.map (toRoomList device config) othersRooms

        ( currentUserRooms, othersRooms ) ->
            roomsContainer <|
                toRoomList device config ( c.user, currentUserRooms )
                    :: List.map (toRoomList device config) othersRooms


roomsContainer : List (Element msg) -> Element msg
roomsContainer rooms_ =
    El.el
        [ El.alignTop
        , El.width El.fill
        ]
        (El.column
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
                rooms_
            ]
        )


toRoomList : Device -> Config msg -> ( RegisteredUser, List Room ) -> Element msg
toRoomList device ((Config { user }) as config) ( owner, rooms_ ) =
    El.column
        [ El.spacing 10
        , El.width El.fill
        ]
        [ El.el [] (Tag.view device user owner)
        , List.map (toRoom device config) rooms_
            |> El.wrappedRow
                [ El.spacing 10
                , El.width El.fill
                ]
        ]


toRoom : Device -> Config msg -> Room -> Element msg
toRoom device (Config config) room =
    El.row
        (List.append
            [ Border.rounded 10
            , Border.width 1
            , El.padding 10
            , El.spacing 10
            , El.width El.fill
            ]
            (roomAttrs room config.onMouseEnterRoom)
            |> List.append (roomOccupantsView device config.user config.lobby.selectedRoom room)
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
            [ maybeEnterBtn device config.onEnterRoom config.user room
            , maybeDeleteBtn device config.onDeleteRoom config.user room
            ]
        ]


roomOccupantsView : Device -> RegisteredUser -> Maybe Room -> Room -> List (Attribute msg)
roomOccupantsView device currentUser maybeForRoom room =
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


occupantsList : Device -> RegisteredUser -> Room -> Element msg
occupantsList device currentUser room =
    case room.members of
        [] ->
            El.none

        _ ->
            El.wrappedRow
                [ padding device
                , Background.color (User.bgColor room.owner)
                , Border.color (User.fgColor room.owner)
                , Border.width 1
                , RoundedBorder.small device
                ]
                [ List.map (Tag.view device currentUser) room.members
                    |> El.wrappedRow
                        [ El.spacing 10
                        , El.width El.fill
                        ]
                    |> El.el [ El.width El.fill ]
                ]


maybeDeleteBtn : Device -> Maybe (Room -> msg) -> RegisteredUser -> Room -> Element msg
maybeDeleteBtn device maybeToOnDelete currentUser room =
    if User.match currentUser room.owner then
        case maybeToOnDelete of
            Nothing ->
                El.text "Deleting Room..."

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
                El.text "Entering Room..."

            Just onClick_ ->
                Button.init
                    |> Button.setLabel "Enter"
                    |> Button.setOnPress (Just (onClick_ room))
                    |> Button.setType (Button.User room.owner)
                    |> Button.view device

    else
        El.none



{- Attributes -}


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
