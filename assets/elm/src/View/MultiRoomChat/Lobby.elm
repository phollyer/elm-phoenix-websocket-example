module View.MultiRoomChat.Lobby exposing
    ( init
    , onAcceptRoomInvite
    , onCreateRoom
    , onDeclineRoomInvite
    , onDeleteRoom
    , onEnterRoom
    , onInviteErrorOk
    , onMouseEnterRoom
    , onMouseLeaveRoom
    , view
    )

import Colors.Alpha as Color
import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Type.Lobby as Lobby exposing (Lobby, RoomAction(..))
import Type.Room as Room exposing (Room)
import Type.User as User exposing (InviteState(..), RegisteredUser, RoomInvite, User(..))
import UI.Align as Align
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import UI.Padding as Padding
import UI.RoundedBorder as RoundedBorder
import UI.Spacing as Spacing
import Utils exposing (andMaybeEventWith, andMaybeEventWithArg)
import View.Button as Button
import View.Tag as Tag



{- Types -}


type Config msg
    = Config
        { user : RegisteredUser
        , lobby : Lobby
        , onCreateRoom : Maybe msg
        , onDeleteRoom : Maybe (Room -> msg)
        , onEnterRoom : Maybe (Room -> msg)
        , onMouseEnterRoom : Maybe (Room -> msg)
        , onMouseLeaveRoom : Maybe (Room -> msg)
        , onAcceptRoomInvite : Maybe (RoomInvite -> msg)
        , onDeclineRoomInvite : Maybe (RoomInvite -> msg)
        , onInviteErrorOk : Maybe (RegisteredUser -> RoomInvite -> msg)
        }



{- Build -}


init : RegisteredUser -> Lobby -> Config msg
init user lobby =
    Config
        { user = user
        , lobby = lobby
        , onCreateRoom = Nothing
        , onDeleteRoom = Nothing
        , onEnterRoom = Nothing
        , onMouseEnterRoom = Nothing
        , onMouseLeaveRoom = Nothing
        , onAcceptRoomInvite = Nothing
        , onDeclineRoomInvite = Nothing
        , onInviteErrorOk = Nothing
        }


onCreateRoom : Maybe msg -> Config msg -> Config msg
onCreateRoom msg (Config config) =
    Config { config | onCreateRoom = msg }


onDeleteRoom : (Room -> msg) -> Config msg -> Config msg
onDeleteRoom msg (Config config) =
    Config { config | onDeleteRoom = Just msg }


onEnterRoom : (Room -> msg) -> Config msg -> Config msg
onEnterRoom msg (Config config) =
    Config { config | onEnterRoom = Just msg }


onMouseEnterRoom : (Room -> msg) -> Config msg -> Config msg
onMouseEnterRoom msg (Config config) =
    Config { config | onMouseEnterRoom = Just msg }


onMouseLeaveRoom : (Room -> msg) -> Config msg -> Config msg
onMouseLeaveRoom msg (Config config) =
    Config { config | onMouseLeaveRoom = Just msg }


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
        , El.inFront <|
            if User.hasInvites config.user then
                roomInvite device (Config config)

            else
                El.none
        , Spacing.large device
        ]
        [ container device
            [ El.width El.fill
            , Spacing.large device
            ]
            [ El.column
                [ BackgroundColor.panel
                , El.height El.fill
                , El.width El.fill
                , FontColor.panel
                , Padding.xLarge device
                , RoundedBorder.large device
                , Spacing.large device
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



{- Received Room Invites -}


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
                [ El.height El.fill
                , El.width El.fill
                , Background.color (Color.white 0.5)
                , RoundedBorder.large device
                ]
                (El.column
                    [ Background.color (User.bgColor invite.from)
                    , Border.color (User.fgColor invite.from)
                    , Border.width 1
                    , El.centerX
                    , El.centerY
                    , Font.color (User.fgColor invite.from)
                    , Padding.medium device
                    , RoundedBorder.medium device
                    , Spacing.medium device
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
                        [ El.centerX
                        , Spacing.large device
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
                [ El.height El.fill
                , El.width El.fill
                , Background.color (Color.white 0.5)
                , RoundedBorder.large device
                ]
                (El.column
                    [ Background.color (User.bgColor user)
                    , Border.color (User.fgColor user)
                    , Border.width 1
                    , El.centerX
                    , El.centerY
                    , Font.color (User.fgColor user)
                    , Padding.medium device
                    , RoundedBorder.medium device
                    , Spacing.medium device
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
userView ({ class, orientation } as device) currentUser =
    El.column
        [ case ( class, orientation ) of
            ( Phone, Portrait ) ->
                Font.center

            _ ->
                Font.alignLeft
        , El.alignTop
        , El.width El.fill
        , Padding.topLarge device
        , Spacing.large device
        ]
        [ El.paragraph
            [ Spacing.medium device ]
            [ El.el
                [ Font.bold
                , FontColor.label
                ]
                (El.text "Username: ")
            , El.el
                [ FontColor.value ]
                (El.text (User.username currentUser))
            ]
        , El.column
            [ El.width El.fill
            , Spacing.large device
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
        [ El.width El.fill ]
        text



{- Create Room Button -}


createRoomBtn : Device -> Maybe msg -> Element msg
createRoomBtn ({ class, orientation } as device) maybeMsg =
    El.el
        [ El.height <|
            El.px <|
                case class of
                    Phone ->
                        50

                    Tablet ->
                        60

                    _ ->
                        70
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
                    |> Button.setAlignY Align.Middle
                    |> Button.view device



{- Lobby Occupants -}


occupantsView : Device -> RegisteredUser -> List RegisteredUser -> Element msg
occupantsView device currentUser occupants =
    El.column
        [ BackgroundColor.panel
        , El.width El.fill
        , FontColor.panel
        , Padding.large device
        , RoundedBorder.large device
        , Spacing.large device
        ]
        [ El.el
            [ El.centerX
            , FontColor.subTitle
            ]
            (El.text "Occupants")
        , El.wrappedRow
            [ Spacing.small device ]
            (List.map (Tag.view device currentUser) occupants)
        ]



{- Rooms -}


roomsView : Device -> Config msg -> Element msg
roomsView device ((Config c) as config) =
    case c.lobby.rooms of
        ( [], [] ) ->
            El.none

        ( currentUserRooms, [] ) ->
            roomsContainer device
                [ toRoomList device config ( c.user, currentUserRooms ) ]

        ( [], othersRooms ) ->
            roomsContainer device <|
                List.map (toRoomList device config) othersRooms

        ( currentUserRooms, othersRooms ) ->
            roomsContainer device <|
                toRoomList device config ( c.user, currentUserRooms )
                    :: List.map (toRoomList device config) othersRooms


roomsContainer : Device -> List (Element msg) -> Element msg
roomsContainer device rooms_ =
    El.el
        [ El.alignTop
        , El.width El.fill
        ]
        (El.column
            [ BackgroundColor.panel
            , El.width El.fill
            , FontColor.panel
            , Padding.large device
            , RoundedBorder.large device
            , Spacing.large device
            ]
            [ El.el
                [ El.centerX
                , FontColor.subTitle
                ]
                (El.text "Room List")
            , El.column
                [ El.width El.fill
                , Spacing.medium device
                ]
                rooms_
            ]
        )


toRoomList : Device -> Config msg -> ( RegisteredUser, List Room ) -> Element msg
toRoomList device ((Config { user }) as config) ( owner, rooms_ ) =
    El.column
        [ El.width El.fill
        , Spacing.large device
        ]
        [ El.el [] (Tag.view device user owner)
        , List.map (toRoom device config) rooms_
            |> El.wrappedRow
                [ El.width El.fill
                , Spacing.small device
                ]
        ]


toRoom : Device -> Config msg -> Room -> Element msg
toRoom device (Config config) room =
    El.row
        ([ Background.color (User.bgColor room.owner)
         , Border.color (User.fgColor room.owner)
         , Border.width 1
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
         , Font.color (User.fgColor room.owner)
         , El.inFront <|
            if Lobby.showRoomOccupants room config.lobby then
                El.el
                    [ El.width El.fill
                    , El.above <|
                        occupantsList device config.user room
                    ]
                    El.none

            else
                El.none
         , Padding.medium device
         , RoundedBorder.medium device
         , Spacing.medium device
         ]
            |> andMaybeEventWithArg config.onMouseEnterRoom room Event.onMouseEnter
            |> andMaybeEventWithArg config.onMouseLeaveRoom room Event.onMouseLeave
        )
        [ El.text <|
            "Occupants: "
                ++ (List.length room.members
                        |> String.fromInt
                   )
        , El.row
            [ El.alignRight
            , Spacing.medium device
            ]
            [ if Lobby.isEnteringRoom room config.lobby then
                El.text "Entering..."

              else
                maybeEnterBtn device room (Config config)
            , if Lobby.isDeletingRoom room config.lobby then
                El.text "Deleting..."

              else
                maybeDeleteBtn device room (Config config)
            ]
        ]


occupantsList : Device -> RegisteredUser -> Room -> Element msg
occupantsList device currentUser room =
    case room.members of
        [] ->
            El.none

        _ ->
            El.column
                [ Background.color (User.bgColor room.owner)
                , Border.color (User.fgColor room.owner)
                , Border.width 1
                , Font.color (User.fgColor room.owner)
                , Padding.small device
                , RoundedBorder.small device
                , Spacing.small device
                ]
                [ El.el
                    []
                    (El.text "Occupants")
                , El.wrappedRow
                    []
                    [ List.map (Tag.view device currentUser) room.members
                        |> El.wrappedRow
                            [ El.width El.fill
                            , Spacing.small device
                            ]
                    ]
                ]


maybeDeleteBtn : Device -> Room -> Config msg -> Element msg
maybeDeleteBtn device room (Config config) =
    if User.match config.user room.owner then
        case config.onDeleteRoom of
            Nothing ->
                El.none

            Just onDelete ->
                Button.init
                    |> Button.setLabel "Delete"
                    |> Button.setOnPress (Just (onDelete room))
                    |> Button.setType (Button.User room.owner)
                    |> Button.view device

    else
        El.none


maybeEnterBtn : Device -> Room -> Config msg -> Element msg
maybeEnterBtn device room (Config config) =
    if User.match config.user room.owner || Room.isOpen room then
        case config.onEnterRoom of
            Nothing ->
                El.none

            Just onEnter ->
                Button.init
                    |> Button.setLabel "Enter"
                    |> Button.setOnPress (Just (onEnter room))
                    |> Button.setType (Button.User room.owner)
                    |> Button.view device

    else
        El.none
