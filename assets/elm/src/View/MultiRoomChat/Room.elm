module View.MultiRoomChat.Room exposing
    ( chatMaxHeight
    , init
    , inviteableUsers
    , onChangeMessage
    , onClickUser
    , onFocusMessage
    , onLoseFocusMessage
    , onSubmitMessage
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element, Orientation(..))
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Html.Attributes as Attr
import Type.ChatMessage exposing (ChatMessage)
import Type.Room exposing (Room)
import Type.User as User exposing (RegisteredUser)
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import UI.Padding as Padding
import UI.RoundedBorder as RoundedBorder
import View.Button as Button
import View.InputField as InputField
import View.Panel as Panel
import View.Tag as Tag


type Config msg
    = Config
        { currentUser : RegisteredUser
        , room : Room
        , maybeOnChange : Maybe (String -> msg)
        , maybeOnClick : Maybe (RegisteredUser -> msg)
        , maybeOnFocus : Maybe msg
        , maybeOnLoseFocus : Maybe msg
        , maybeOnSubmit : Maybe msg
        , inviteableUsers : List RegisteredUser
        , chatMaxHeight : Int
        }


init : RegisteredUser -> Room -> Config msg
init currentUser room =
    Config
        { currentUser = currentUser
        , room = room
        , maybeOnChange = Nothing
        , maybeOnClick = Nothing
        , maybeOnFocus = Nothing
        , maybeOnLoseFocus = Nothing
        , maybeOnSubmit = Nothing
        , inviteableUsers = []
        , chatMaxHeight = 0
        }


onChangeMessage : (String -> msg) -> Config msg -> Config msg
onChangeMessage toMsg (Config config) =
    Config { config | maybeOnChange = Just toMsg }


onClickUser : (RegisteredUser -> msg) -> Config msg -> Config msg
onClickUser toMsg (Config config) =
    Config { config | maybeOnClick = Just toMsg }


onFocusMessage : msg -> Config msg -> Config msg
onFocusMessage msg (Config config) =
    Config { config | maybeOnFocus = Just msg }


onLoseFocusMessage : msg -> Config msg -> Config msg
onLoseFocusMessage msg (Config config) =
    Config { config | maybeOnLoseFocus = Just msg }


onSubmitMessage : msg -> Config msg -> Config msg
onSubmitMessage msg (Config config) =
    Config { config | maybeOnSubmit = Just msg }


inviteableUsers : List RegisteredUser -> Config msg -> Config msg
inviteableUsers users (Config config) =
    Config { config | inviteableUsers = users }


chatMaxHeight : Int -> Config msg -> Config msg
chatMaxHeight height (Config config) =
    Config { config | chatMaxHeight = height }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    container device
        [ El.column
            [ El.height <|
                El.px config.chatMaxHeight
            , El.width <|
                El.fillPortion 3
            ]
            [ messagesView device (Config config)
            , occupantsTypingView device (Config config)
            , form device (Config config)
            ]
        , occupantsContainer device
            [ roomOccupants device config.currentUser config.room.members
            , lobbyOccupants device (Config config)
            ]
        ]


container : Device -> List (Element msg) -> Element msg
container { class, orientation } =
    case ( class, orientation ) of
        ( Phone, _ ) ->
            El.column
                [ El.width El.fill
                , El.spacing 10
                ]

        ( Tablet, Portrait ) ->
            El.column
                [ El.width El.fill
                , El.spacing 10
                ]

        _ ->
            El.row
                [ El.height El.fill
                , El.width El.fill
                , El.spacing 10
                , El.paddingXY 5 0
                ]


occupantsContainer : Device -> List (Element msg) -> Element msg
occupantsContainer ({ class, orientation } as device) =
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            El.column
                [ El.width El.fill
                , El.spacing 10
                ]

        ( Phone, Landscape ) ->
            El.row
                [ El.width El.fill
                , El.spacing 10
                ]

        _ ->
            El.row
                [ El.height El.fill
                , El.width <|
                    El.fillPortion 2
                , El.spacing 10
                ]



{- Messages -}


messagesView : Device -> Config msg -> Element msg
messagesView device (Config config) =
    El.el
        [ BackgroundColor.messages
        , El.clipY
        , El.scrollbarY
        , El.height El.fill
        , El.width El.fill
        , RoundedBorder.large device
        ]
        (El.column
            [ padding device
            , spacing device
            , El.height El.fill
            , El.width El.fill
            ]
         <|
            List.map (toMessage device config.currentUser) config.room.messages
        )


toMessage : Device -> RegisteredUser -> ChatMessage -> Element msg
toMessage device currentUser message =
    if User.match currentUser message.owner then
        userMessage device currentUser message

    else
        othersMessage device currentUser message


userMessage : Device -> RegisteredUser -> ChatMessage -> Element msg
userMessage device currentUser { text } =
    row
        [ emptySpace
        , column
            [ El.el
                [ El.alignRight ]
                (Tag.view device currentUser currentUser)
            , messageContent device El.alignRight currentUser text
            ]
        ]


othersMessage : Device -> RegisteredUser -> ChatMessage -> Element msg
othersMessage device currentUser { owner, text } =
    row
        [ column
            [ El.el [] <|
                Tag.view device currentUser owner
            , messageContent device El.alignLeft owner text
            ]
        , emptySpace
        ]


messageContent : Device -> Attribute msg -> RegisteredUser -> String -> Element msg
messageContent device alignment owner text =
    El.column
        [ alignment
        , padding device
        , spacing device
        , Background.color (User.bgColor owner)
        , Border.color (User.fgColor owner)
        , Border.width 1
        , Font.color (User.fgColor owner)
        , RoundedBorder.medium device
        ]
        (toParagraphs text)


toParagraphs : String -> List (Element msg)
toParagraphs text =
    String.split "\n" text
        |> List.map toParagraph


toParagraph : String -> Element msg
toParagraph text =
    El.el [ El.width El.fill ] <|
        El.paragraph
            [ El.width El.fill ]
            [ El.text text ]


row : List (Element msg) -> Element msg
row =
    El.row
        [ El.width El.fill ]


column : List (Element msg) -> Element msg
column =
    El.column
        [ El.spacing 5
        , El.width <|
            El.fillPortion 5
        ]


emptySpace : Element msg
emptySpace =
    El.el
        [ El.width <|
            El.fillPortion 1
        ]
        El.none



{- Occupants Typing -}


occupantsTypingView : Device -> Config msg -> Element msg
occupantsTypingView device (Config { currentUser, room }) =
    if room.occupantsTyping == [] then
        El.none

    else
        El.wrappedRow
            [ El.width El.fill
            , Padding.top 10
            ]
        <|
            El.el
                [ Font.bold
                , FontColor.label
                ]
                (El.text "Members Typing: ")
                :: List.map (Tag.view device currentUser) room.occupantsTyping



{- Form -}


form : Device -> Config msg -> Element msg
form ({ class, orientation } as device) config =
    let
        container_ =
            case ( class, orientation ) of
                ( Phone, Portrait ) ->
                    El.column

                _ ->
                    El.row
    in
    container_
        [ El.spacing 10
        , El.width El.fill
        , Padding.top 10
        ]
        [ inputField device config
        , submitButton device config
        ]


inputField : Device -> Config msg -> Element msg
inputField device (Config config) =
    InputField.init
        |> InputField.label "New Message"
        |> InputField.text config.room.message
        |> InputField.multiline True
        |> InputField.onChange config.maybeOnChange
        |> InputField.onFocus config.maybeOnFocus
        |> InputField.onLoseFocus config.maybeOnLoseFocus
        |> InputField.view device


submitButton : Device -> Config msg -> Element msg
submitButton device (Config config) =
    El.el
        [ El.alignBottom
        , El.centerX
        ]
        (Button.init
            |> Button.setLabel "Send Message"
            |> Button.setOnPress config.maybeOnSubmit
            |> Button.setEnabled (String.trim config.room.message /= "")
            |> Button.view device
        )



{- Room Occupants -}


roomOccupants : Device -> RegisteredUser -> List RegisteredUser -> Element msg
roomOccupants device currentUser occupants =
    Panel.init
        |> Panel.title "Room Occupants"
        |> Panel.element
            (El.column
                [ padding device
                , spacing device
                , El.width El.fill
                ]
                (List.map (Tag.view device currentUser) occupants)
            )
        |> Panel.view device



{- Lobby Occupants -}


lobbyOccupants : Device -> Config msg -> Element msg
lobbyOccupants device (Config config) =
    El.column
        [ El.height El.fill
        , El.width El.fill
        ]
        [ Panel.init
            |> Panel.title "Lobby Occupants"
            |> Panel.description
                [ [ El.text "Click on a user to invite them into the room. "
                  , El.text "Click them again to revoke the invitation."
                  ]
                ]
            |> Panel.element
                (El.column
                    [ padding device
                    , spacing device
                    , El.width El.fill
                    ]
                    (List.map (occupantView device (Config config)) config.inviteableUsers)
                )
            |> Panel.view device
        ]


occupantView : Device -> Config msg -> RegisteredUser -> Element msg
occupantView device (Config { maybeOnClick, currentUser }) user =
    case maybeOnClick of
        Nothing ->
            El.none

        Just onClick_ ->
            El.paragraph
                [ padding device
                , Background.color (User.bgColor user)
                , Border.color (User.fgColor user)
                , Border.width 1
                , El.mouseOver
                    [ Border.color (User.bgColor user)
                    , Border.shadow
                        { size = 1
                        , blur = 5
                        , color = User.bgColor user
                        , offset = ( 0, 0 )
                        }
                    ]
                , El.pointer
                , El.width El.fill
                , Event.onClick (onClick_ user)
                , Font.color (User.fgColor user)
                , RoundedBorder.small device
                ]
                [ El.text <|
                    User.username user
                        ++ (if User.isInvited user currentUser then
                                " (Invited)"

                            else
                                ""
                           )
                ]



{- Attributes -}


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


spacing : Device -> Attribute msg
spacing { class } =
    El.spacing <|
        case class of
            Phone ->
                5

            Tablet ->
                7

            _ ->
                10
