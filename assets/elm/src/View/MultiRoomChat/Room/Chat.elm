module View.MultiRoomChat.Room.Chat exposing
    ( init
    , membersTyping
    , messages
    , messagesContainerMaxHeight
    , onChange
    , onFocus
    , onLoseFocus
    , onSubmit
    , userText
    , view
    )

import Element as El exposing (Device, DeviceClass(..), Element, Orientation(..))
import Element.Border as Border
import Element.Font as Font
import Html.Attributes as Attr
import Type.ChatMessage exposing (ChatMessage)
import Type.Room exposing (Room)
import Type.User exposing (RegisteredUser)
import UI.BackgroundColor as BackgroundColor
import UI.FontColor as FontColor
import UI.Padding as Padding
import View.MultiRoomChat.Room.Form as MessageForm
import View.MultiRoomChat.Room.Messages as Messages
import View.Tag as Tag



{- Model -}


type Config msg
    = Config
        { user : RegisteredUser
        , room : Room
        , userText : String
        , membersTyping : List RegisteredUser
        , messages : List ChatMessage
        , messagesContainerMaxHeight : Int
        , onChange : Maybe (String -> msg)
        , onFocus : Maybe msg
        , onLoseFocus : Maybe msg
        , onSubmit : Maybe msg
        }


init : RegisteredUser -> Room -> Config msg
init user room =
    Config
        { user = user
        , room = room
        , userText = ""
        , membersTyping = []
        , messages = []
        , messagesContainerMaxHeight = 0
        , onChange = Nothing
        , onFocus = Nothing
        , onLoseFocus = Nothing
        , onSubmit = Nothing
        }


userText : String -> Config msg -> Config msg
userText text (Config config) =
    Config { config | userText = text }


membersTyping : List RegisteredUser -> Config msg -> Config msg
membersTyping members (Config config) =
    Config { config | membersTyping = members }


messages : List ChatMessage -> Config msg -> Config msg
messages messages_ (Config config) =
    Config { config | messages = messages_ }


messagesContainerMaxHeight : Int -> Config msg -> Config msg
messagesContainerMaxHeight height (Config config) =
    Config { config | messagesContainerMaxHeight = height }


onChange : (String -> msg) -> Config msg -> Config msg
onChange toMsg (Config config) =
    Config { config | onChange = Just toMsg }


onFocus : msg -> Config msg -> Config msg
onFocus toMsg (Config config) =
    Config { config | onFocus = Just toMsg }


onLoseFocus : msg -> Config msg -> Config msg
onLoseFocus toMsg (Config config) =
    Config { config | onLoseFocus = Just toMsg }


onSubmit : msg -> Config msg -> Config msg
onSubmit msg (Config config) =
    Config { config | onSubmit = Just msg }



{- View -}


view : Device -> Config msg -> Element msg
view device (Config config) =
    El.column
        [ El.height <|
            El.px config.messagesContainerMaxHeight
        , El.width El.fill
        ]
        [ messagesView device (Config config)
        , membersTypingView device config.user config.membersTyping
        , form device (Config config)
        ]


membersTypingView : Device -> RegisteredUser -> List RegisteredUser -> Element msg
membersTypingView device currentUser membersTyping_ =
    if membersTyping_ == [] then
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
                :: List.map (Tag.view device currentUser) membersTyping_



{- Messages -}


messagesView : Device -> Config msg -> Element msg
messagesView device (Config config) =
    El.el
        [ BackgroundColor.messages
        , Border.rounded 10
        , El.htmlAttribute <|
            Attr.id "message-list"
        , El.clipY
        , El.scrollbarY
        , El.height El.fill
        , El.width El.fill
        ]
        (Messages.init config.user
            |> Messages.messages config.messages
            |> Messages.view device
        )



{- Form -}


form : Device -> Config msg -> Element msg
form device (Config config) =
    MessageForm.init
        |> MessageForm.text config.userText
        |> MessageForm.onChange config.onChange
        |> MessageForm.onFocus config.onFocus
        |> MessageForm.onLoseFocus config.onLoseFocus
        |> MessageForm.onSubmit config.onSubmit
        |> MessageForm.view device
