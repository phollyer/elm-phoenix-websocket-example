module View.MultiRoomChat.Room.Messages exposing
    ( init
    , messages
    , view
    )

import Element as El exposing (Attribute, Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Type.ChatMessage exposing (ChatMessage)
import Type.User as User exposing (RegisteredUser)
import View.Tag as Tag



{- Model -}


type Config
    = Config
        { user : RegisteredUser
        , messages : List ChatMessage
        }


init : RegisteredUser -> Config
init user =
    Config
        { user = user
        , messages = []
        }


messages : List ChatMessage -> Config -> Config
messages list (Config config) =
    Config { config | messages = list }



{- View -}


view : Device -> Config -> Element msg
view device (Config config) =
    El.column
        [ padding device
        , spacing device
        , El.height El.fill
        , El.width El.fill
        ]
    <|
        List.map (toMessage device config.user) config.messages


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



{- Message -}


messageContent : Device -> Attribute msg -> RegisteredUser -> String -> Element msg
messageContent device alignment owner text =
    El.column
        [ alignment
        , padding device
        , spacing device
        , roundedBorders device
        , Background.color (User.bgColor owner)
        , Border.color (User.fgColor owner)
        , Border.width 1
        , Font.color (User.fgColor owner)
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
