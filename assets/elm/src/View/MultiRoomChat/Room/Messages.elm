module View.MultiRoomChat.Room.Messages exposing
    ( init
    , messages
    , user
    , view
    )

import Colors.Opaque as Color
import Element as El exposing (Attribute, Color, Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Type.ChatMessage exposing (ChatMessage)
import Type.User as User exposing (User)
import UI.FontColor as FontColor



{- Model -}


type Config
    = Config
        { user : User
        , messages : List ChatMessage
        }


init : Config
init =
    Config
        { user = User.init
        , messages = []
        }


user : User -> Config -> Config
user user_ (Config config) =
    Config { config | user = user_ }


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


toMessage : Device -> User -> ChatMessage -> Element msg
toMessage device currentUser message =
    if currentUser.id == message.owner.id then
        userMessage device message

    else
        othersMessage device message


userMessage : Device -> ChatMessage -> Element msg
userMessage device { owner, text } =
    row
        [ emptySpace
        , column
            [ username device El.alignRight owner
            , messageContent device El.alignRight owner text
            ]
        ]


othersMessage : Device -> ChatMessage -> Element msg
othersMessage device { owner, text } =
    row
        [ column
            [ username device El.alignLeft owner
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



{- Username -}


username : Device -> Attribute msg -> User -> Element msg
username device alignment user_ =
    El.el
        [ alignment
        , padding device
        , roundedBorders device
        , Background.color user_.backgroundColor
        , Border.color user_.foregroundColor
        , Border.width 1
        , Font.color user_.foregroundColor
        ]
        (El.text user_.username)



{- Message -}


messageContent : Device -> Attribute msg -> User -> String -> Element msg
messageContent device alignment owner text =
    El.column
        [ alignment
        , padding device
        , spacing device
        , roundedBorders device
        , Background.color owner.backgroundColor
        , Border.color owner.foregroundColor
        , Border.width 1
        , Font.color owner.foregroundColor
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
