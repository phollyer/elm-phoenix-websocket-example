module View.Tag exposing (view)

import Element as El exposing (Attribute, Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Type.User as User exposing (RegisteredUser)


view : Device -> RegisteredUser -> RegisteredUser -> Element msg
view device currentUser user =
    let
        _ =
            Debug.log "currentUser" currentUser

        _ =
            Debug.log "user" user
    in
    El.paragraph
        [ padding device
        , roundedBorders device
        , Background.color (User.bgColor user)
        , Border.width 1
        , Border.color (User.fgColor user)
        , Font.color (User.fgColor user)
        ]
        [ El.text <|
            if User.match currentUser user then
                "You"

            else
                User.username user
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
