module View.Tag exposing (view)

import Element as El exposing (Attribute, Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Type.User as User exposing (RegisteredUser)
import UI.Padding as Padding
import UI.RoundedBorder as RoundedBorder


view : Device -> RegisteredUser -> RegisteredUser -> Element msg
view device currentUser user =
    El.paragraph
        [ Background.color (User.bgColor user)
        , Border.width 1
        , Border.color (User.fgColor user)
        , Font.color (User.fgColor user)
        , Padding.small device
        , RoundedBorder.small device
        ]
        [ El.text <|
            if User.match currentUser user then
                "You"

            else
                User.username user
        ]
