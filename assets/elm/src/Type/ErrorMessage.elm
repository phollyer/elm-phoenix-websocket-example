module Type.ErrorMessage exposing
    ( ErrorMessage(..)
    , toString
    )

import Type.RoomInvite exposing (RoomInvite)


type ErrorMessage
    = UsernameCannotBeBlank
    | UsernamePartsCannotBeLongerThan12Chars
    | BackgroundColorNotSelected
    | ForegroundColorNotSelected
    | RoomClosed RoomInvite


toString : ErrorMessage -> String
toString error =
    case error of
        BackgroundColorNotSelected ->
            "A Background Color must be seleceted"

        ForegroundColorNotSelected ->
            "A Foreground Color must be selected"

        UsernameCannotBeBlank ->
            "Username can not be empty"

        UsernamePartsCannotBeLongerThan12Chars ->
            "Username parts can not be longer than 12 chars"

        RoomClosed _ ->
            "That room has now closed"
