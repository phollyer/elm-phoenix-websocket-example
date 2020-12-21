module Type.ErrorMessage exposing
    ( ErrorMessage(..)
    , toString
    )

import Type.RoomInvite exposing (RoomInvite)


type ErrorMessage
    = UsernameCannotBeBlank
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

        RoomClosed _ ->
            "That room has now closed"
