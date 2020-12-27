module Type.ErrorMessage exposing
    ( ErrorMessage(..)
    , toString
    )


type ErrorMessage
    = UsernameCannotBeBlank
    | UsernamePartsCannotBeMoreThan12Chars
    | UsernameCannotBeMoreThan25Chars
    | BackgroundColorNotSelected
    | ForegroundColorNotSelected
    | RoomClosed


toString : ErrorMessage -> String
toString error =
    case error of
        UsernameCannotBeBlank ->
            "Username can not be empty"

        UsernamePartsCannotBeMoreThan12Chars ->
            "Username parts can not be longer than 12 chars"

        UsernameCannotBeMoreThan25Chars ->
            "Username can not be longer than 25 chars"

        BackgroundColorNotSelected ->
            "A Background Color must be seleceted"

        ForegroundColorNotSelected ->
            "A Foreground Color must be selected"

        RoomClosed ->
            "That room has now closed"
