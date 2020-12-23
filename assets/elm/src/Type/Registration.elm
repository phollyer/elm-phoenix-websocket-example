module Type.Registration exposing
    ( Registration
    , bgColorSelected
    , encode
    , fgColorSelected
    , init
    , processErrors
    , usernameChanged
    , validate
    )

import Element exposing (Color)
import Json.Encode as JE exposing (Value)
import Type.Color as Color
import Type.ErrorMessage exposing (ErrorMessage(..))
import Type.TwoTrack exposing (TwoTrack(..), bind)


type alias Registration =
    { username : String
    , usernameError : Maybe ErrorMessage
    , backgroundColor : Maybe Color
    , backgroundColorError : Maybe ErrorMessage
    , foregroundColor : Maybe Color
    , foregroundColorError : Maybe ErrorMessage
    }


init : Registration
init =
    { username = ""
    , usernameError = Nothing
    , backgroundColor = Nothing
    , backgroundColorError = Nothing
    , foregroundColor = Nothing
    , foregroundColorError = Nothing
    }


usernameChanged : String -> Registration -> Registration
usernameChanged name registration =
    { registration
        | username = name
        , usernameError =
            if String.trim name == "" then
                registration.usernameError

            else
                Nothing
    }


bgColorSelected : Color -> Registration -> Registration
bgColorSelected color registration =
    { registration
        | backgroundColor = Just color
        , backgroundColorError = Nothing
    }


fgColorSelected : Color -> Registration -> Registration
fgColorSelected color registration =
    { registration
        | foregroundColor = Just color
        , foregroundColorError = Nothing
    }


processErrors : List ErrorMessage -> Registration -> Registration
processErrors errors registration =
    List.foldl
        (\error reg ->
            case error of
                UsernameCannotBeBlank ->
                    { reg | usernameError = Just UsernameCannotBeBlank }

                UsernamePartsCannotBeLongerThan12Chars ->
                    { reg | usernameError = Just UsernamePartsCannotBeLongerThan12Chars }

                BackgroundColorNotSelected ->
                    { reg | backgroundColorError = Just BackgroundColorNotSelected }

                ForegroundColorNotSelected ->
                    { reg | foregroundColorError = Just ForegroundColorNotSelected }

                _ ->
                    reg
        )
        { registration
            | usernameError = Nothing
            , backgroundColorError = Nothing
            , foregroundColorError = Nothing
        }
        errors



{- Validation -}


type Field
    = Username String
    | BackgroundColor Color
    | ForegroundColor Color


validate : Registration -> TwoTrack Field
validate { username, backgroundColor, foregroundColor } =
    Success []
        |> bind validateUsername username
        |> bind validateBackgroundColor backgroundColor
        |> bind validateForegroundColor foregroundColor


validateUsername : String -> TwoTrack Field
validateUsername username =
    if String.trim username == "" then
        Failure [ UsernameCannotBeBlank ]

    else if usernamePartMoreThan12Chars username then
        Failure [ UsernamePartsCannotBeLongerThan12Chars ]

    else
        Success [ Username (String.trim username) ]


usernamePartMoreThan12Chars : String -> Bool
usernamePartMoreThan12Chars username =
    String.words username
        |> List.filter (\word -> String.length word > 12)
        |> List.isEmpty
        |> not


validateBackgroundColor : Maybe Color -> TwoTrack Field
validateBackgroundColor maybeColor =
    case maybeColor of
        Nothing ->
            Failure [ BackgroundColorNotSelected ]

        Just color ->
            Success [ BackgroundColor color ]


validateForegroundColor : Maybe Color -> TwoTrack Field
validateForegroundColor maybeColor =
    case maybeColor of
        Nothing ->
            Failure [ ForegroundColorNotSelected ]

        Just color ->
            Success [ ForegroundColor color ]



{- Encoder -}


encode : List Field -> Value
encode fields =
    JE.object <|
        List.map encodeField fields


encodeField : Field -> ( String, Value )
encodeField field =
    case field of
        Username username ->
            ( "username", JE.string username )

        BackgroundColor color ->
            ( "background_color", Color.encode color )

        ForegroundColor color ->
            ( "foreground_color", Color.encode color )
