module Type.TwoTrack exposing
    ( TwoTrack(..)
    , bind
    )

import Type.ErrorMessage exposing (ErrorMessage)


type TwoTrack entity
    = Success (List entity)
    | Failure (List ErrorMessage)


bind : (a -> TwoTrack entity) -> a -> TwoTrack entity -> TwoTrack entity
bind func input twoTrack =
    case ( func input, twoTrack ) of
        ( Failure e, Failure f ) ->
            Failure (List.append e f)

        ( Failure e, _ ) ->
            Failure e

        ( _, Failure f ) ->
            Failure f

        ( Success a, Success b ) ->
            Success (List.append a b)
