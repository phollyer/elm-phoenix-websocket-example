module Type.TwoTrack exposing
    ( TwoTrack(..)
    , bind
    )

import Type.ErrorMessage exposing (ErrorMessage)



{- Types -}


type TwoTrack entity
    = Success (List entity)
    | Failure (List ErrorMessage)



{- Build -}


bind : (result -> TwoTrack entity) -> (a -> result) -> a -> TwoTrack entity -> TwoTrack entity
bind switch func input twoTrack =
    case ( switch <| func input, twoTrack ) of
        ( Success a, Success b ) ->
            Success (List.append a b)

        ( Failure e, Failure f ) ->
            Failure (List.append e f)

        ( Failure e, _ ) ->
            Failure e

        ( _, Failure f ) ->
            Failure f
