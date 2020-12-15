module Utils exposing
    ( andMaybeEventWith
    , andMaybeEventWithArg
    , batch
    , updatePhoenixWith
    )

import Element exposing (Attribute)
import Phoenix


batch : List (Cmd msg) -> ( model, Cmd msg ) -> ( model, Cmd msg )
batch cmds ( model, cmd ) =
    ( model
    , Cmd.batch (cmd :: cmds)
    )


updatePhoenixWith : (Phoenix.Msg -> msg) -> { model | phoenix : Phoenix.Model } -> ( Phoenix.Model, Cmd Phoenix.Msg ) -> ( { model | phoenix : Phoenix.Model }, Cmd msg )
updatePhoenixWith toMsg model ( phoenix, phoenixCmd ) =
    ( { model | phoenix = phoenix }
    , Cmd.map toMsg phoenixCmd
    )



{- View -}


andMaybeEventWith : Maybe msg -> (msg -> Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
andMaybeEventWith maybeMsg toEvent attrs =
    case maybeMsg of
        Nothing ->
            attrs

        Just msg ->
            toEvent msg :: attrs


andMaybeEventWithArg : Maybe (a -> msg) -> a -> (msg -> Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
andMaybeEventWithArg maybeMsg arg toEvent attrs =
    case maybeMsg of
        Nothing ->
            attrs

        Just msg ->
            toEvent (msg arg) :: attrs
