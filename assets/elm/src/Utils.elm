module Utils exposing
    ( andMaybeEventWith
    , andMaybeEventWithArg
    , batch
    , updatePhoenixSessionWith
    , updatePhoenixWith
    )

import Element exposing (Attribute)
import Phoenix
import Session exposing (Session)


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


updatePhoenixSessionWith : (Phoenix.Msg -> msg) -> { model | session : Session } -> ( Phoenix.Model, Cmd Phoenix.Msg, Phoenix.PhoenixMsg ) -> ( { model | session : Session }, Cmd msg )
updatePhoenixSessionWith toMsg model ( phoenix, phoenixCmd, _ ) =
    ( { model | session = Session.updatePhoenix phoenix model.session }
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
