module Type.Example exposing
    ( Example(..)
    , toSrc
    )

{- Types -}


type Example
    = ConnectWithBadParams
    | ConnectWithGoodParams
    | JoinMultipleChannels
    | JoinWithBadParams
    | JoinWithGoodParams
    | MultiRoomChat
    | PushMultipleEvents
    | PushOneEvent
    | PushWithTimeout
    | ReceiveEvents
    | SimpleConnect
    | SimpleJoinAndLeave



{- Transform -}


toSrc : Example -> String
toSrc example =
    "https://github.com/phollyer/elm-phoenix-websocket-example/blob/master/assets/elm/src/Example/" ++ toModule example


toString : Example -> String
toString example =
    case example of
        ConnectWithBadParams ->
            "ConnectWithBadParams"

        ConnectWithGoodParams ->
            "ConnectWithGoodParams"

        JoinMultipleChannels ->
            "JoinMultipleChannels"

        JoinWithBadParams ->
            "JoinWithBadParams"

        JoinWithGoodParams ->
            "JoinWithGoodParams"

        MultiRoomChat ->
            "MultiRoomChat"

        PushMultipleEvents ->
            "PushMutipleEvents"

        PushOneEvent ->
            "PushOneEvent"

        PushWithTimeout ->
            "PushWithTimeout"

        ReceiveEvents ->
            "ReceiveEvents"

        SimpleConnect ->
            "SimpleConnect"

        SimpleJoinAndLeave ->
            "SimpleJoinAndLeave"


toModule : Example -> String
toModule example =
    toString example ++ ".elm"
