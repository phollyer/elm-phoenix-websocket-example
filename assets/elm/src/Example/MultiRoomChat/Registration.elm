module Example.MultiRoomChat.Registration exposing (..)

import Element as El exposing (Color, Device, Element)
import Json.Encode as JE exposing (Value)
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), joinConfig)
import Types exposing (User, decodeUser)
import Utils exposing (updatePhoenixWith)
import View.MultiRoomChat.Lobby.Registration as Registration



{- Model -}


type Model
    = Model
        { phoenix : Phoenix.Model
        , username : String
        , backgroundColor : Maybe Color
        , foregroundColor : Maybe Color
        }


init : Phoenix.Model -> Model
init phoenix =
    Model
        { phoenix = phoenix
        , username = ""
        , backgroundColor = Nothing
        , foregroundColor = Nothing
        }


toPhoenix : Model -> Phoenix.Model
toPhoenix (Model { phoenix }) =
    phoenix



{- Update -}


type OutMsg
    = Empty
    | JoinLobby Phoenix.Model User


type Msg
    = GotBackgroundColorSelection Color
    | GotForegroundColorSelection Color
    | GotUsernameChange String
    | GotJoinLobby
    | PhoenixMsg Phoenix.Msg


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg (Model model) =
    case msg of
        GotUsernameChange name ->
            ( Model { model | username = name }, Cmd.none, Empty )

        GotBackgroundColorSelection color ->
            ( Model { model | backgroundColor = Just color }, Cmd.none, Empty )

        GotForegroundColorSelection color ->
            ( Model { model | foregroundColor = Just color }, Cmd.none, Empty )

        GotJoinLobby ->
            case validateUserInput (Model model) of
                Success fields ->
                    updatePhoenixWith PhoenixMsg (Model model) <|
                        Phoenix.join "example:lobby" <|
                            Phoenix.setJoinConfig
                                { joinConfig
                                    | topic = "example:lobby"
                                    , events = [ "room_list" ]
                                    , payload = encodeFields fields
                                }
                                model.phoenix

                _ ->
                    ( Model model, Cmd.none, Empty )

        PhoenixMsg subMsg ->
            let
                ( newModel, cmd, phoenixMsg ) =
                    Phoenix.update subMsg model.phoenix
                        |> Phoenix.updateWith PhoenixMsg model
            in
            case phoenixMsg of
                ChannelResponse (JoinOk "example:lobby" payload) ->
                    case decodeUser payload of
                        Ok user ->
                            ( Model newModel, cmd, JoinLobby newModel.phoenix user )

                        Err error ->
                            ( Model newModel, cmd, Empty )

                _ ->
                    ( Model newModel, cmd, Empty )


updatePhoenixWith : (Phoenix.Msg -> Msg) -> Model -> ( Phoenix.Model, Cmd Phoenix.Msg ) -> ( Model, Cmd Msg, OutMsg )
updatePhoenixWith toMsg (Model model) ( phoenix, phoenixCmd ) =
    ( Model { model | phoenix = phoenix }, Cmd.map toMsg phoenixCmd, Empty )



{- Validation -}


isValid : Model -> Bool
isValid model =
    case validateUserInput model of
        Success _ ->
            True

        Failure _ ->
            False


validateUserInput : Model -> TwoTrack
validateUserInput (Model { username, backgroundColor, foregroundColor }) =
    Success []
        |> bind validateUsername username
        |> bind validateBackgroundColor backgroundColor
        |> bind validateForegroundColor foregroundColor


bind : (a -> TwoTrack) -> a -> TwoTrack -> TwoTrack
bind switch input twoTrack =
    case ( switch input, twoTrack ) of
        ( Success a, Success b ) ->
            Success (List.append a b)

        ( Failure e, Failure f ) ->
            Failure (List.append e f)

        ( Failure e, _ ) ->
            Failure e

        ( _, Failure f ) ->
            Failure f


type TwoTrack
    = Success (List Field)
    | Failure (List ErrorMessage)


type Field
    = Username String
    | BackgroundColor Color
    | ForegroundColor Color


type ErrorMessage
    = UsernameCannotBeBlank
    | BackgroundColorNotSelected
    | ForegroundColorNotSelected


validateUsername : String -> TwoTrack
validateUsername username =
    if username == "" then
        Failure [ UsernameCannotBeBlank ]

    else
        Success [ Username username ]


validateBackgroundColor : Maybe Color -> TwoTrack
validateBackgroundColor maybeColor =
    case maybeColor of
        Nothing ->
            Failure [ BackgroundColorNotSelected ]

        Just color ->
            Success [ BackgroundColor color ]


validateForegroundColor : Maybe Color -> TwoTrack
validateForegroundColor maybeColor =
    case maybeColor of
        Nothing ->
            Failure [ ForegroundColorNotSelected ]

        Just color ->
            Success [ ForegroundColor color ]



{- Encoder -}


encodeFields : List Field -> Value
encodeFields fields =
    JE.object <|
        List.map encodeField fields


encodeField : Field -> ( String, Value )
encodeField field =
    case field of
        Username username ->
            ( "username", JE.string username )

        BackgroundColor color ->
            ( "background_color", encodeColor color )

        ForegroundColor color ->
            ( "foreground_color", encodeColor color )


encodeColor : Color -> Value
encodeColor color =
    let
        rgba =
            El.toRgb color
    in
    JE.object
        [ ( "red", JE.float rgba.red )
        , ( "green", JE.float rgba.green )
        , ( "blue", JE.float rgba.blue )
        , ( "alpha", JE.float rgba.alpha )
        ]



{- Subscriptions -}


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Phoenix.subscriptions model.phoenix
        |> Sub.map PhoenixMsg



{- View -}


view : Device -> Model -> Element Msg
view device (Model model) =
    Registration.init
        |> Registration.username model.username
        |> Registration.selectedBackgroundColor model.backgroundColor
        |> Registration.selectedForegroundColor model.foregroundColor
        |> Registration.onChange GotUsernameChange
        |> Registration.onBackgroundColorChange GotBackgroundColorSelection
        |> Registration.onForegroundColorChange GotForegroundColorSelection
        |> Registration.onSubmit GotJoinLobby
        |> Registration.isSubmittable (isValid (Model model))
        |> Registration.view device
