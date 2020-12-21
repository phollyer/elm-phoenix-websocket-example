module Example.MultiRoomChat.Registration exposing (..)

import Element as El exposing (Color, Device, Element)
import Json.Encode as JE exposing (Value)
import Phoenix exposing (ChannelResponse(..), PhoenixMsg(..), joinConfig)
import Types exposing (ErrorMessage(..), TwoTrack(..), User, bind, decodeUser, encodeColor)
import UI.FontColor exposing (error)
import Utils exposing (updatePhoenixWith)
import View.MultiRoomChat.Lobby.Registration as Registration exposing (backgroundColorError)



{- Model -}


type Model
    = Model
        { phoenix : Phoenix.Model
        , username : String
        , usernameError : Maybe ErrorMessage
        , backgroundColor : Maybe Color
        , backgroundColorError : Maybe ErrorMessage
        , foregroundColor : Maybe Color
        , foregroundColorError : Maybe ErrorMessage
        }


init : Phoenix.Model -> Model
init phoenix =
    Model
        { phoenix = phoenix
        , username = ""
        , usernameError = Nothing
        , backgroundColor = Nothing
        , backgroundColorError = Nothing
        , foregroundColor = Nothing
        , foregroundColorError = Nothing
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
            ( Model
                { model
                    | username = name
                    , usernameError =
                        if String.trim name == "" then
                            model.usernameError

                        else
                            Nothing
                }
            , Cmd.none
            , Empty
            )

        GotBackgroundColorSelection color ->
            ( Model
                { model
                    | backgroundColor = Just color
                    , backgroundColorError = Nothing
                }
            , Cmd.none
            , Empty
            )

        GotForegroundColorSelection color ->
            ( Model
                { model
                    | foregroundColor = Just color
                    , foregroundColorError = Nothing
                }
            , Cmd.none
            , Empty
            )

        GotJoinLobby ->
            case validateUserInput (Model model) of
                Success fields ->
                    updatePhoenixWith PhoenixMsg (Model model) <|
                        Phoenix.join "example:lobby" <|
                            Phoenix.setJoinConfig
                                { joinConfig
                                    | topic = "example:lobby"
                                    , events =
                                        [ "room_list"
                                        , "room_invite"
                                        , "invite_declined"
                                        ]
                                    , payload = encodeFields fields
                                }
                                model.phoenix

                Failure errors ->
                    ( handleErrors errors (Model model), Cmd.none, Empty )

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


handleErrors : List ErrorMessage -> Model -> Model
handleErrors errors (Model model) =
    List.foldl
        (\error (Model model_) ->
            case error of
                UsernameCannotBeBlank ->
                    Model { model_ | usernameError = Just UsernameCannotBeBlank }

                BackgroundColorNotSelected ->
                    Model { model_ | backgroundColorError = Just BackgroundColorNotSelected }

                ForegroundColorNotSelected ->
                    Model { model_ | foregroundColorError = Just ForegroundColorNotSelected }

                _ ->
                    Model model_
        )
        (Model
            { model
                | usernameError = Nothing
                , backgroundColorError = Nothing
                , foregroundColorError = Nothing
            }
        )
        errors



{- Validation -}


type Field
    = Username String
    | BackgroundColor Color
    | ForegroundColor Color


validateUserInput : Model -> TwoTrack Field
validateUserInput (Model { username, backgroundColor, foregroundColor }) =
    Success []
        |> bind validateUsername username
        |> bind validateBackgroundColor backgroundColor
        |> bind validateForegroundColor foregroundColor


validateUsername : String -> TwoTrack Field
validateUsername username =
    if String.trim username == "" then
        Failure [ UsernameCannotBeBlank ]

    else
        Success [ Username (String.trim username) ]


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
        |> Registration.usernameError model.usernameError
        |> Registration.backgroundColor model.backgroundColor
        |> Registration.backgroundColorError model.backgroundColorError
        |> Registration.foregroundColor model.foregroundColor
        |> Registration.foregroundColorError model.foregroundColorError
        |> Registration.onChange GotUsernameChange
        |> Registration.onBackgroundColorChange GotBackgroundColorSelection
        |> Registration.onForegroundColorChange GotForegroundColorSelection
        |> Registration.onSubmit GotJoinLobby
        |> Registration.view device
