module UI exposing
    ( code
    , functionLink
    )

import Colors.Opaque as Color
import Element as El exposing (Element)
import Element.Background as Background
import Element.Font as Font


{-| Formatted [code] snippet.
-}
code : String -> Element msg
code text =
    El.el
        [ Font.family [ Font.typeface "Roboto Mono" ]
        , Background.color Color.gainsboro
        , El.padding 1
        , Font.color Color.black
        ]
        (El.text text)


{-| A fomratted link to a functions docs.
-}
functionLink : String -> Element msg
functionLink function =
    El.newTabLink
        [ Font.family
            [ Font.typeface "Roboto Mono" ]
        ]
        { url = toPackageUrl function
        , label =
            El.paragraph
                []
                (format function)
        }


toPackageUrl : String -> String
toPackageUrl function =
    let
        base =
            "https://package.elm-lang.org/packages/phollyer/elm-phoenix-websocket/latest/Phoenix"
    in
    case String.split "." function of
        _ :: func :: [] ->
            base ++ "#" ++ func

        func :: [] ->
            base ++ "#" ++ func

        _ ->
            base


format : String -> List (Element msg)
format function =
    case String.split "." function of
        phoenix :: func :: [] ->
            [ El.el [ Font.color Color.orange ] (El.text phoenix)
            , El.el [ Font.color Color.darkgrey ] (El.text ("." ++ func))
            ]

        func :: [] ->
            [ El.el [ Font.color Color.darkgrey ] (El.text ("." ++ func))
            ]

        _ ->
            []
