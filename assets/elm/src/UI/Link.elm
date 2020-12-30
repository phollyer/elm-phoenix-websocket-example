module UI.Link exposing
    ( function
    , srcLink
    )

import Element as El exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Type.Example as Example exposing (Example)
import UI.BorderColor as BorderColor
import UI.BorderWidth as BorderWidth
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily


{-| A fomratted link to a functions docs.
-}
function : String -> Element msg
function func =
    El.newTabLink
        [ FontFamily.code ]
        { url = toPackageUrl func
        , label =
            El.paragraph
                []
                (format func)
        }


srcLink : Example -> Element msg
srcLink example =
    El.newTabLink
        []
        { url = Example.toSrc example
        , label =
            El.el
                [ Border.dashed
                , BorderColor.link
                , BorderWidth.bottom 1
                , FontColor.link
                ]
                (El.text "here")
        }


toPackageUrl : String -> String
toPackageUrl func =
    let
        base =
            "https://package.elm-lang.org/packages/phollyer/elm-phoenix-websocket/latest/Phoenix"
    in
    case String.split "." func of
        _ :: f :: [] ->
            base ++ "#" ++ f

        f :: [] ->
            base ++ "#" ++ f

        _ ->
            base


format : String -> List (Element msg)
format func =
    case String.split "." func of
        phoenix :: f :: [] ->
            [ El.el [ FontColor.moduleName ] (El.text phoenix)
            , El.el [ FontColor.function ] (El.text ("." ++ f))
            ]

        f :: [] ->
            [ El.el [ FontColor.function ] (El.text ("." ++ f))
            ]

        _ ->
            []
