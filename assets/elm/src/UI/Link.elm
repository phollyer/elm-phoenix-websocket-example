module UI.Link exposing
    ( function
    , srcLink
    , type_
    )

import Element as El exposing (Element)
import Element.Border as Border
import Type.Example as Example exposing (Example)
import UI.BorderColor as BorderColor
import UI.BorderWidth as BorderWidth
import UI.FontColor as FontColor
import UI.FontFamily as FontFamily


base : String
base =
    "https://package.elm-lang.org/packages/phollyer/elm-phoenix-websocket/latest/Phoenix#"



{- Function -}


function : String -> Element msg
function func =
    El.newTabLink
        [ FontFamily.code ]
        { url =
            case String.split "." func of
                _ :: f :: [] ->
                    base ++ f

                f :: [] ->
                    base ++ f

                _ ->
                    base
        , label =
            El.paragraph [] (format func)
        }



{- Type -}


type_ : ( String, String ) -> Element msg
type_ ( actual, text ) =
    El.newTabLink
        [ FontFamily.code ]
        { url = base ++ actual
        , label =
            El.paragraph [] (format text)
        }



{- Source Code -}


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



{- Transform -}


format : String -> List (Element msg)
format func =
    case String.split "." func of
        phoenix :: f :: [] ->
            [ El.el [ FontColor.moduleName ] (El.text phoenix)
            , El.el [ FontColor.function ] (El.text ("." ++ f))
            ]

        f :: [] ->
            [ El.el [ FontColor.function ] (El.text f) ]

        _ ->
            []
