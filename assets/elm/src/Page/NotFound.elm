module Page.NotFound exposing (view)

import Element as El exposing (Device, Element)
import UI.FontSize as FontSize


view : Device -> { title : String, content : Element msg }
view device =
    { title = "Not Found"
    , content =
        El.el
            [ El.centerX
            , El.centerY
            , FontSize.pageNotFound device
            ]
            (El.text " Page Not Found")
    }
