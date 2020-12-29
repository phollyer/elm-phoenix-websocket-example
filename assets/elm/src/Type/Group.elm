module Type.Group exposing
    ( Group
    , init
    , layoutForDevice
    , layouts
    , order
    , orderForDevice
    )

import Element exposing (Device, DeviceClass, Orientation)
import List.Extra as List



{- Model -}


type Group
    = Group
        { layouts : List ( DeviceClass, Orientation, List Int )
        , order : List ( DeviceClass, Orientation, List Int )
        }


init : Group
init =
    Group
        { layouts = []
        , order = []
        }


layouts : List ( DeviceClass, Orientation, List Int ) -> Group -> Group
layouts list (Group config) =
    Group { config | layouts = list }


order : List ( DeviceClass, Orientation, List Int ) -> Group -> Group
order list (Group config) =
    Group { config | order = list }



{- Helpers -}


layoutForDevice : Device -> Group -> Maybe (List Int)
layoutForDevice device (Group config) =
    findForDevice device config.layouts


orderForDevice : Device -> List item -> Group -> List item
orderForDevice device items (Group config) =
    case findForDevice device config.order of
        Nothing ->
            items

        Just sortOrder ->
            List.indexedMap Tuple.pair items
                |> List.map2 (\newIndex ( _, item ) -> ( newIndex, item )) sortOrder
                |> List.sortBy Tuple.first
                |> List.unzip
                |> Tuple.second



{- Private -}


findForDevice : Device -> List ( DeviceClass, Orientation, a ) -> Maybe a
findForDevice { class, orientation } list =
    List.find (\( class_, orientation_, _ ) -> class_ == class && orientation_ == orientation) list
        |> Maybe.map (\( _, _, a ) -> a)
