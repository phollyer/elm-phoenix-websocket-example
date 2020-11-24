module Session exposing
    ( Session
    , device
    , init
    , navKey
    , phoenix
    , updateDevice
    , updatePhoenix
    , vsn
    )

import Browser.Navigation as Nav
import Element exposing (Device)
import Phoenix
import Ports


init : String -> Nav.Key -> Device -> Session
init vsn_ key device_ =
    Session
        { key = key
        , device = device_
        , phoenix = Phoenix.init Ports.config
        , vsn = vsn_
        }


type Session
    = Session
        { key : Nav.Key
        , device : Device
        , phoenix : Phoenix.Model
        , vsn : String
        }


navKey : Session -> Nav.Key
navKey (Session session) =
    session.key


phoenix : Session -> Phoenix.Model
phoenix (Session session) =
    session.phoenix


device : Session -> Device
device (Session session) =
    session.device


vsn : Session -> String
vsn (Session session) =
    session.vsn


updateDevice : Device -> Session -> Session
updateDevice device_ (Session session) =
    Session { session | device = device_ }


updatePhoenix : Phoenix.Model -> Session -> Session
updatePhoenix phx (Session session) =
    Session { session | phoenix = phx }
