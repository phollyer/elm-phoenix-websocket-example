
import { Socket, Presence } from "phoenix"
import { Elm } from "../elm/src/Main.elm";
import ElmPhoenixWebSocket from "./elmPhoenixWebSocket";

let flags =
{
    height: window.innerHeight,
    width: window.innerWidth,
    vsn: document.querySelector("#body").dataset.vsn
}

const app = Elm.Main.init({ flags: flags });

ElmPhoenixWebSocket.init(app.ports, Socket, Presence);

