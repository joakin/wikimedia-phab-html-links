if (process.env.NODE_ENV === "production") {
  require("./rollbar");
}

import "./main.css";
import * as Elm from "./Main.elm";

const root = document.getElementById("root");
const app = Elm.Main.embed(root, { text: getParameterByName("t") || "" });

app.ports.replaceURL.subscribe(url =>
  window.history.replaceState(null, null, url)
);

function getParameterByName(name) {
  var match = RegExp("[?&]" + name + "=([^&]*)").exec(window.location.search);
  return match && decodeURIComponent(match[1].replace(/\+/g, " "));
}
