if (process.env.NODE_ENV === "production") {
  require("./rollbar");
}

import "./main.css";
import * as Elm from "./Main.elm";

const root = document.getElementById("root");
Elm.Main.embed(root, { text: getParameterByName("t") || "" });

function getParameterByName(name) {
  var match = RegExp("[?&]" + name + "=([^&]*)").exec(window.location.search);
  return match && decodeURIComponent(match[1].replace(/\+/g, " "));
}
