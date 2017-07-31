if (process.env.NODE_ENV === "production") {
  require("./rollbar");
}

import "./main.css";
import * as Elm from "./Main.elm";

const root = document.getElementById("root");
const app = Elm.Main.embed(root, { text: getTextParam(window.location.hash) });

app.ports.replaceURL.subscribe(url =>
  window.history.replaceState(null, null, url)
);

function getTextParam(hash) {
  if (!hash) return "";

  return decodeURIComponent(hash.slice(1));
}
