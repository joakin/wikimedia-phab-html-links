if (process.env.NODE_ENV === "production") {
  require("./rollbar");
}

import "./main.css";
import * as Elm from "./Main.elm";

const root = document.getElementById("root");
const app = Elm.Main.embed(root, { text: getTextParam(window.location.hash) });

openLinksInNewTab(root)

app.ports.replaceURL.subscribe(url =>
  window.history.replaceState(null, null, url)
);

function getTextParam(hash) {
  if (!hash) return "";

  return decodeURIComponent(hash.slice(1));
}

function openLinksInNewTab(el) {
  document.body.addEventListener('click', (event) => {
    if (event.target.nodeName === 'A') {
      event.target.setAttribute('target', '_blank')
    }
  })
}