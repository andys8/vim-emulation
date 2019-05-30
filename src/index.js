import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const app = Elm.Main.init({
  node: document.getElementById("root")
});

app.ports.quitVim.subscribe(function() {
  const el = document.getElementById("outermost");
  if (el) {
    el.remove();
  }
});

registerServiceWorker();
