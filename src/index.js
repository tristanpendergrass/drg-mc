import { Elm } from "./Main.elm";

const saveGameKey = 'drg-mc:saveGame'
const saveGame = localStorage.getItem(saveGameKey);
const initialGame = saveGame ? JSON.parse(saveGame) : {};
const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: {
    initialGame,
    now: Date.now(),
  }
});

app.ports.saveGame.subscribe(function(game) {
  localStorage.setItem(saveGameKey, JSON.stringify(game));
});
