import 'elm-pep';
import { Elm } from "./Main.elm";

function getRandomInt() {
  return Math.floor(Math.random() * 999999);
}

const saveGameKey = 'drg-mc:saveGame'
const saveGame = localStorage.getItem(saveGameKey);
const initialGame = saveGame ? JSON.parse(saveGame) : {};
const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: {
    initialSeed: getRandomInt(),
    initialGame,
    now: Date.now(),
  }
});

app.ports.saveGame.subscribe(function(game) {
  localStorage.setItem(saveGameKey, JSON.stringify(game));
});

app.ports.closePopover.subscribe(function(popoverId) {
  document.getElementById(popoverId).hidePopover();
});

app.ports.openModal.subscribe(function(modalId) {
  const modal = document.getElementById(modalId);
  if (modal && typeof modal.showModal === 'function') {
    modal.showModal();
  }
});