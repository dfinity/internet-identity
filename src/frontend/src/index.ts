import { initExistingUser } from "./flows/existingUser";
import { initNewUser } from "./flows/newUser";
import oauth from "./utils/oath";
import "./styles/main.css";

const init = () => {
  if (window.location.href.match(/authorize/)) {
    oauth();
  }
  // Initialize our flows
  initExistingUser();
  initNewUser();
};

init();
