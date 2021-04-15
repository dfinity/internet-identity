import { initExistingUser } from "./flows/existingUser";
import { initNewUser } from "./flows/newUser";

import "./styles/main.css";

const init = () => {
  // Initialize our flows
  initExistingUser();
  initNewUser();
};

init();
