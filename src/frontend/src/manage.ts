import { initManageIdentities } from "./flows/manageIdentities";
import "web-dialog";
import "./styles/main.css";

const init = () => {
  // Initialize our flows
  initManageIdentities();
};

init();
