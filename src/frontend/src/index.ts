import "web-dialog";
import "./styles/main.css";
import { initLogout } from "./flows/logout";
import { routerInit } from "./utils/router";
import { loginKnown } from "./pages/loginKnown";

const init = () => {
  initLogout();
  // routerInit();
  loginKnown();
};

init();
