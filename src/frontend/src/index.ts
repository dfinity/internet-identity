import "web-dialog";
import "./styles/main.css";
import { initLogout } from "./flows/logout";
import { routerInit } from "./utils/router";

const init = () => {
  initLogout();
  routerInit();
};

init();
