import { initExistingUser } from "./flows/existingUser";
import { initNewUser } from "./flows/newUser";
import oauth from "./utils/oath";
import "web-dialog";
import "./styles/main.css";
import { initLogout } from "./flows/logout";
import { renderIndex } from "./pages";
import { renderManage } from "./pages/manage";
import { routerInit } from "./utils/router";

const init = () => {
  initLogout();
  routerInit();
};

init();
