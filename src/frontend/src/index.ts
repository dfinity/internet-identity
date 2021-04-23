import "web-dialog";
import "./styles/main.css";
import { login } from "./flows/login";
import oauth from "./utils/oauth";
import { addDevice } from "./flows/addDevice";
import { renderManage } from "./flows/manage";
import "clipboard";

const init = async () => {
  // initLogout();

  const { userId, connection } = await login();
  const url = new URL(document.URL);
  if (window.location.href.match(/authorize/)) {
    oauth(userId, connection);
  } else if (!!url.hash?.split("device=")[1]) {
    addDevice(userId, connection);
  } else {
    renderManage(userId, connection);
  }
};

init();
