import "web-dialog";
import "./styles/main.css";
import { initLogout } from "./flows/logout";
import { login } from "./flows/login";
import oauth from "./utils/oauth";
import { addDevice } from "./flows/addDevice";
import { renderManage } from "./flows/manage";
import "clipboard";
import { IDPActor } from "./utils/idp_actor";

const init = async () => {
  // initLogout();
  addDevice(BigInt(9999), undefined as unknown as IDPActor)

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
