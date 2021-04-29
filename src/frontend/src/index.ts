import "web-dialog";
import "./styles/main.css";
import { login } from "./flows/login";
import oauth from "./utils/oauth";
import auth from "./auth";
import { addDevice } from "./flows/addDevice";
import { renderManage } from "./flows/manage";

const init = async () => {
  const { userNumber, connection } = await login();
  const url = new URL(document.URL);
  if (url.hash == "#authorize") {
    auth(userNumber, connection);
  } else if (window.location.href.match(/authorize/)) {
    console.log("oauth");
    oauth(userNumber, connection);
  } else if (!!url.hash?.split("device=")[1]) {
    addDevice(userNumber, connection);
  } else {
    renderManage(userNumber, connection);
  }
};

init();
