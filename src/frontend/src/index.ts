import "./styles/main.css";
import { login } from "./flows/login";
import auth from "./auth";
import { addDevice } from "./flows/addDevice";
import { renderManage } from "./flows/manage";
import { compatibilityNotice } from "./flows/compatibilityNotice";
import { aboutView } from "./flows/about";
import { intentFromUrl } from "./utils/userIntent";
import { hasRequiredFeatures } from "./utils/featureDetection";

const init = async () => {
  const url = new URL(document.URL);
  if (url.hash === "#about") {
    return aboutView();
  }

  if (!(await hasRequiredFeatures(url))) {return compatibilityNotice();}

  const userIntent = intentFromUrl(url);
  const { userNumber, connection } = await login(userIntent);

  switch (userIntent.kind) {
    case "auth": {
      return auth(userNumber, connection);
    }
    case "addDevice": {
      return addDevice(userNumber, connection);
    }
    case "manage": {
      return renderManage(userNumber, connection);
    }
  }
};

init();
