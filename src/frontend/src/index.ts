import "./styles/main.css";
import { login } from "./flows/login";
import auth from "./auth";
import { addDevice } from "./flows/addDevice";
import { renderManage } from "./flows/manage";
import { compatibilityNotice } from "./flows/compatibilityNotice";
import { aboutView } from "./flows/about";
import { faqView } from "./flows/faq";
import { intentFromUrl } from "./utils/userIntent";
import { hasRequiredFeatures } from "./utils/featureDetection";
import { displaySingleDeviceWarning } from "./flows/displaySingleDeviceWarning";
import { setupRecovery } from "./flows/recovery/setupRecovery";
import { IIConnection } from "./utils/iiConnection";

const init = async () => {
  const url = new URL(document.URL);

  // Custom routing to the FAQ page
  if (window.location.pathname === "/faq") {
    return faqView();
  }

  if (url.hash === "#about") {
    return aboutView();
  }

  if (!(await hasRequiredFeatures(url))) {
    return compatibilityNotice();
  }

  const userIntent = intentFromUrl(url);
  const { userNumber, connection } = await login(userIntent);

  if ((await IIConnection.lookupRecovery(userNumber)).length === 0) {
    await displaySingleDeviceWarning();
    await setupRecovery(userNumber, connection);
  }

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
