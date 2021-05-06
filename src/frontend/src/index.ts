import "./styles/main.css";
import { login } from "./flows/login";
import auth from "./auth";
import { addDevice } from "./flows/addDevice";
import { renderManage } from "./flows/manage";
import { compatibilityNotice } from "./flows/compatibilityNotice";
import { aboutView } from "./flows/about";
import { intentFromUrl } from "./utils/userIntent";

const init = async () => {
  const url = new URL(document.URL);
  if (url.hash == "#about") {
    return aboutView()
  }

  if (window.PublicKeyCredential) {
    const isIos = navigator.userAgent.match(/(iPhone|iPod|iPad)/);
    const isAndroid = navigator.userAgent.match(/Android/);
    if(isIos || isAndroid){ 
      try {
        const available = PublicKeyCredential.isUserVerifyingPlatformAuthenticatorAvailable()
        if (available) {
          // Proceed
        } else {
          return compatibilityNotice();
        }
      } catch (error) {
        return compatibilityNotice(); 
      }
    } else {
      // proceed
    }
  } else {
    return compatibilityNotice();
  }
    
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
