import "./styles/main.css";
import { login } from "./flows/login";
import { renderManage } from "./flows/manage";
import { compatibilityNotice } from "./flows/compatibilityNotice";
import { aboutView } from "./flows/about";
import { faqView } from "./flows/faq";
import { intentFromUrl } from "./utils/userIntent";
import { checkRequiredFeatures } from "./utils/featureDetection";
import { recoveryWizard } from "./flows/recovery/recoveryWizard";
import { showWarningIfNecessary } from "./banner";
import authorizeAuthentication from "./flows/authenticate";
import { displayError } from "./components/displayError";

const readCanisterId = (): string => {

    const setupJs = document.querySelector("#setupJs") as HTMLElement;
    if(setupJs === null) {

        displayError({
            title: "Canister ID not set",
            message:
                "There was a problem contacting the IC. The host serving this page did not give us a canister ID. Try reloading the page and contact support if the problem persists.",
            primaryButton: "Reload",
        }).then(() => {
            window.location.reload();
        });
        throw new Error("canisterId is undefined"); // abort further execution of this script
    }

    const canisterId = setupJs.dataset.canisterId;

    if(canisterId === undefined) {
        displayError({
            title: "Canister ID not set",
            message:
                "There was a problem contacting the IC. The host serving this page did not give us a canister ID. Try reloading the page and contact support if the problem persists.",
            primaryButton: "Reload",
        }).then(() => {
            window.location.reload();
        });
        throw new Error("canisterId is undefined"); // abort further execution of this script
    }

    return canisterId;
}

const init = async () => {
  const url = new URL(document.URL);

  // If the build is not "official", show a warning
  // https://github.com/dfinity/internet-identity#build-features
  showWarningIfNecessary();

  // Custom routing to the FAQ page
  if (window.location.pathname === "/faq") {
    return faqView();
  }

  if (window.location.pathname === "/about" || url.hash === "#about") {
    return aboutView();
  }

  const okOrReason = await checkRequiredFeatures(url);
  if (okOrReason !== true) {
    return compatibilityNotice(okOrReason);
  }

  const userIntent = intentFromUrl(url);

  switch (userIntent.kind) {
    // Authenticate to a third party service
    case "auth": {
      // show the application 'authorize authentication' screen. The user can authenticate, create a new anchor or jump to other pages to recover and manage.
      const authSuccess = await authorizeAuthentication();
      // show the recovery wizard before sending the window post message, otherwise the II window will be closed
      await recoveryWizard(authSuccess.userNumber, authSuccess.connection);
      // send the delegation back to the dapp window (which will then close the II window)
      authSuccess.sendDelegationMessage();
      return;
    }
    // Open the management page
    case "manage": {
      // Go through the login flow, potentially creating an anchor.
      const { userNumber, connection } = await login();
      // Here, if the user doesn't have any recovery device, we prompt them to add
      // one. The exact flow depends on the device they use.
      await recoveryWizard(userNumber, connection);
      // From here on, the user is authenticated to II.
      return renderManage(userNumber, connection);
    }
  }
};

init();
