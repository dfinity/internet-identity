import { showWarningIfNecessary } from "./banner";
import { displayError } from "./components/displayError";
import { showMessage } from "./components/message";
import { anyFeatures, features } from "./features";
import { aboutView } from "./flows/about";
import { registerTentativeDevice } from "./flows/addDevice/welcomeView/registerTentativeDevice";
import { authFlowAuthorize } from "./flows/authorize";
import { compatibilityNotice } from "./flows/compatibilityNotice";
import { authFlowManage } from "./flows/manage";
import { I18n } from "./i18n";
import "./styles/main.css";
import { getAddDeviceAnchor } from "./utils/addDeviceLink";
import { checkRequiredFeatures } from "./utils/featureDetection";
import { Connection } from "./utils/iiConnection";
import { version } from "./version";

import copyJson from "./index.json";

/** Reads the canister ID from the <script> tag.
 *
 * The canister injects the canister ID as a `data-canister-id` attribute on the script tag, which we then read to figure out where to make the IC calls.
 */
const readCanisterId = (): string => {
  // The backend uses a known element ID so that we can pick up the value from here
  const setupJs = document.querySelector("#setupJs") as HTMLElement | null;
  if (setupJs === null || setupJs.dataset.canisterId === undefined) {
    void displayError({
      title: "Canister ID not set",
      message:
        "There was a problem contacting the IC. The host serving this page did not give us a canister ID. Try reloading the page and contact support if the problem persists.",
      primaryButton: "Reload",
    }).then(() => {
      window.location.reload();
    });
    throw new Error("canisterId is undefined"); // abort further execution of this script
  }

  return setupJs.dataset.canisterId;
};

// Show version information for the curious programmer
const printDevMessage = () => {
  console.log("Welcome to Internet Identity!");
  console.log(
    "The code can be found here: https://github.com/dfinity/internet-identity"
  );
  console.log(
    `https://github.com/dfinity/internet-identity/commit/${version.commit}`
  );
  if (version.release !== undefined) {
    console.log(`This is version ${version.release}`);
  }
  if (version.dirty) {
    console.warn("This version is dirty");
  }

  if (anyFeatures()) {
    const message = `
Some features are enabled:
${Object.entries(features)
  .map(([k, v]) => ` - ${k}: ${v}`)
  .join("\n")}
see more at https://github.com/dfinity/internet-identity#features
      `;
    console.warn(message);
  }
};

const init = async () => {
  try {
    printDevMessage();
  } catch (e) {
    console.warn("Error when printing version information:", e);
  }

  const url = new URL(document.URL);

  // If the build is not "official", show a warning
  // https://github.com/dfinity/internet-identity#build-features
  showWarningIfNecessary();

  // Redirect to the FAQ
  // The canister should already be handling this with a 301 when serving "/faq", this is just a safety
  // measure.
  if (window.location.pathname === "/faq") {
    const faqUrl =
      "https://support.dfinity.org/hc/en-us/sections/8730568843412-Internet-Identity";
    window.location.replace(faqUrl);
  }

  if (window.location.pathname === "/about") {
    return aboutView();
  }

  const okOrReason = await checkRequiredFeatures(url);
  if (okOrReason !== true) {
    return compatibilityNotice(okOrReason);
  }

  // Prepare the actor/connection to talk to the canister
  const connection = new Connection(readCanisterId());

  // Figure out if user is trying to add a device. If so, use the anchor from the URL.
  const addDeviceAnchor = getAddDeviceAnchor();
  if (addDeviceAnchor !== undefined) {
    // Register this device (tentatively)
    await registerTentativeDevice(addDeviceAnchor, connection);

    const i18n = new I18n();
    const staticCopy = i18n.staticLang(copyJson);

    // Show a good bye message
    showMessage({
      message: staticCopy.close_page_device_added,
      role: "notify-device-added",
    });
    return;
  }

  // Simple, #-based routing
  if (url.hash === "#authorize") {
    // User was brought here by a dapp for authorization
    void authFlowAuthorize(connection);
  } else {
    // The default flow
    void authFlowManage(connection);
  }
};

void init();
