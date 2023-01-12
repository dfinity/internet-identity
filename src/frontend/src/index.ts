import "./styles/main.css";
import { authFlowManage } from "./flows/manage";
import { authFlowAuthorize } from "./flows/authorize";
import { compatibilityNotice } from "./flows/compatibilityNotice";
import { aboutView } from "./flows/about";
import { faqView } from "./flows/faq";
import { intentFromUrl } from "./utils/userIntent";
import { version } from "./version";
import { checkRequiredFeatures } from "./utils/featureDetection";
import { showWarningIfNecessary } from "./banner";
import { displayError } from "./components/displayError";
import { Connection } from "./utils/iiConnection";
import { anyFeatures, features } from "./features";

/** Reads the canister ID from the <script> tag.
 *
 * The canister injects the canister ID as a `data-canister-id` attribute on the script tag, which we then read to figure out where to make the IC calls.
 */
const readCanisterId = (): string => {
  // The backend uses a known element ID so that we can pick up the value from here
  const setupJs = document.querySelector("#setupJs") as HTMLElement | null;
  if (setupJs === null || setupJs.dataset.canisterId === undefined) {
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

  // Custom routing to the FAQ page
  if (window.location.pathname === "/faq") {
    return faqView();
  }

  if (window.location.pathname === "/about") {
    return aboutView();
  }

  const okOrReason = await checkRequiredFeatures(url);
  if (okOrReason !== true) {
    return compatibilityNotice(okOrReason);
  }

  const userIntent = intentFromUrl(url);

  // Prepare the actor/connection to talk to the canister
  const connection = new Connection(readCanisterId());

  switch (userIntent.kind) {
    // Authenticate to a third party service
    case "auth": {
      await authFlowAuthorize(connection);
      break;
    }
    // Open the management page
    case "manage": {
      await authFlowManage(connection);
      break;
    }
  }
};

init();
