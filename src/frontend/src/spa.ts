import loaderUrl from "$src/components/loader.png";
import { showWarningIfNecessary } from "./banner";
import { displayError } from "./components/displayError";
import { anyFeatures, features } from "./features";
import { compatibilityNotice } from "./flows/compatibilityNotice";
import "./styles/main.css";
import { supportsWebAuthn } from "./utils/featureDetection";
import { Connection } from "./utils/iiConnection";
import { version } from "./version";

import { isNullish, nonNullish } from "@dfinity/utils";

// Polyfill Buffer globally for the browser
import { Buffer } from "buffer";
globalThis.Buffer = Buffer;

/** Reads the canister ID from the <script> tag.
 *
 * The canister injects the canister ID as a `data-canister-id` attribute on the script tag, which we then read to figure out where to make the IC calls.
 */
const readCanisterId = (): string => {
  // The backend uses a known element ID so that we can pick up the value from here
  const setupJs = document.querySelector(
    "[data-canister-id]"
  ) as HTMLElement | null;
  if (isNullish(setupJs) || isNullish(setupJs.dataset.canisterId)) {
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
  if (nonNullish(version.release)) {
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

// Ensure the loader/spinner assets are ready when called
// https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/rel/preload
const preloadLoaderImage = () => {
  const link = document.createElement("link");
  link.setAttribute("rel", "preload");
  link.setAttribute("href", loaderUrl);
  link.setAttribute("as", "image");
  document.head.append(link);
};

// Create a single page app with canister connection
export const createSpa = (app: (connection: Connection) => Promise<never>) => {
  try {
    printDevMessage();
  } catch (e) {
    console.warn("Error when printing version information:", e);
  }

  // Preload the loader
  preloadLoaderImage();

  // If the build is not "official", show a warning
  // https://github.com/dfinity/internet-identity#build-features
  showWarningIfNecessary();

  if (!supportsWebAuthn()) {
    return compatibilityNotice();
  }

  // Prepare the actor/connection to talk to the canister
  const connection = new Connection(readCanisterId());

  return app(connection);
};
