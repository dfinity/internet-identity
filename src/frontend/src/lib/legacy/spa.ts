import loaderUrl from "$lib/templates/loader.png?url";
import { showWarningIfNecessary } from "./banner";
import { displayError } from "$lib/templates/displayError";
import { anyFeatures, features } from "./features";
import { compatibilityNotice } from "$lib/legacy/flows/compatibilityNotice";
import "./styles/main.css";
import { supportsWebAuthn } from "$lib/utils/featureDetection";
import { Connection } from "$lib/utils/iiConnection";
import { version } from "./version";

import { init } from "$lib/generated/internet_identity_idl";
import type { InternetIdentityInit } from "$lib/generated/internet_identity_types";
import { fromBase64 } from "$lib/utils/utils";
import { IDL } from "@dfinity/candid";
import { isNullish, nonNullish } from "@dfinity/utils";

// Polyfill Buffer globally for the browser
import { Buffer } from "buffer";

globalThis.Buffer = Buffer;

/** Reads the canister ID from the <script> tag.
 *
 * The canister injects the canister ID as a `data-canister-id` attribute on the script tag, which we then read to figure out where to make the IC calls.
 */
export const readCanisterId = (): string => {
  // The backend uses a known element ID so that we can pick up the value from here
  const setupJs = document.querySelector(
    "[data-canister-id]",
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

export const readCanisterConfig = (): InternetIdentityInit => {
  // The backend uses a known element ID so that we can pick up the value from here
  const setupJs = document.querySelector(
    "[data-canister-config]",
  ) as HTMLElement | null;
  if (isNullish(setupJs) || isNullish(setupJs.dataset.canisterConfig)) {
    void displayError({
      title: "Canister config not set",
      message:
        "There was a problem contacting the IC. The host serving this page did not give us the canister config. Try reloading the page and contact support if the problem persists.",
      primaryButton: "Reload",
    }).then(() => {
      window.location.reload();
    });
    throw new Error("canister config is undefined"); // abort further execution of this script
  }

  try {
    const [jsonValue] = IDL.decode(
      [init({ IDL })[0]._type],
      fromBase64(setupJs.dataset.canisterConfig),
    );
    return jsonValue as unknown as InternetIdentityInit;
  } catch {
    void displayError({
      title: "Canister config not valid",
      message:
        "There was a problem contacting the IC. The host serving this page did not give us a valid canister config. Try reloading the page and contact support if the problem persists.",
      primaryButton: "Reload",
    }).then(() => {
      window.location.reload();
    });
    throw new Error("canister config is invalid"); // abort further execution of this script
  }
};

// Show version information for the curious programmer
export const printDevMessage = () => {
  console.log("Welcome to Internet Identity!");
  console.log(
    "The code can be found here: https://github.com/dfinity/internet-identity",
  );
  console.log(
    `https://github.com/dfinity/internet-identity/commit/${version.commit}`,
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

  // Prepare the actor/connection to talk to the canister
  const connection = new Connection(readCanisterId(), readCanisterConfig());

  // If the build is not "official", show a warning
  // https://github.com/dfinity/internet-identity#build-features
  showWarningIfNecessary(connection.canisterConfig);

  if (!supportsWebAuthn()) {
    return compatibilityNotice();
  }

  return app(connection);
};
