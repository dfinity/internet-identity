import { isNullish, nonNullish } from "@dfinity/utils";
import { displayError } from "$lib/templates/displayError";
import type { InternetIdentityInit } from "$lib/generated/internet_identity_types";
import { IDL } from "@dfinity/candid";
import { init } from "$lib/generated/internet_identity_idl";
import { version } from "$lib/legacy/version";
import { anyFeatures, features } from "$lib/legacy/features";
import { fromBase64 } from "$lib/utils/utils";

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
