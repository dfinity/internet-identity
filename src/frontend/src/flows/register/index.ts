import {
  apiResultToLoginFlowResult,
  cancel,
  LoginFlowResult,
} from "$src/utils/flowResult";
import { Connection, IIWebAuthnIdentity } from "$src/utils/iiConnection";
import { setAnchorUsed } from "$src/utils/userNumber";
import { unknownToString } from "$src/utils/utils";
import { nonNullish } from "@dfinity/utils";
import type { UAParser } from "ua-parser-js";
import { promptCaptcha } from "./captcha";
import { displayUserNumber } from "./finish";
import { savePasskey } from "./passkey";

/** Registration (anchor creation) flow for new users */
export const register = async ({
  connection,
}: {
  connection: Connection;
}): Promise<LoginFlowResult> => {
  try {
    // Kick-off fetching "ua-parser-js";
    const uaParser = loadUAParser();

    // Kick-off the challenge request early, so that we might already
    // have a captcha to show once we get to the CAPTCHA screen
    const preloadedChallenge = connection.createChallenge();
    const identity = await savePasskey();
    if (identity === "canceled") {
      return cancel;
    }

    const alias = await inferAlias({
      authenticatorType: identity.getAuthenticatorAttachment(),
      userAgent: navigator.userAgent,
      uaParser,
    });

    const captchaResult = await promptCaptcha({
      connection,
      challenge: preloadedChallenge,
      identity,
      alias,
    });

    if ("tag" in captchaResult) {
      return captchaResult;
    } else {
      const result = apiResultToLoginFlowResult(captchaResult);
      if (result.tag === "ok") {
        setAnchorUsed(result.userNumber);
        await displayUserNumber(result.userNumber);
      }
      return result;
    }
  } catch (e) {
    return {
      tag: "err",
      title: "Failed to create Internet Identity",
      message: "An error occurred during Internet Identity creation.",
      detail: unknownToString(e, "unknown error"),
    };
  }
};

type AuthenticatorType = ReturnType<
  IIWebAuthnIdentity["getAuthenticatorAttachment"]
>;
type PreloadedUAParser = ReturnType<typeof loadUAParser>;

// Logic for inferring a passkey alias based on the authenticator type & user agent
export const inferAlias = async ({
  authenticatorType,
  userAgent,
  uaParser: uaParser_,
}: {
  authenticatorType: AuthenticatorType;
  userAgent: typeof navigator.userAgent;
  uaParser: PreloadedUAParser;
}): Promise<string> => {
  const UNNAMED = "Unnamed Passkey";
  const FIDO = "FIDO Passkey";
  const ICLOUD = "iCloud Passkey";

  // If the authenticator is cross platform, then it's FIDO
  if (authenticatorType === "cross-platform") {
    return FIDO;
  }

  // Otherwise, make sure the UA parser module is loaded, because
  // everything from here will use UA heuristics
  const UAParser = await uaParser_;
  if (UAParser === undefined) {
    return UNNAMED;
  }
  const uaParser = new UAParser(userAgent);

  if (
    authenticatorType === "platform" &&
    uaParser.getEngine().name === "WebKit"
  ) {
    // Safari, including Chrome, FireFox etc on iOS/iPadOs
    const version = uaParser.getBrowser().version;

    if (nonNullish(version) && Number(version) >= 16.2) {
      // Safari 16.2 enforce usage of iCloud passkeys
      return ICLOUD;
    } else {
      // If the Safari version is older, then we just give the device (since
      // each apple device like iPhone, iPad, etc has its own OS, there is no
      // need to duplicate the info with the OS)
      const device = uaParser.getDevice();
      if (nonNullish(device) && nonNullish(device.model)) {
        return device.model;
      }
    }
  }

  if (
    authenticatorType !== "platform" &&
    uaParser.getEngine().name === "Gecko" &&
    uaParser.getOS().name === "Mac OS"
  ) {
    // FireFox on Mac OS does not support TouchID, so if it's not a "platform" authenticator it's some sort
    // of FIDO device, even if no authenticator type was provided
    return FIDO;
  }

  const os = uaParser.getOS().name;

  // As a last resort, we try to show something like "Chrome on Linux" or just "Chrome" or just "Linux"
  const browser = uaParser.getBrowser().name;
  const browserOn = [
    ...(nonNullish(browser) ? [browser] : []),
    ...(nonNullish(os) ? [os] : []),
  ];
  authenticatorType satisfies undefined | "platform";
  if (browserOn.length !== 0) {
    return browserOn.join(" on ");
  }

  // If all else fails, the device is unnamed
  return UNNAMED;
};

// Dynamically load the user agent parser module
export const loadUAParser = async (): Promise<typeof UAParser | undefined> => {
  try {
    return (await import("ua-parser-js")).default;
  } catch (e) {
    console.error(e);
    return undefined;
  }
};
