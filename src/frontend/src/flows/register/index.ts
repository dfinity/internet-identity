import { Challenge } from "$generated/internet_identity_types";
import {
  LoginFlowCanceled,
  LoginFlowResult,
  apiResultToLoginFlowResult,
  cancel,
} from "$src/utils/flowResult";
import {
  AuthenticatedConnection,
  Connection,
  IIWebAuthnIdentity,
  RegisterResult,
} from "$src/utils/iiConnection";
import { setAnchorUsed } from "$src/utils/userNumber";
import { unknownToString } from "$src/utils/utils";
import { ECDSAKeyIdentity } from "@dfinity/identity";
import { nonNullish } from "@dfinity/utils";
import type { UAParser } from "ua-parser-js";
import { badChallenge, precomputeFirst, promptCaptcha } from "./captcha";
import { displayUserNumberWarmup } from "./finish";
import { savePasskey } from "./passkey";

/** Registration (anchor creation) flow for new users */
export const registerFlow = async <T>({
  createChallenge: createChallenge_,
  register,
}: {
  createChallenge: () => Promise<Challenge>;
  register: (opts: {
    alias: string;
    identity: IIWebAuthnIdentity;
    challengeResult: { chars: string; challenge: Challenge };
  }) => Promise<RegisterResult<T>>;
}): Promise<RegisterResult<T> | LoginFlowCanceled> => {
  // Kick-off fetching "ua-parser-js";
  const uaParser = loadUAParser();

  // Kick-off the challenge request early, so that we might already
  // have a captcha to show once we get to the CAPTCHA screen
  const createChallenge = precomputeFirst(() => createChallenge_());

  const identity = await savePasskey();
  if (identity === "canceled") {
    return cancel;
  }

  const alias = await inferAlias({
    authenticatorType: identity.getAuthenticatorAttachment(),
    userAgent: navigator.userAgent,
    uaParser,
  });

  const displayUserNumber = displayUserNumberWarmup();

  const result = await promptCaptcha({
    createChallenge,
    register: async ({ chars, challenge }) => {
      const result = await register({
        identity,
        alias,
        challengeResult: { chars, challenge },
      });

      if (result.kind === "badChallenge") {
        return badChallenge;
      }

      return result;
    },
  });

  if ("tag" in result) {
    result.tag satisfies "canceled";
    return result;
  }

  if (result.kind === "loginSuccess") {
    const userNumber = result.userNumber;
    setAnchorUsed(userNumber);
    await displayUserNumber({ userNumber });
  }
  return result;
};

/** Concrete implementation of the registration flow */
export const register = async ({
  connection,
}: {
  connection: Connection;
}): Promise<LoginFlowResult> => {
  try {
    const result = await registerFlow<AuthenticatedConnection>({
      createChallenge: () => connection.createChallenge(),
      register: async ({
        identity,
        alias,
        challengeResult: {
          chars,
          challenge: { challenge_key: key },
        },
      }) => {
        const tempIdentity = await ECDSAKeyIdentity.generate({
          extractable: false,
        });
        const result = await connection.register({
          identity,
          tempIdentity,
          alias,
          challengeResult: { chars, key },
        });

        return result;
      },
    });

    if ("tag" in result) {
      result satisfies { tag: "canceled" };
      return result;
    }

    return apiResultToLoginFlowResult(result);
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

  const browser = uaParser.getBrowser().name;
  if (browser === "Chrome") {
    // Chrome has a concept of shared "Passkeys". If the user is signed in (to Chrome), then Chrome will create a
    // Passkey shared across all the users' Chromes; if the user is _not_ signed in, then it will be local. We haven't
    // found a way to figure out if the generated Passkey is shared or not, so to be safe we just say "Chrome" as a tradeoff
    // between "Chrome on [OS]" (local) and "Chrome Passkey" (shared).
    return "Chrome";
  }

  // As a last resort, we try to show something like "Opera on Linux" or just "Opera" or just "Linux"
  const os = uaParser.getOS().name;
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
  }
};
