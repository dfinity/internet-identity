import { AuthnMethodData } from "$generated/internet_identity_types";
import { withLoader } from "$src/components/loader";
import {
  PinIdentityMaterial,
  constructPinIdentity,
} from "$src/crypto/pinIdentity";
import { idbStorePinIdentityMaterial } from "$src/flows/pin/idb";
import { registerDisabled } from "$src/flows/registerDisabled";
import { I18n } from "$src/i18n";
import { setAnchorUsed } from "$src/storage";
import {
  passkeyAuthnMethodData,
  pinAuthnMethodData,
} from "$src/utils/authnMethodData";
import {
  AlreadyInProgress,
  ApiError,
  Connection,
  IIWebAuthnIdentity,
  InvalidAuthnMethod,
  InvalidCaller,
  LoginSuccess,
  NoRegistrationFlow,
  RateLimitExceeded,
  RegisterNoSpace,
  RegistrationFlowStepSuccess,
  UnexpectedCall,
  WrongCaptchaSolution,
} from "$src/utils/iiConnection";
import { SignIdentity } from "@dfinity/agent";
import { ECDSAKeyIdentity } from "@dfinity/identity";
import { nonNullish } from "@dfinity/utils";
import { TemplateResult } from "lit-html";
import type { UAParser } from "ua-parser-js";
import { tempKeyWarningBox } from "../manage/tempKeys";
import { setPinFlow } from "../pin/setPin";
import { precomputeFirst, promptCaptcha } from "./captcha";
import { displayUserNumberWarmup } from "./finish";
import { savePasskeyOrPin } from "./passkey";

/** Registration (identity creation) flow for new users */
export const registerFlow = async ({
  identityRegistrationStart,
  checkCaptcha,
  identityRegistrationFinish,
  storePinIdentity,
  registrationAllowed,
  pinAllowed,
  uaParser,
}: {
  identityRegistrationStart: () => Promise<
    | RegistrationFlowStepSuccess
    | ApiError
    | InvalidCaller
    | AlreadyInProgress
    | RateLimitExceeded
  >;
  checkCaptcha: (
    captchaSolution: string
  ) => Promise<
    | RegistrationFlowStepSuccess
    | ApiError
    | NoRegistrationFlow
    | UnexpectedCall
    | WrongCaptchaSolution
  >;
  identityRegistrationFinish: ({
    identity,
    authnMethod,
  }: {
    identity: SignIdentity;
    authnMethod: AuthnMethodData;
  }) => Promise<
    | LoginSuccess
    | ApiError
    | NoRegistrationFlow
    | UnexpectedCall
    | RegisterNoSpace
    | InvalidAuthnMethod
  >;
  storePinIdentity: (opts: {
    userNumber: bigint;
    pinIdentityMaterial: PinIdentityMaterial;
  }) => Promise<void>;
  registrationAllowed: boolean;
  pinAllowed: () => Promise<boolean>;
  uaParser: PreloadedUAParser;
}): Promise<
  | (LoginSuccess & { authnMethod: "passkey" | "pin" })
  | ApiError
  | NoRegistrationFlow
  | UnexpectedCall
  | RegisterNoSpace
  | InvalidAuthnMethod
  | InvalidCaller
  | AlreadyInProgress
  | RateLimitExceeded
  | "canceled"
> => {
  if (!registrationAllowed) {
    const result = await registerDisabled();
    result satisfies { tag: "canceled" };
    return "canceled";
  }

  // Kick-off the flow start request early, so that we might already
  // have a captcha to show once we get to the CAPTCHA screen
  const flowStart = precomputeFirst(() => identityRegistrationStart());

  const displayUserNumber = displayUserNumberWarmup();
  const savePasskeyResult = await savePasskeyOrPin({
    pinAllowed: await pinAllowed(),
  });
  if (savePasskeyResult === "canceled") {
    return "canceled";
  }
  const result_ = await (async () => {
    if (savePasskeyResult === "pin") {
      const pinResult = await setPinFlow();
      if (pinResult.tag === "canceled") {
        return "canceled";
      }

      pinResult.tag satisfies "ok";

      // XXX: this withLoader could be replaced with one that indicates what's happening (like the
      // "Hang tight, ..." spinner)
      const { identity, pinIdentityMaterial } = await withLoader(() =>
        constructPinIdentity(pinResult)
      );
      const alias = await inferPinAlias({
        userAgent: navigator.userAgent,
        uaParser,
      });
      return {
        identity,
        authnMethodData: pinAuthnMethodData({
          alias,
          pubKey: identity.getPublicKey().toDer(),
        }),
        finalizeIdentity: (userNumber: bigint) =>
          storePinIdentity({ userNumber, pinIdentityMaterial }),
        finishSlot: tempKeyWarningBox({ i18n: new I18n() }),
        authnMethod: "pin" as const,
      };
    } else {
      const identity = savePasskeyResult;
      // TODO: Return something meaningful if getting the passkey identity fails
      if (identity === undefined) {
        return "canceled";
      }
      const alias = await inferPasskeyAlias({
        authenticatorType: identity.getAuthenticatorAttachment(),
        userAgent: navigator.userAgent,
        uaParser,
      });
      return {
        identity,
        authnMethodData: passkeyAuthnMethodData({
          alias,
          pubKey: identity.getPublicKey().toDer(),
          credentialId: identity.rawId,
          authenticatorAttachment: identity.getAuthenticatorAttachment(),
        }),
        authnMethod: "passkey" as const,
      };
    }
  })();

  if (result_ === "canceled") {
    return "canceled";
  }

  const {
    identity,
    authnMethodData,
    finalizeIdentity,
    finishSlot,
    authnMethod,
  }: {
    identity: SignIdentity;
    authnMethodData: AuthnMethodData;
    finalizeIdentity?: (userNumber: bigint) => Promise<void>;
    finishSlot?: TemplateResult;
    authnMethod: "pin" | "passkey";
  } = result_;

  const startResult = await flowStart();
  if (startResult.kind !== "registrationFlowStepSuccess") {
    return startResult;
  }
  startResult satisfies RegistrationFlowStepSuccess;

  if (startResult.nextStep.step === "checkCaptcha") {
    const captchaResult = await promptCaptcha({
      captcha_png_base64: startResult.nextStep.captcha_png_base64,
      checkCaptcha,
    });
    if (captchaResult === "canceled") {
      return "canceled";
    }
    if (captchaResult.kind !== "registrationFlowStepSuccess") {
      return captchaResult;
    }
    captchaResult satisfies RegistrationFlowStepSuccess;
  }

  const result = await withLoader(() =>
    identityRegistrationFinish({
      authnMethod: authnMethodData,
      identity,
    })
  );

  if (result.kind !== "loginSuccess") {
    return result;
  }
  result.kind satisfies "loginSuccess";

  const userNumber = result.userNumber;
  await finalizeIdentity?.(userNumber);
  // We don't want to nudge the user with the recovery phrase warning page
  // right after they've created their anchor.
  result.connection.updateIdentityMetadata({
    recoveryPageShownTimestampMillis: Date.now(),
  });
  // Immediately commit (and await) the metadata, so that the identity is fully set up when the user sees the success page
  // This way, dropping of at that point does not negatively impact UX with additional nagging.
  await withLoader(() =>
    Promise.all([result.connection.commitMetadata(), setAnchorUsed(userNumber)])
  );
  await displayUserNumber({
    userNumber,
    marketingIntroSlot: finishSlot,
  });
  return { ...result, authnMethod };
};

export type RegisterFlowOpts = Parameters<typeof registerFlow>[0];

export const getRegisterFlowOpts = async ({
  connection,
  allowPinRegistration,
}: {
  connection: Connection;
  allowPinRegistration: boolean;
}): Promise<RegisterFlowOpts> => {
  // Kick-off fetching "ua-parser-js";
  const uaParser = loadUAParser();
  const tempIdentity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  return {
    /** Check that the current origin is not the explicit canister id or a raw url.
     *  Explanation why we need to do this:
     *  https://forum.dfinity.org/t/internet-identity-deprecation-of-account-creation-on-all-origins-other-than-https-identity-ic0-app/9694
     **/
    registrationAllowed:
      !/(^https:\/\/rdmx6-jaaaa-aaaaa-aaadq-cai\.ic0\.app$)|(.+\.raw\..+)/.test(
        window.origin
      ),
    pinAllowed: () =>
      // If pin auth is disallowed by the authenticating dapp then abort, otherwise check
      // if pin auth is allowed for the user agent
      allowPinRegistration
        ? pinRegisterAllowed({ userAgent: navigator.userAgent, uaParser })
        : Promise.resolve(false),
    identityRegistrationStart: async () =>
      await connection.identity_registration_start({ tempIdentity }),
    checkCaptcha: async (captchaSolution) =>
      await connection.check_captcha({ tempIdentity, captchaSolution }),
    identityRegistrationFinish: async ({ identity, authnMethod }) =>
      await connection.identity_registration_finish({
        tempIdentity,
        identity,
        authnMethod,
      }),
    uaParser,
    storePinIdentity: idbStorePinIdentityMaterial,
  };
};

type AuthenticatorType = ReturnType<
  IIWebAuthnIdentity["getAuthenticatorAttachment"]
>;
type PreloadedUAParser = ReturnType<typeof loadUAParser>;

// Logic for inferring a passkey alias based on the authenticator type & user agent
export const inferPasskeyAlias = async ({
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

export const inferPinAlias = async ({
  userAgent,
  uaParser: uaParser_,
}: {
  userAgent: typeof navigator.userAgent;
  uaParser: PreloadedUAParser;
}): Promise<string> => {
  const UNNAMED = "Unnamed Temporary Key";

  // Otherwise, make sure the UA parser module is loaded, because
  // everything from here will use UA heuristics
  const UAParser = await uaParser_;
  if (UAParser === undefined) {
    return UNNAMED;
  }
  const uaParser = new UAParser(userAgent);
  const browser = uaParser.getBrowser().name;
  // We try to show something like "Temporary Key: Opera on Linux" or just "Temporary Key: Opera" or just "Temporary Key: Linux"
  const os = uaParser.getOS().name;
  const browserOn = [
    ...(nonNullish(browser) ? [browser] : []),
    ...(nonNullish(os) ? [os] : []),
  ];
  if (browserOn.length !== 0) {
    return browserOn.join(" on ");
  }

  // If all else fails, the device is unnamed
  return UNNAMED;
};

// Logic for deciding whether PIN identity registration is allowed based on the user agent
export const pinRegisterAllowed = async ({
  userAgent,
  uaParser: uaParser_,
}: {
  userAgent: typeof navigator.userAgent;
  uaParser: PreloadedUAParser;
}): Promise<boolean> => {
  // Otherwise, make sure the UA parser module is loaded, because
  // everything from here will use UA heuristics
  const UAParser = await uaParser_;
  if (UAParser === undefined) {
    // When in doubt, allow PIN registration
    return true;
  }
  const uaParser = new UAParser(userAgent);
  return uaParser.getDevice().vendor === "Apple";
};

// Dynamically load the user agent parser module
export const loadUAParser = async (): Promise<typeof UAParser | undefined> => {
  try {
    return (await import("ua-parser-js")).default;
  } catch (e) {
    console.error(e);
  }
};
