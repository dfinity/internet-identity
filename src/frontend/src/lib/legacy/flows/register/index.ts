import type { AuthnMethodData } from "$lib/generated/internet_identity_types";
import { withLoader } from "$lib/templates/loader";
import {
  PinIdentityMaterial,
  constructPinIdentity,
} from "$lib/legacy/crypto/pinIdentity";
import { OPENID_AUTHENTICATION } from "$lib/state/featureFlags";
import { get } from "svelte/store";
import { anyFeatures } from "$lib/legacy/features";
import { idbStorePinIdentityMaterial } from "$lib/legacy/flows/pin/idb";
import { registerDisabled } from "$lib/legacy/flows/registerDisabled";
import { I18n } from "$lib/legacy/i18n";
import { setAnchorUsed } from "$lib/legacy/storage";
import {
  passkeyAuthnMethodData,
  pinAuthnMethodData,
} from "$lib/utils/authnMethodData";
import {
  AlreadyInProgress,
  ApiError,
  Connection,
  IIWebAuthnIdentity,
  InvalidAuthnMethod,
  InvalidCaller,
  LoginSuccess,
  MissingGoogleClientId,
  NoRegistrationFlow,
  RateLimitExceeded,
  RegisterNoSpace,
  RegistrationFlowStepSuccess,
  UnexpectedCall,
  WrongCaptchaSolution,
} from "$lib/utils/iiConnection";
import { isRegistrationAllowed } from "$lib/utils/isRegistrationAllowed";
import { lookupAAGUID } from "$lib/utils/webAuthn";
import { SignIdentity } from "@dfinity/agent";
import { ECDSAKeyIdentity } from "@dfinity/identity";
import { nonNullish } from "@dfinity/utils";
import { TemplateResult } from "lit-html";
import type { UAParser } from "ua-parser-js";
import { tempKeyWarningBox } from "../manage/tempKeys";
import { setPinFlow } from "../pin/setPin";
import { precomputeFirst, promptCaptcha } from "./captcha";
import { displayUserNumberWarmup } from "./finish";
import { savePasskeyPinOrOpenID } from "./passkey";
import {
  RegistrationEvents,
  registrationFunnel,
} from "$lib/utils/analytics/registrationFunnel";

/** Registration (identity creation) flow for new users */
export const registerFlow = async ({
  identityRegistrationStart,
  checkCaptcha,
  identityRegistrationFinish,
  storePinIdentity,
  registrationAllowed,
  pinAllowed,
  uaParser,
  googleAllowed,
  openidIdentityRegistrationFinish,
}: {
  identityRegistrationStart: () => Promise<
    | RegistrationFlowStepSuccess
    | ApiError
    | InvalidCaller
    | AlreadyInProgress
    | RateLimitExceeded
  >;
  checkCaptcha: (
    captchaSolution: string,
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
  registrationAllowed: { isAllowed: boolean; allowedOrigins: string[] };
  pinAllowed: () => Promise<boolean>;
  uaParser: PreloadedUAParser;
  googleAllowed: boolean;
  openidIdentityRegistrationFinish: () => Promise<
    | LoginSuccess
    | ApiError
    | NoRegistrationFlow
    | UnexpectedCall
    | RegisterNoSpace
    | InvalidAuthnMethod
    | MissingGoogleClientId
  >;
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
  | MissingGoogleClientId
  | "canceled"
> => {
  if (!registrationAllowed.isAllowed) {
    const result = await registerDisabled(registrationAllowed.allowedOrigins);
    result satisfies { tag: "canceled" };
    return "canceled";
  }

  // Kick-off the flow start request early, so that we might already
  // have a captcha to show once we get to the CAPTCHA screen
  const flowStart = precomputeFirst(() => identityRegistrationStart());

  const displayUserNumber = displayUserNumberWarmup();
  // We register the device's origin in the current domain.
  // If we want to change it, we need to change this line.
  const deviceOrigin = window.location.origin;
  registrationFunnel.trigger(RegistrationEvents.Trigger);
  const savePasskeyResult = await savePasskeyPinOrOpenID({
    pinAllowed: await pinAllowed(),
    googleAllowed,
    origin: deviceOrigin,
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
        constructPinIdentity(pinResult),
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
    } else if (savePasskeyResult === "google") {
      const _startResult = await captchaIfNecessary(flowStart, checkCaptcha);
      if (_startResult === "canceled") {
        return "canceled";
      } else if (_startResult.kind !== "registrationFlowStepSuccess") {
        return _startResult;
      }

      const openIdResult = await withLoader(() => {
        return openidIdentityRegistrationFinish();
      });

      if (openIdResult.kind === "loginSuccess") {
        return {
          ...openIdResult,
          authnMethod: "google",
        };
      } else {
        return openIdResult;
      }
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
        aaguid: identity.aaguid,
      });
      return {
        identity,
        authnMethodData: passkeyAuthnMethodData({
          alias,
          pubKey: identity.getPublicKey().toDer(),
          credentialId: identity.rawId,
          authenticatorAttachment: identity.getAuthenticatorAttachment(),
          origin: deviceOrigin,
        }),
        authnMethod: "passkey" as const,
      };
    }
  })();

  if (result_ === "canceled") {
    return "canceled";
  } else if (
    // if we have successfully authenticated with google
    // the reason we have to return earlier is we don't actually
    // get any authnMethodData - jwt etc is
    "kind" in result_ &&
    result_.kind === "loginSuccess" &&
    result_.authnMethod === "google"
  ) {
    // for now we switch to passkey here so dapps don't know it's google
    return { ...result_, authnMethod: "passkey" as const };
  } else if ("kind" in result_ && result_.kind === "loginSuccess") {
    // this branch is needed for typescript
    return { ...result_, authnMethod: "passkey" as const };
  } else if ("kind" in result_) {
    // if openid returned some error
    return result_;
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
    authnMethod: "pin" | "passkey" | "google";
  } = result_;

  const startOrCaptchaResult = await captchaIfNecessary(
    flowStart,
    checkCaptcha,
  );
  if (startOrCaptchaResult === "canceled") return "canceled";

  const result = await withLoader(() =>
    identityRegistrationFinish({
      authnMethod: authnMethodData,
      identity,
    }),
  );

  if (result.kind !== "loginSuccess") {
    return result;
  }
  result.kind satisfies "loginSuccess";

  registrationFunnel.trigger(RegistrationEvents.Created);
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
    Promise.all([
      result.connection.commitMetadata(),
      setAnchorUsed(userNumber),
    ]),
  );
  await displayUserNumber({
    userNumber,
    marketingIntroSlot: finishSlot,
  });
  registrationFunnel.trigger(RegistrationEvents.Success);
  return { ...result, authnMethod };
};

export type RegisterFlowOpts = Parameters<typeof registerFlow>[0];

export const getRegisterFlowOpts = async ({
  connection,
  allowPinRegistration,
  getGoogleClientId,
}: {
  connection: Connection;
  allowPinRegistration: boolean;
  getGoogleClientId: () => string | undefined;
}): Promise<RegisterFlowOpts> => {
  // Kick-off fetching "ua-parser-js";
  const uaParser = loadUAParser();
  const tempIdentity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const registrationAllowed =
    // Allow registration in DEV mode
    anyFeatures() ||
    isRegistrationAllowed(connection.canisterConfig, window.location.origin);
  const allowedOrigins = connection.canisterConfig.related_origins[0] || [];
  return {
    registrationAllowed: { isAllowed: registrationAllowed, allowedOrigins },
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
    googleAllowed:
      get(OPENID_AUTHENTICATION) &&
      (connection.canisterConfig?.openid_google?.[0]?.length ?? 0) > 0,
    openidIdentityRegistrationFinish: () =>
      connection.openid_identity_registration_finish(
        getGoogleClientId,
        tempIdentity,
      ),
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
  aaguid,
}: {
  authenticatorType: AuthenticatorType;
  userAgent: typeof navigator.userAgent;
  uaParser: PreloadedUAParser;
  aaguid?: string;
}): Promise<string> => {
  // First lookup if alias can be found in known list
  // before falling back to user agent implementation.
  if (nonNullish(aaguid)) {
    const knownName = await lookupAAGUID(aaguid);
    if (nonNullish(knownName)) {
      return knownName;
    }
  }

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

/**
 * Handles the captcha verification step if required by the registration flow
 * @returns The registration flow step result or "canceled" if the user canceled
 */
async function captchaIfNecessary(
  flowStart: () => Promise<
    | RegistrationFlowStepSuccess
    | ApiError
    | InvalidCaller
    | AlreadyInProgress
    | RateLimitExceeded
  >,
  checkCaptcha: (
    captchaSolution: string,
  ) => Promise<
    | RegistrationFlowStepSuccess
    | ApiError
    | NoRegistrationFlow
    | UnexpectedCall
    | WrongCaptchaSolution
  >,
): Promise<
  | RegistrationFlowStepSuccess
  | ApiError
  | InvalidCaller
  | AlreadyInProgress
  | RateLimitExceeded
  | NoRegistrationFlow
  | UnexpectedCall
  | "canceled"
> {
  const startResult = await flowStart();
  if (startResult.kind !== "registrationFlowStepSuccess") {
    return startResult;
  }
  startResult satisfies RegistrationFlowStepSuccess;

  if (startResult.nextStep.step === "checkCaptcha") {
    registrationFunnel.trigger(RegistrationEvents.CaptchaCheck);
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
    return captchaResult;
  }

  // If no captcha was needed, return the original success result
  return startResult;
}
