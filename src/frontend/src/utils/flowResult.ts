import { ApiResult, AuthenticatedConnection } from "./iiConnection";
import { WEBAUTHN_CANCEL_TEMPLATE } from "./webAuthnErrorUtils";

export type LoginFlowResult =
  | LoginFlowSuccess
  | LoginFlowError
  | LoginFlowCanceled;

export type LoginFlowSuccess = {
  tag: "ok";
} & LoginData;

export type LoginData = {
  userNumber: bigint;
  connection: AuthenticatedConnection;
};

export type LoginFlowError = {
  tag: "err";
} & LoginError;

export type LoginError = {
  title: string;
  message: string;
  detail?: string;
};

/** The result of a login flow that was canceled */
export type LoginFlowCanceled = { tag: "canceled" };
export const cancel: LoginFlowCanceled = { tag: "canceled" };

export const apiResultToLoginFlowResult = (
  result: ApiResult
): LoginFlowSuccess | LoginFlowError => {
  switch (result.kind) {
    case "loginSuccess": {
      return {
        tag: "ok",
        userNumber: result.userNumber,
        connection: result.connection,
      };
    }
    case "authFail": {
      return {
        tag: "err",
        title: "Failed to authenticate",
        message:
          "We failed to authenticate you using your security device. If this is the first time you're trying to log in with this device, you have to add it as a new device first.",
        detail: result.error.message,
      };
    }
    case "unknownUser": {
      return {
        tag: "err",
        title: "Unknown Identity Anchor",
        message: `Failed to find an identity for the Identity Anchor ${result.userNumber}. Please check your Identity Anchor and try again.`,
        detail: "",
      };
    }
    case "apiError": {
      return {
        tag: "err",
        title: "We couldn't reach Internet Identity",
        message:
          "We failed to call the Internet Identity service, please try again.",
        detail: result.error.message,
      };
    }
    case "registerNoSpace": {
      return {
        tag: "err",
        title: "Failed to register",
        message:
          "Failed to register with Internet Identity, because there is no space left at the moment. We're working on increasing the capacity.",
      };
    }
    case "badChallenge": {
      return {
        tag: "err",
        title: "Failed to register",
        message:
          "Failed to register with Internet Identity, because the CAPTCHA challenge wasn't successful",
      };
    }
    case "seedPhraseFail": {
      return {
        tag: "err",
        title: "Invalid Seed Phrase",
        message:
          "Failed to authenticate using this seed phrase. Did you enter it correctly?",
      };
    }
    case "cancelOrTimeout": {
      return {
        tag: "err",
        ...WEBAUTHN_CANCEL_TEMPLATE,
      };
    }
  }
};
