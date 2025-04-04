import { FlowError } from "$lib/templates/authenticateBox";
import { nonNullish } from "@dfinity/utils";
import { html, TemplateResult } from "lit-html";

// Maps all errors kinds to their error types (without kind field):
//  KindToError<'authFail'> = { ...fields of AuthFail with kind...};
// The 'Omit' seems to be a necessary step while looking up the error, otherwise typescript
// thinks the types conflict
type KindToError<K extends FlowError["kind"]> = Omit<
  FlowError & { kind: K },
  "kind"
>;

// Makes the error human-readable
const clarifyError: {
  [K in FlowError["kind"]]: (err: KindToError<K>) => {
    title: string;
    message: string;
    detail?: string;
  };
} = {
  authFail: (err) => ({
    title: "Failed to authenticate",
    message:
      "We failed to authenticate you using your security device. If this is the first time you're trying to log in with this device, you have to add it as a new device first.",
    detail: err.error.message,
  }),
  webAuthnFailed: () => ({
    title: "Operation canceled",
    message:
      "The interaction with your security device was canceled or timed out. Please try again.",
  }),
  unknownUser: (err) => ({
    title: "Unknown Internet Identity",
    message: `Failed to find Internet Identity ${err.userNumber}. Please check your Internet Identity and try again.`,
  }),
  apiError: (err) => ({
    title: "We couldn't reach Internet Identity",
    message:
      "We failed to call the Internet Identity service, please try again.",
    detail: err.error.message,
  }),
  badPin: () => ({ title: "Could not authenticate", message: "Invalid PIN" }),
  registerNoSpace: () => ({
    title: "Failed to register",
    message:
      "Failed to register with Internet Identity, because there is no space left at the moment. We're working on increasing the capacity.",
  }),
  pinNotAllowed: () => ({
    title: "PIN method not allowed",
    message:
      "The Dapp you are authenticating to does not allow PIN identities and you only have a PIN identity. Please retry using a Passkey: open a new Internet Identity page, add a passkey and retry.",
  }),
  alreadyInProgress: () => ({
    title: "Registration is already in progress",
    message: "Registration has already been started on this session.",
  }),
  rateLimitExceeded: () => ({
    title: "Registration rate limit exceeded",
    message:
      "Internet Identity is under heavy load. Too many registrations. Please try again later.",
  }),
  invalidCaller: () => ({
    title: "Registration is not allowed using the anonymous identity",
    message:
      "Registration was attempted using the anonymous identity which is not allowed.",
  }),
  invalidAuthnMethod: (err) => ({
    title: "Invalid authentication method",
    message: `Invalid authentication method: ${err.message}.`,
  }),
  noRegistrationFlow: () => ({
    title: "Registration flow timed out",
    message: "Registration flow timed out. Please restart.",
  }),
  unexpectedCall: (err) => ({
    title: "Unexpected call",
    message: `Unexpected call: expected next step "${err.nextStep.step}"`,
  }),
  missingGoogleClientId: () => ({
    title: "Google Sign-in unavailable",
    message: "Sign in with Google accounts is currently unavailable.",
  }),
  googleLoginFailed: () => ({
    title: "Google Sign-in failed",
    message: "Failed to sign in with Google. Please try again.",
  }),
};

export const flowErrorToastTemplate = <K extends FlowError["kind"]>(
  flowError: KindToError<K> & { kind: K },
): TemplateResult => {
  const props = clarifyError[flowError.kind](flowError);
  const detailSlot = nonNullish(props.detail)
    ? html`<div class="l-stack">
        <h4>Error details:</h4>
        <pre data-role="error-detail" class="t-paragraph">${props.detail}</pre>
      </div>`
    : undefined;
  return html`
    <h3 data-error-code=${flowError.kind} class="t-title c-card__title">
      ${props.title}
    </h3>
    <div data-role="warning-message" class="t-paragraph">${props.message}</div>
    ${detailSlot}
  `;
};
