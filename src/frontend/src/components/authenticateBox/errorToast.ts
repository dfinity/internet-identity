import { FlowError } from "$src/components/authenticateBox";
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

// Makes the error human readable
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
  badChallenge: () => ({
    title: "Failed to register",
    message:
      "Failed to register with Internet Identity, because the CAPTCHA challenge wasn't successful",
  }),
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
};

export const flowErrorToastTemplate = <K extends FlowError["kind"]>(
  flowError: KindToError<K> & { kind: K }
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
