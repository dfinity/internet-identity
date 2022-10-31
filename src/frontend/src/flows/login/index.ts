import { AuthenticatedConnection, Connection } from "../../utils/iiConnection";
import { Ref } from "lit-html/directives/ref.js";
import { addRemoteDevice } from "../addDevice/welcomeView";
import { apiResultToLoginFlowResult, LoginFlowResult } from "./flowResult";
import { displayError } from "../../components/displayError";
import { footer } from "../../components/footer";
import { getUserNumber } from "../../utils/userNumber";
import { html, render, TemplateResult } from "lit-html";
import { icLogo } from "../../components/icons";
import { mkAnchorInput } from "../../components/anchorInput";
import { navbar } from "../../components/navbar";
import { parseUserNumber, setUserNumber } from "../../utils/userNumber";
import { registerIfAllowed } from "../../utils/registerAllowedCheck";
import { unknownToString } from "../../utils/utils";
import { useRecovery } from "../recovery/useRecovery";
import { withLoader } from "../../components/loader";

/** Data and callbacks used on the login page */
type PageProps = {
  submit: (res: bigint) => void;
  addDevice: (userNumber: bigint | undefined) => void;
  recover: () => void;
  register: () => void;
} & Returning;

/** Type representing either a returning user (with saved anchor and function to clear
 * said anchor) or a first time user */
export type Returning =
  | { returning: true; clearCache: () => void; userNumber: bigint }
  | { returning: false };

// We retry logging in until we get a successful Identity Anchor connection pair
// If we encounter an unexpected error we reload to be safe
export const login = async (
  connection: Connection
): Promise<{
  userNumber: bigint;
  connection: AuthenticatedConnection;
}> => {
  try {
    const userNumber = getUserNumber();
    const clearCache = () => {
      const message = `This will forget your anchor number but your credentials will still be stored. To clear your credentials please check your device's documentation on how to clear WebAuthn credentials.`;
      const confirmed = confirm(message);
      if (confirmed) {
        window.localStorage.clear();
        window.location.reload();
      }
    };

    const returning: Returning =
      userNumber !== undefined
        ? { returning: true, clearCache, userNumber }
        : { returning: false };
    const x = await new Promise<LoginFlowResult>((resolve, reject) => {
      loginPage({
        submit: (userNumber) => doLogin(userNumber, connection).then(resolve),
        addDevice: (userNumber) => addRemoteDevice(connection, userNumber),
        recover: () => useRecovery(connection),
        register: () =>
          registerIfAllowed(connection)
            .then((res) => {
              if (res === null) {
                window.location.reload();
              } else {
                resolve(res);
              }
            })
            .catch(reject),
        ...returning,
      });
    });

    switch (x.tag) {
      case "ok": {
        return { userNumber: x.userNumber, connection: x.connection };
      }
      case "err": {
        await displayError({ ...x, primaryButton: "Try again" });
        return login(connection);
      }
    }
  } catch (err: unknown) {
    await displayError({
      title: "Something went wrong",
      message:
        "An unexpected error occurred during authentication. Please try again",
      detail: unknownToString(err, "Unknown error"),
      primaryButton: "Try again",
    });
    window.location.reload();
    return Promise.reject(err);
  }
};

export const loginPage = (props: PageProps): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  const content = pageContent(props);
  render(content.template, container);
};

const pageContent = (
  props: PageProps
): {
  template: TemplateResult;
  userNumberInput: Ref<HTMLInputElement>;
} => {
  const anchorInput = mkAnchorInput({
    onSubmit: props.submit,
    userNumber: props.returning ? props.userNumber : undefined,
    focus: props.returning,
  });

  const addDeviceClick = () => {
    const userNumberInput = anchorInput.userNumberInput.value;
    const userNumber =
      userNumberInput === undefined
        ? undefined
        : parseUserNumber(userNumberInput.value) ?? undefined;
    props.addDevice(userNumber);
  };

  const signin = html`
    <div class="${props.returning && "l-stack"}">
      ${anchorInput.template}
      <button
        @click="${anchorInput.submit}"
        type="button"
        id="loginButton"
        class="c-button ${props.returning ? "" : "c-button--secondary"}"
      >
        Authenticate
      </button>
      <p class="t-paragraph t-weak t-centered">
        Using this device for the first time?
        <button @click=${addDeviceClick} id="addNewDeviceButton" class="t-link">
          Add this device</button
        ><br />
        Lost access?
        <button @click=${props.recover} id="recoverButton" class="t-link">
          Recover anchor
        </button>
      </p>
    </div>
  `;

  const signup = html`
    <div class="${!props.returning && "l-stack"}">
      <p class="t-paragraph t-centered ${props.returning && "t-weak"}">
        Claim an Identity Anchor to interact with apps on the Internet Computer.
      </p>
      <div class="l-stack">
        <button
          type="button"
          @click=${props.register}
          id="registerButton"
          class="c-button ${props.returning ? "c-button--secondary" : ""}"
        >
          Claim an Anchor
        </button>
      </div>
    </div>
  `;

  const dividerReturning = html`
    <div class="l-divider l-divider--text" aria-label="Other Options">New?</div>
  `;

  const divider = html`<div
    class="l-divider l-divider--text"
    aria-label="Other Options"
  >
    Returning?
  </div>`;

  const template = html` <section
      class="l-container c-card c-card--highlight"
      aria-label="Authentication"
    >
      <div class="c-logo">${icLogo}</div>
      <article class="l-stack">
        <h1 id="loginWelcome" class="t-title t-title--main t-centered">
          Unlock the <br />Full Power of Web3
        </h1>

        ${props.returning ? signin : signup}
        ${props.returning ? dividerReturning : divider}
        ${props.returning ? signup : signin}
      </article>

      ${props.returning
        ? html`
            <div class="l-divider" aria-label="Other Options"></div>
            <p class="t-paragraph t-weak t-centered">
              <button @click=${props.clearCache} class="t-link">
                Clear identity anchor from browser
              </button>
            </p>
          `
        : ""}
      ${navbar}
    </section>
    ${footer}`;

  return { ...anchorInput, template };
};

const doLogin = async (
  userNumber: bigint,
  connection: Connection
): Promise<LoginFlowResult> => {
  const result = await withLoader(() => connection.login(userNumber));
  if (result.kind === "loginSuccess") {
    setUserNumber(userNumber);
  }
  return apiResultToLoginFlowResult(result);
};
