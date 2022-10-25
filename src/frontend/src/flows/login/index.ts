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
    const returning: Returning =
      userNumber !== undefined
        ? { returning: true, clearCache: () => {}, userNumber }
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
    inputId: "registerUserNumber",
    onSubmit: props.submit,
    userNumber: props.returning ? props.userNumber : undefined,
  });

  const clearAnchor = () => {
    const message = `This will forget your anchor but your credentials will still stored. To clear your credentials delete browser history.`;
    const confirmed = confirm(message);
    if (confirmed) {
      window.localStorage.clear();
      window.location.reload();
    }
  };

  const addDeviceClick = () => {
    const userNumberInput = anchorInput.userNumberInput.value;
    const userNumber =
      userNumberInput === undefined
        ? undefined
        : parseUserNumber(userNumberInput.value) ?? undefined;
    props.addDevice(userNumber);
  };

  const template = html`
  <section class="l-container c-card c-card--highlight" aria-label="Authentication">
    <div class="c-logo">${icLogo}</div>
    <article class="l-stack">
      <hgroup class="t-centered">
        <h1 id="loginWelcome" class="t-title t-title--main">Welcome!</h1>
      <hgroup>
      <div class="l-stack">
        ${anchorInput.template}
        <button @click="${
          anchorInput.submit
        }" type="button" id="loginButton" class="c-button">
          Authenticate
        </button>
        <div style="t-centered">
          <span class="t-paragraph t-weak">
            Lost access? <button @click=${
              props.recover
            } id="recoverButton" class="t-link">Recover Anchor </button> or <button @click=${addDeviceClick} id="addNewDeviceButton" class="t-link">Enroll Device</button><br/>
          </span>
        </div>
      </div>
    </article>

    <div class="l-divider" aria-label="Other Options">
    </div>
    <p class="t-paragraph t-weak">
    New here? An Identity Anchor is a unique ID that is used to authenticate yourself. You will be able to use it to log in to all kinds of apps.
    </p>
    <button type="button" @click=${
      props.register
    } id="registerButton" class="c-button c-button--secondary">
      Create an Anchor
    </button>

    ${
      props.returning
        ? html`
            <div class="l-divider" aria-label="Other Options"></div>
            <p class="t-paragraph t-weak">
              Need a fresh start?
              <button @click=${props.clearCache} class="t-link">
                Clear anchor cache</button
              ><br />
            </p>
          `
        : ""
    }
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
