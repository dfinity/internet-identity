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
    const x = await loginPage(connection, userNumber);

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

export const loginPage = async (
  connection: Connection,
  userNumber?: bigint
): Promise<LoginFlowResult> =>
  new Promise((resolve, reject) => {
    const container = document.getElementById("pageContent") as HTMLElement;
    const content = pageContent({
      onContinue: (userNumber) => resolve(doLogin(userNumber, connection)),
      userNumber,
    });
    render(content.template, container);
    initLinkDevice(connection);
    initRegister(connection, resolve, reject);
    initRecovery(connection);
  });

const pageContent = (props: {
  onContinue: (res: bigint) => void;
  userNumber?: bigint;
}): {
  template: TemplateResult;
  userNumberInput: Ref<HTMLInputElement>;
} => {
  const anchorInput = mkAnchorInput({
    inputId: "registerUserNumber",
    onSubmit: props.onContinue,
    userNumber: props.userNumber,
  });

  const clearAnchor = () => {
    const message = `This will forget your anchor but your credentials will still stored. To clear your credentials delete browser history.`;

    const confirmed = confirm(message);

    if (confirmed) {
      window.localStorage.clear();
      window.location.reload();
    }
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
        <button @click="${anchorInput.submit}" type="button" id="loginButton" class="c-button">
          Authenticate
        </button>
        <div style="text-align: right;">
          <span class="t-paragraph t-weak">
            Lost access? <a id="recoverButton" href="#" class="t-link">Recover Anchor </a><br/>
            New device? <a id="addNewDeviceButton" href="#" class="t-link">Enroll it now </a><br/>
            Fresh start? <a @click=${clearAnchor} href="#" class="t-link">Clear anchor</a><br/>
          </span>
        </div>
      </div>
    </article>
      <div class="l-divider" aria-label="Other Options">
      </div>
      <p class="t-paragraph t-weak">
An Identity Anchor is a unique ID that is used to authenticate yourself. You will be able to use it to log in to all kinds of apps.
      </p>

    <button type="button" id="registerButton" class="c-button c-button--secondary">
      Create an Anchor
    </button>
    ${navbar}
  </section>
  ${footer}`;

  return { ...anchorInput, template };
};

const initRegister = (
  connection: Connection,
  resolve: (res: LoginFlowResult) => void,
  reject: (err: Error) => void
) => {
  const registerButton = document.getElementById(
    "registerButton"
  ) as HTMLButtonElement;
  registerButton.onclick = () => {
    registerIfAllowed(connection)
      .then((res) => {
        if (res === null) {
          window.location.reload();
        } else {
          resolve(res);
        }
      })
      .catch(reject);
  };
};

const initRecovery = (connection: Connection) => {
  const recoverButton = document.getElementById(
    "recoverButton"
  ) as HTMLAnchorElement;
  recoverButton.onclick = () => useRecovery(connection);
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

const initLinkDevice = (connection: Connection) => {
  const addNewDeviceButton = document.getElementById(
    "addNewDeviceButton"
  ) as HTMLButtonElement;

  addNewDeviceButton.onclick = async () => {
    const userNumberInput = document.getElementById(
      "registerUserNumber"
    ) as HTMLInputElement;

    const userNumber = parseUserNumber(userNumberInput.value) ?? undefined;
    await addRemoteDevice(connection, userNumber);
  };
};
