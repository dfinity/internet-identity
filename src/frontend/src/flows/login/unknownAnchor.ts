import { html, render, TemplateResult } from "lit-html";
import { Ref } from "lit-html/directives/ref.js";
import { Connection } from "../../utils/iiConnection";
import { parseUserNumber, setUserNumber } from "../../utils/userNumber";
import { withLoader } from "../../components/loader";
import { icLogo } from "../../components/icons";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import { mkAnchorInput } from "../../components/anchorInput";
import { useRecovery } from "../recovery/useRecovery";
import { apiResultToLoginFlowResult, LoginFlowResult } from "./flowResult";
import { addRemoteDevice } from "../addDevice/welcomeView";
import { registerIfAllowed } from "../../utils/registerAllowedCheck";

const pageContent = (props: {
  onContinue: (res: bigint) => void;
}): {
  template: TemplateResult;
  userNumberInput: Ref<HTMLInputElement>;
} => {
  const anchorInput = mkAnchorInput({
    inputId: "registerUserNumber",
    onSubmit: props.onContinue,
  });
  const template = html`
  <section class="l-container c-card c-card--highlight" aria-label="Authentication">
    <div class="c-logo">${icLogo}</div>
    <article class="l-stack">
      <hgroup>
        <h1 id="loginWelcome" class="t-title t-title--main">Welcome to<br />Internet Identity</h1>
        <p class="t-lead">Provide an Identity Anchor to authenticate.</p>
      <hgroup>
      <div class="l-stack">
        ${anchorInput.template}
        <button @click="${anchorInput.submit}" type="button" id="loginButton" class="c-button">
          Authenticate
        </button>
      </div>
    </article>

    <aside class="l-stack l-stack--spacious" aria-label="Other actions">
      <ul class="t-discreet c-list">
        <li class="textLink" id="registerSection">
          New?
          <a id="registerButton" class="t-link">Create an Internet Identity Anchor.</a>
        </li>
        <li class="textLink">
          Already have an anchor
          <a id="addNewDeviceButton" class="t-link">
            but using a new device?
          </a>
        </li>
        <li class="textLink">
          Lost access
          <a id="recoverButton" class="t-link">
            and want to recover?
          </a>
        </li>
      </ul>
    </aside>
    ${navbar}
  </section>
  ${footer}`;

  return { ...anchorInput, template };
};

export const loginUnknownAnchor = async (
  connection: Connection
): Promise<LoginFlowResult> =>
  new Promise((resolve, reject) => {
    const container = document.getElementById("pageContent") as HTMLElement;
    const content = pageContent({
      onContinue: (userNumber) => resolve(doLogin(userNumber, connection)),
    });
    render(content.template, container);
    initLinkDevice(connection);
    initRegister(connection, resolve, reject);
    initRecovery(connection);
  });

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
