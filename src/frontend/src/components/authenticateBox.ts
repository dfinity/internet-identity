import { html, render, TemplateResult } from "lit-html";
import { icLogo, caretDownIcon } from "./icons";
import { footer } from "./footer";
import { withLoader } from "./loader";
import { addRemoteDevice } from "../flows/addDevice/welcomeView";
import { mkAnchorInput } from "./anchorInput";
import { getUserNumber, setUserNumber } from "../utils/userNumber";
import { unreachable } from "../utils/utils";
import { Connection } from "../utils/iiConnection";
import { ref, createRef, Ref } from "lit-html/directives/ref.js";
import {
  apiResultToLoginFlowResult,
  LoginFlowResult,
  LoginFlowSuccess,
  LoginFlowError,
  LoginData,
} from "../flows/login/flowResult";
import { displayError } from "./displayError";
import { useRecovery } from "../flows/recovery/useRecovery";
import { registerIfAllowed } from "../utils/registerAllowedCheck";
import { withRef } from "../utils/lit-html";

/** Properties for the authentication box component which authenticates a user
 * to II or to another dapp */
type PageProps = { templates: AuthTemplates } & {
  onContinue: (arg: bigint) => void;
  recoverAnchor: (userNumner?: bigint) => void;
  register: () => void;
  addDevice: () => void;
  userNumber?: bigint;
};

/** Text and content to display in the box */
export type AuthTemplates = {
  message: TemplateResult;
  chasm?: ChasmOpts;
};

/** Options to display a "chasm" in the authbox */
type ChasmOpts = {
  info: string;
  message: TemplateResult;
};

/** Authentication box component which authenticates a user
 * to II or to another dapp */
export const authenticateBox = (
  connection: Connection,
  templates: AuthTemplates
): Promise<LoginData> => {
  const retryOnError = async (result: LoginFlowResult): Promise<LoginData> => {
    switch (result.tag) {
      case "err":
        await displayError({
          title: result.title,
          message: result.message,
          detail: result.detail !== "" ? result.detail : undefined,
          primaryButton: "Try again",
        });
        return authenticateBox(connection, templates);
      case "ok":
        return result;
      case "canceled":
        return authenticateBox(connection, templates);
      default:
        unreachable(result);
        break;
    }
  };

  return new Promise<LoginData>((resolve) => {
    authenticateBoxTemplate({
      templates,
      addDevice: () => addRemoteDevice(connection),
      onContinue: async (userNumber) => {
        const loginData = await authenticate(connection, userNumber).then(
          retryOnError
        );
        setUserNumber(loginData.userNumber);
        resolve(loginData);
      },
      register: async () => {
        const loginData = await registerIfAllowed(connection).then(
          retryOnError
        );
        setUserNumber(loginData.userNumber);
        resolve(loginData);
      },
      recoverAnchor: (userNumber) => useRecovery(connection, userNumber),
      userNumber: getUserNumber(),
    });
  });
};

export const authenticateBoxTemplate = (props: PageProps): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(props), container);
};

const pageContent = ({
  templates,
  addDevice,
  onContinue,
  recoverAnchor,
  register,
  userNumber,
}: PageProps): TemplateResult => {
  const anchorInput = mkAnchorInput({
    userNumber,
    onSubmit: onContinue,
  });

  return html` <div class="l-container c-card c-card--highlight">
      <!-- The title is hidden but used for accessibility -->
      <h1 class="is-hidden">Internet Identity</h1>
      <div class="c-logo">${icLogo}</div>
      <div class="l-stack t-centered">
        ${templates.message} ${templates.chasm ? mkChasm(templates.chasm) : ""}
      </div>

      ${anchorInput.template}

      <button
        @click="${anchorInput.submit}"
        id="authorizeButton"
        class="c-button"
      >
        Authorize
      </button>
      ${mkLinks({
        register,
        recoverAnchor: () => recoverAnchor(anchorInput.readUserNumber()),
        addDevice,
      })}
    </div>
    ${footer}`;
};

export const authenticate = async (
  connection: Connection,
  userNumber: bigint
): Promise<LoginFlowSuccess | LoginFlowError> => {
  try {
    const result = await withLoader(() => connection.login(userNumber));
    return apiResultToLoginFlowResult(result);
  } catch (error) {
    return {
      tag: "err",
      title: "Authentication Failed",
      message: "An error occurred during authentication.",
      detail: error instanceof Error ? error.message : JSON.stringify(error),
    };
  }
};

const mkChasm = ({ info, message }: ChasmOpts): TemplateResult => {
  /* the chasm that opens to reveal details about alternative origin */
  const chasmRef: Ref<HTMLDivElement> = createRef();

  /* the (purely visual) arrow on the chasm */
  const chasmToggleRef: Ref<HTMLSpanElement> = createRef();

  /* Toggle the chasm open/closed */
  const chasmToggle = () =>
    withRef(chasmRef, (chasm) => {
      const classes = chasm.classList;

      if (classes.contains("c-chasm--closed")) {
        classes.remove("c-chasm--closed");
        classes.add("c-chasm--open");

        withRef(chasmToggleRef, (arrow) =>
          arrow.classList.add("c-chasm__button--flipped")
        );
      } else if (classes.contains("c-chasm--open")) {
        classes.remove("c-chasm--open");
        classes.add("c-chasm--closed");

        withRef(chasmToggleRef, (arrow) =>
          arrow.classList.remove("c-chasm__button--flipped")
        );
      }
    });

  return html`
    <p class="t-paragraph t-weak"><span id="alternative-origin-chasm-toggle" class="t-action" @click="${chasmToggle}" >${info} <span ${ref(
    chasmToggleRef
  )} class="t-link__icon c-chasm__button">${caretDownIcon}</span></span><br/>
      <div ${ref(chasmRef)} class="c-chasm c-chasm--closed">
        <div class="c-chasm__arrow"></div>
        <div class="t-weak c-chasm__content">
            ${message}
        </div>
      </div>
    </p>
`;
};

const mkLinks = ({
  recoverAnchor,
  register,
  addDevice,
}: {
  recoverAnchor: () => void;
  register: () => void;
  addDevice: () => void;
}) => html`
  <div class="l-stack">
    <ul class="c-list--flex">
      <li>
        <a @click=${register} id="registerButton" class="t-link"
          >Create Anchor</a
        >
      </li>
      <li>
        <a @click="${recoverAnchor}" id="recoverButton" class="t-link"
          >Lost Access?</a
        >
      </li>
      <li>
        <a @click="${addDevice}" id="addNewDeviceButton" class="t-link"
          >Add a device</a
        >
      </li>
    </ul>
  </div>
`;
