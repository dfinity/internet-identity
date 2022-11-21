import { html, render, TemplateResult } from "lit-html";
import { icLogo, caretDownIcon } from "./icons";
import { footer } from "./footer";
import { withLoader } from "./loader";
import { addRemoteDevice } from "../flows/addDevice/welcomeView";
import { mkAnchorPicker } from "./anchorPicker";
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
  onContinue: (userNumber: bigint) => void;
  recoverAnchor: (userNumber?: bigint) => void;
  register: () => void;
  addDevice: () => void;
  userNumber?: bigint;
};

/** Text and content to display in the box */
export type AuthTemplates = {
  message: TemplateResult;
  button: string;
  chasm?: ChasmOpts;
};

/** Options to display a "chasm" in the authbox */
type ChasmOpts = {
  info: string;
  message: TemplateResult;
};

/** Authentication box component which authenticates a user
 * to II or to another dapp */
export const authenticateBox = async (
  connection: Connection,
  templates: AuthTemplates
): Promise<LoginData> => {
  const promptAuth = () =>
    new Promise<LoginFlowResult>((resolve) => {
      authenticateBoxTemplate({
        templates,
        addDevice: () => addRemoteDevice(connection),
        onContinue: (userNumber) => {
          resolve(authenticate(connection, userNumber));
        },
        register: () => {
          resolve(registerIfAllowed(connection));
        },
        recoverAnchor: (userNumber) => useRecovery(connection, userNumber),
        userNumber: getUserNumber(),
      });
    });

  // Retry until user has successfully authenticated
  for (;;) {
    const result = await promptAuth();
    switch (result.tag) {
      case "ok":
        setUserNumber(result.userNumber);
        return result;
      case "err":
        await displayError({
          title: result.title,
          message: result.message,
          detail: result.detail !== "" ? result.detail : undefined,
          primaryButton: "Try again",
        });
        break;
      case "canceled":
        break;
      default:
        unreachable(result);
        break;
    }
  }
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
  const anchorInput = mkAnchorPicker({
    savedAnchors: userNumber !== undefined ? [userNumber] : [],
    pick: onContinue,
    button: templates.button,
    addDevice,
    recoverAnchor,
    register,
  });

  return html` <div class="l-container c-card c-card--highlight">
      <!-- The title is hidden but used for accessibility -->
      <h1 data-page="authenticate" class="is-hidden">Internet Identity</h1>
      <div class="c-logo">${icLogo}</div>
      <div class="l-stack t-centered">
        ${templates.message} ${templates.chasm ? mkChasm(templates.chasm) : ""}
      </div>

      ${anchorInput.template}
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
      const expanded = chasm.getAttribute("aria-expanded") === "true";
      if (!expanded) {
        chasm.setAttribute("aria-expanded", "true");

        withRef(chasmToggleRef, (arrow) =>
          arrow.classList.add("c-chasm__button--flipped")
        );
      } else {
        chasm.setAttribute("aria-expanded", "false");

        withRef(chasmToggleRef, (arrow) =>
          arrow.classList.remove("c-chasm__button--flipped")
        );
      }
    });

  return html`
    <p class="t-paragraph t-weak"><span id="alternative-origin-chasm-toggle" class="t-action" @click="${chasmToggle}" >${info} <span ${ref(
    chasmToggleRef
  )} class="t-link__icon c-chasm__button">${caretDownIcon}</span></span>
      <div ${ref(chasmRef)} class="c-chasm" aria-expanded="false">
        <div class="c-chasm__arrow"></div>
        <div class="t-weak c-chasm__content">
            ${message}
        </div>
      </div>
    </p>
`;
};
