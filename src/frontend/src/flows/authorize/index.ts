import {
  authenticateBox,
  AuthnTemplates,
} from "$src/components/authenticateBox";
import { displayError } from "$src/components/displayError";
import { caretDownIcon, spinner } from "$src/components/icons";
import { showMessage } from "$src/components/message";
import { recoveryWizard } from "$src/flows/recovery/recoveryWizard";
import { DynamicKey, I18n } from "$src/i18n";
import { Connection } from "$src/utils/iiConnection";
import { TemplateElement, withRef } from "$src/utils/lit-html";
import { unreachable } from "$src/utils/utils";
import { html, render, TemplateResult } from "lit-html";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";
import { authenticationProtocol } from "./postMessageInterface";

import copyJson from "./index.json";

/* Template for the authbox when authenticating to a dapp */
export const authnTemplateAuthorize = ({
  origin,
  derivationOrigin,
  i18n,
}: {
  origin: string;
  derivationOrigin?: string;
  i18n: I18n;
}): AuthnTemplates => {
  const copy = i18n.i18n(copyJson);

  const marketingBlocks = [
    {
      title: copy.secure_and_convenient,
      body: copy.instead_of_passwords,
    },
    {
      title: copy.no_tracking,
      body: copy.get_privacy,
    },
    {
      title: copy.control_your_identity,
      body: copy.securely_access,
    },
    {
      title: copy.own_and_participate,
      body: copy.share_and_vote,
    },
    {
      title: copy.sign_in_to_web3,
      body: copy.manages_keys,
    },
    {
      title: copy.opensource_and_transparent,
      body: copy.internet_identity_codebase,
    },
  ];

  const chasm =
    derivationOrigin !== undefined && derivationOrigin !== origin
      ? mkChasm({
          info: "shared identity",
          message: html`<span class="t-strong">${origin}</span>
            ${copy.is_alternative_of} <br /><span class="t-strong"
              >${derivationOrigin}</span
            ><br />${copy.auth_same_identity}`,
        })
      : undefined;

  const wrap = (title: DynamicKey) => html`
    <div class="t-centered">
      <h1 class="t-title t-title--main">${title}</h1>
      <p class="t-lead">
        ${copy.to_continue_to}
        <a href="${origin}" target="_blank" rel="noopener noreferrer"
          >${origin}</a
        ><br />
      </p>
      ${chasm}
    </div>
  `;

  const marketing = (block: {
    title: DynamicKey;
    body: DynamicKey;
  }): TemplateResult => html`
    <div class="c-marketing-block">
      <h2 class="t-title t-title--main">${block.title}</h2>
      <p class="t-paragraph t-weak">${block.body}</p>
    </div>
  `;

  return {
    firstTime: {
      slot: wrap(copy.first_time_create),
      useExistingText: copy.first_time_use,
      createAnchorText: copy.first_time_create_text,
      additionalInfo: html`${marketingBlocks.map(marketing)}`,
    },
    useExisting: {
      slot: wrap(copy.use_existing_enter_anchor),
    },
    pick: {
      slot: wrap(copy.pick_choose_anchor),
    },
  };
};

/** Run the authentication flow, including postMessage protocol, offering to authenticate
 * using an existing anchor or creating a new anchor, etc.
 */
export const authFlowAuthorize = async (
  connection: Connection
): Promise<void> => {
  const i18n = new I18n();
  const container = document.getElementById("pageContent") as HTMLElement;
  const copy = i18n.i18n(copyJson);
  render(html`<h1>${copy.starting_authentication}</h1>`, container);
  const loadingMessage = (msg: TemplateElement) =>
    render(
      html`
        <div class="l-container c-card c-card--highlight t-centered">
          <div class="c-spinner-wrapper">
            <div class="c-spinner">${spinner}</div>
          </div>
          <p class="t-lead t-paragraph l-stack">${msg}</p>
        </div>
      `,
      container
    );
  const result = await authenticationProtocol({
    authenticate: async (authContext) => {
      const authSuccess = await authenticateBox(
        connection,
        authnTemplateAuthorize({
          origin: authContext.requestOrigin,
          derivationOrigin: authContext.authRequest.derivationOrigin,
          i18n,
        })
      );
      await recoveryWizard(authSuccess.userNumber, authSuccess.connection);
      return authSuccess;
    },
    onInvalidOrigin: (result) =>
      displayError({
        title: copy.invalid_derivation_origin,
        message: html`"${result.authContext.authRequest.derivationOrigin}"
        ${copy.is_not_valid_origin_for} "${result.authContext.requestOrigin}"`,
        detail: result.message,
        primaryButton: copy.continue,
      }),

    onProgress: (status) => {
      switch (status) {
        case "waiting":
          loadingMessage(copy.waiting_for_auth_data);
          break;
        case "validating":
          loadingMessage(copy.validating_auth_data);
          break;
        case "fetching delegation":
          loadingMessage(copy.finalizing_auth);
          break;
        default:
          unreachable(status);
          break;
      }
    },
  });

  switch (result) {
    case "orphan":
      await displayError({
        title: copy.invalid_data,
        message: copy.no_auth_data,
        primaryButton: copy.go_home,
      });

      location.hash = "";
      window.location.reload();
      break;
    case "failure":
      render(html`<h1>${copy.auth_failed}</h1>`, container);
      break;
    case "success":
      showMessage({
        role: "notify-auth-success",
        message: copy.auth_success,
      });
      break;
    default:
      unreachable(result);
      break;
  }
};

/** Options to display a "chasm" in the authbox */
type ChasmOpts = {
  info: string;
  message: TemplateResult;
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
