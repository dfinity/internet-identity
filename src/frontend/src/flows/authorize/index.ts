import {
  authenticateBox,
  AuthnTemplates,
} from "$src/components/authenticateBox";
import { displayError } from "$src/components/displayError";
import { caretDownIcon, spinner } from "$src/components/icons";
import { showMessage } from "$src/components/message";
import { BASE_URL } from "$src/environment";
import { getDapps } from "$src/flows/dappsExplorer/dapps";
import { recoveryWizard } from "$src/flows/recovery/recoveryWizard";
import { I18n } from "$src/i18n";
import { Connection } from "$src/utils/iiConnection";
import { TemplateElement } from "$src/utils/lit-html";
import { Chan, unreachable } from "$src/utils/utils";
import { html, render, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { authenticationProtocol } from "./postMessageInterface";

import { nonNullish } from "@dfinity/utils";
import copyJson from "./index.json";

/* Template for the authbox when authenticating to a dapp */
export const authnTemplateAuthorize = ({
  origin,
  derivationOrigin,
  knownDapp,
  i18n,
}: {
  origin: string;
  derivationOrigin?: string;
  knownDapp?: { name: string; logo: string };
  i18n: I18n;
}): AuthnTemplates => {
  const copy = i18n.i18n(copyJson);

  // A couple of HTML elements used on all variations of the screen
  const strong = (slot: TemplateElement) =>
    html`<strong class="t-strong">${slot}</strong>`;
  const h1 = (slot: TemplateElement) =>
    html`<h1
      class="t-title t-title--main"
      style="text-align: left; display: inline;"
    >
      ${slot}
    </h1>`;

  // The chasm message on alternative origin
  const isAltOriginOf = (action: "pick" | "use_existing" | "first_time") =>
    nonNullish(derivationOrigin) && derivationOrigin !== origin
      ? html`
          ${copy[`${action}_alternative_of`]} ${strong(origin)}
          ${copy[`${action}_alternative_of_join`]} ${strong(derivationOrigin)}
        `
      : undefined;

  // There are a few big variations of this screen that share some common elements:
  //  * "first time users" on a known dapp, where the dapp logo is shown
  //  * "first time users" on an UNKNOWN dapp, where a generic title is shown
  //  * "returning users" which includes some small variations depending on alternative origins, if the dapp is known, etc.

  // Variation: the dapp is known and user is using II for the first time
  const firstTimeKnown = ({
    action,
    name,
    logo,
  }: {
    action: "pick" | "use_existing" | "first_time";
    name: string;
    logo: string;
  }) => html`
    <img data-role="known-dapp-image" class="c-dapp-logo" src=${
      BASE_URL + logo
    }></img>
    <div class="l-stack">
      ${h1(
        html`${copy.first_time_title_1}<br />${copy.first_time_title_join}
          ${name} `
      )}
      ${mkChasm({ message: isAltOriginOf(action) ?? strong(origin) })}
      <p class="t-lead l-stack">${copy.first_time_subtitle}</p>
   </div>
  `;

  // Variation: the dapp is NOT known and user is using II for the first time
  const firstTimeUnknown = (action: "pick" | "use_existing" | "first_time") => {
    const altOrigin = isAltOriginOf(action);
    return html`
      <div class="l-stack">
        ${h1(copy.first_time_create)}
        <p class="t-lead l-stack">${copy.first_time_unknown_subtitle}</p>
        ${nonNullish(altOrigin)
          ? mkChasm({ info: strong(origin), message: altOrigin })
          : strong(origin)}
      </div>
    `;
  };

  // Variation: the user has used II before
  const returning = (action: "pick" | "use_existing") => {
    const altOrigin = isAltOriginOf(action);
    return html`
      <div class="l-stack">
        ${h1(
          html`${copy[`${action}_title_1`]}<br />${copy[`${action}_title_2`]}`
        )}
        <p class="t-lead l-stack">
          ${copy[`${action}_subtitle`]} ${copy[`${action}_subtitle_join`]}
          ${nonNullish(knownDapp)
            ? mkChasm({
                info: strong(knownDapp.name),
                message: altOrigin ?? strong(origin),
              })
            : nonNullish(altOrigin)
            ? mkChasm({ info: strong(origin), message: altOrigin })
            : strong(origin)}
        </p>
      </div>
    `;
  };

  return {
    firstTime: {
      slot: nonNullish(knownDapp)
        ? firstTimeKnown({ ...knownDapp, action: "first_time" })
        : firstTimeUnknown("first_time"),
      useExistingText: copy.first_time_use,
      createAnchorText: copy.first_time_create_text,
    },
    useExisting: {
      slot: returning("use_existing"),
    },
    pick: {
      slot: returning("pick"),
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
  const dapps = getDapps();
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
        i18n,
        authnTemplateAuthorize({
          origin: authContext.requestOrigin,
          derivationOrigin: authContext.authRequest.derivationOrigin,
          i18n,
          knownDapp: dapps.find(
            (dapp) => new URL(dapp.link).origin === authContext.requestOrigin
          ),
        })
      );

      // Here, if the user is returning & doesn't have any recovery device, we prompt them to add
      // one. The exact flow depends on the device they use.
      if (!authSuccess.newAnchor) {
        await recoveryWizard(authSuccess.userNumber, authSuccess.connection);
      }
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
  info?: TemplateElement;
  message: TemplateResult;
};

const mkChasm = ({ info, message }: ChasmOpts): TemplateResult => {
  /* Toggle the chasm open/closed */
  const ariaExpanded = new Chan(false);
  const chasmToggle = () => ariaExpanded.send(!ariaExpanded.latest);
  const btnFlipped = ariaExpanded.map((expanded) =>
    expanded ? "c-chasm__button--flipped" : undefined
  );

  return html`
    <span
      id="alternative-origin-chasm-toggle"
      style="cursor: pointer;"
      class="t-action"
      data-action="toggle-chasm"
      @click=${() => chasmToggle()}
      >${info}
      <span class="t-link__icon c-chasm__button ${asyncReplace(btnFlipped)}"
        >${caretDownIcon}</span
      ></span
    >
    <div
      class="c-chasm c-chasm--title"
      aria-expanded=${asyncReplace(ariaExpanded)}
    >
      <div class="c-chasm__inner">
        <div class="c-chasm__arrow"></div>
        <div class="t-weak c-chasm__content">
          <p class="t-paragraph t-paragraph--weak">${message}</p>
        </div>
      </div>
    </div>
  `;
};
