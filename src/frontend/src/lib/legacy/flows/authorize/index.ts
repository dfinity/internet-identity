import {
  AuthnTemplates,
  authenticateBox,
} from "$lib/templates/authenticateBox";
import { displayError } from "$lib/templates/displayError";
import { caretDownIcon } from "$lib/templates/icons";
import { withLoader } from "$lib/templates/loader";
import { showMessage } from "$lib/templates/message";
import { showSpinner } from "$lib/templates/spinner";
import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
import { recoveryWizard } from "$lib/legacy/flows/recovery/recoveryWizard";
import { I18n } from "$lib/legacy/i18n";
import { getAnchorIfLastUsed, setKnownPrincipal } from "$lib/legacy/storage";
import { Connection } from "$lib/utils/iiConnection";
import { TemplateElement } from "$lib/utils/lit-html";
import { Chan } from "$lib/utils/utils";
import { SignedDelegation as FrontendSignedDelegation } from "@dfinity/identity";
import { Principal } from "@dfinity/principal";
import { nonNullish } from "@dfinity/utils";
import { TemplateResult, html } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import { fetchDelegation } from "./fetchDelegation";
import copyJson from "./index.json";
import { AuthContext, authenticationProtocol } from "./postMessageInterface";

/* Template for the authbox when authenticating to a dapp */
export const authnTemplateAuthorize = ({
  origin,
  derivationOrigin,
  knownDapp,
  i18n,
}: {
  origin: string;
  derivationOrigin?: string;
  knownDapp?: { name: string; logoSrc: string };
  i18n: I18n;
}): AuthnTemplates => {
  const copy = i18n.i18n(copyJson);

  // A couple of HTML elements used on all variations of the screen
  const strong = (slot: TemplateElement) =>
    html`<strong class="t-strong">${slot}</strong>`;
  const h1 = (slot: TemplateElement) =>
    html`<h1 class="t-title t-title--main" style="display: inline;">
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
    logoSrc,
  }: {
    action: "pick" | "use_existing" | "first_time";
    name: string;
    logoSrc: string;
  }) => html`
    <div class="c-origin-preview c-origin-preview--header">
      <img
        class="c-origin-preview__logo c-origin-preview__logo--background"
        src=${logoSrc}
        alt=""
      />
      <img
        data-role="known-dapp-image"
        class="c-origin-preview__logo"
        src=${logoSrc}
        alt=""
      />
    </div>
    <div class="l-stack">
      ${h1(
        html`${copy.first_time_title_1}<br />${copy.first_time_title_join}
          ${name} `,
      )}
      ${mkChasm({ message: isAltOriginOf(action) ?? strong(origin) })}
    </div>
  `;

  // Variation: the dapp is NOT known and user is using II for the first time
  const firstTimeUnknown = (action: "pick" | "use_existing" | "first_time") => {
    const altOrigin = isAltOriginOf(action);
    return html`
      <div>
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
    // The "use_existing" screen has a different layout (mainWindow) than the "pick" screen (landingPage).
    // Ideally the outer space would be handled by the parent.
    const classAttribute = action === "use_existing" ? 'class="l-stack"' : "";
    return html`
      <div ${classAttribute}>
        ${h1(html`${copy[`${action}_title_1`]}`)}
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
        ? firstTimeKnown({
            // XXX: cannot destructure, because getter values get lost
            logoSrc: knownDapp.logoSrc,
            name: knownDapp.name,
            action: "first_time",
          })
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

const authenticate = async (
  connection: Connection,
  authContext: AuthContext,
): Promise<
  | {
      kind: "success";
      delegations: FrontendSignedDelegation[];
      userPublicKey: Uint8Array;
      authnMethod: "pin" | "passkey" | "recovery";
    }
  | { kind: "failure"; text: string }
> => {
  const i18n = new I18n();
  const copy = i18n.i18n(copyJson);

  const validationResult = await validateDerivationOrigin({
    requestOrigin: authContext.requestOrigin,
    derivationOrigin: authContext.authRequest.derivationOrigin,
  });

  if (validationResult.result === "invalid") {
    await displayError({
      title: copy.invalid_derivation_origin,
      message: html`"${authContext.authRequest.derivationOrigin}"
      ${copy.is_not_valid_origin_for} "${authContext.requestOrigin}"`,
      detail: validationResult.message,
      primaryButton: copy.continue,
    });

    return {
      kind: "failure",
      text: `Invalid derivation origin: ${validationResult.message}`,
    };
  }

  let autoSelectionIdentity = undefined;
  if (nonNullish(authContext.authRequest.autoSelectionPrincipal)) {
    autoSelectionIdentity = await getAnchorIfLastUsed({
      principal: authContext.authRequest.autoSelectionPrincipal,
      origin: authContext.requestOrigin,
    });
  }

  const authSuccess = await authenticateBox({
    connection,
    i18n,
    templates: authnTemplateAuthorize({
      origin: authContext.requestOrigin,
      derivationOrigin: authContext.authRequest.derivationOrigin,
      i18n,
      knownDapp: getDapps().find((dapp) =>
        dapp.hasOrigin(authContext.requestOrigin),
      ),
    }),
    allowPinLogin: authContext.authRequest.allowPinAuthentication ?? true,
    // Registration with PIN is not allowed anymore
    allowPinRegistration: false,
    autoSelectionIdentity: autoSelectionIdentity,
  });

  // TODO: Remove if we see more problems logging in.
  // The reason to remove this is that two power users (Björn and Jan) were confused with this screen.
  // If technical power users are confused, then normal users will also be confused.
  // if (authSuccess.showAddCurrentDevice && DOMAIN_COMPATIBILITY.isEnabled()) {
  //   await registerCurrentDeviceCurrentOrigin(
  //     authSuccess.userNumber,
  //     authSuccess.connection
  //   );
  // }

  // at this point, derivationOrigin is either validated or undefined
  const derivationOrigin =
    authContext.authRequest.derivationOrigin ?? authContext.requestOrigin;

  const result = await withLoader(() =>
    fetchDelegation({
      connection: authSuccess.connection,
      derivationOrigin,
      publicKey: authContext.authRequest.sessionPublicKey,
      maxTimeToLive: authContext.authRequest.maxTimeToLive,
    }),
  );

  if ("error" in result) {
    return {
      kind: "failure",
      text: "Unexpected error",
    };
  }

  // Here, if the user is returning & doesn't have any recovery device, we prompt them to add
  // one. The exact flow depends on the device they use.
  // XXX: Must happen before auth protocol is done, otherwise the authenticating dapp
  // may have already closed the II window
  if (!authSuccess.newAnchor) {
    await recoveryWizard(authSuccess.userNumber, authSuccess.connection);
  }

  // Ignore the response of committing the metadata because it's not crucial.
  void authSuccess.connection.commitMetadata();

  const [userKey, parsed_signed_delegation] = result;

  const userPublicKey = Uint8Array.from(userKey);
  const principal = Principal.selfAuthenticating(userPublicKey);

  await setKnownPrincipal({
    userNumber: authSuccess.userNumber,
    origin: authContext.requestOrigin,
    principal,
  });

  return {
    kind: "success",
    delegations: [parsed_signed_delegation],
    userPublicKey: Uint8Array.from(userKey),
    authnMethod: authSuccess.authnMethod,
  };
};
/** Run the authentication flow, including postMessage protocol, offering to authenticate
 * using an existing anchor or creating a new anchor, etc.
 */
export const authFlowAuthorize = async (
  connection: Connection,
): Promise<never> => {
  const i18n = new I18n();
  const copy = i18n.i18n(copyJson);

  showSpinner({ message: copy.starting_authentication });
  const result = await authenticationProtocol({
    authenticate: (authContext) => authenticate(connection, authContext),

    onProgress: (status) =>
      showSpinner({
        message: {
          waiting: copy.waiting_for_auth_data,
          validating: copy.finalizing_auth,
        }[status],
      }),
  });

  if (result === "orphan") {
    await displayError({
      title: copy.missing_request,
      message: copy.no_request_received,
      primaryButton: copy.go_home,
    });

    location.hash = "";
    return window.location.assign("/") as never;
  }
  if (result === "closed") {
    await displayError({
      title: copy.connection_closed,
      message: copy.connection_could_not_be_established,
      primaryButton: copy.go_home,
    });

    location.hash = "";
    return window.location.assign("/") as never;
  }
  if (result === "invalid") {
    await displayError({
      title: copy.invalid_request,
      message: copy.invalid_authentication_request_received,
      primaryButton: copy.go_home,
    });

    location.hash = "";
    return window.location.assign("/") as never;
  }

  if (result === "failure" || result === "unverified-origin") {
    showMessage({
      message: copy.auth_failed,
    });
    throw new Error("Authentication failure");
  }

  result satisfies "success";
  showMessage({
    role: "notify-auth-success",
    message: copy.auth_success,
  });
  return await new Promise((_) => {
    /* halt */
  });
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
    expanded ? "c-chasm__button--flipped" : undefined,
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
