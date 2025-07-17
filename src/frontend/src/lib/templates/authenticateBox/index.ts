import { mkAnchorInput } from "$lib/templates/anchorInput";
import { mkAnchorPicker } from "$lib/templates/anchorPicker";
import { flowErrorToastTemplate } from "$lib/templates/authenticateBox/errorToast";
import { displayError } from "$lib/templates/displayError";
import { landingPage } from "$lib/templates/landingPage";
import { withLoader } from "$lib/templates/loader";
import { mainWindow } from "$lib/templates/mainWindow";
import { promptUserNumber } from "$lib/templates/promptUserNumber";
import { toast } from "$lib/templates/toast";
import {
  PinIdentityMaterial,
  reconstructPinIdentity,
} from "$lib/legacy/crypto/pinIdentity";
import { OPENID_AUTHENTICATION } from "$lib/state/featureFlags";
import { get } from "svelte/store";
import { registerTentativeDevice } from "$lib/legacy/flows/addDevice/welcomeView/registerTentativeDevice";
import { idbRetrievePinIdentityMaterial } from "$lib/legacy/flows/pin/idb";
import { usePin } from "$lib/legacy/flows/pin/usePin";
import { useRecovery } from "$lib/legacy/flows/recovery/useRecovery";
import {
  RegisterFlowOpts,
  getRegisterFlowOpts,
  registerFlow,
} from "$lib/legacy/flows/register";
import { I18n } from "$lib/legacy/i18n";
import { getAnchors, setAnchorUsed } from "$lib/legacy/storage";
import {
  AlreadyInProgress,
  ApiError,
  AuthFail,
  AuthenticatedConnection,
  BadPin,
  Connection,
  GoogleLoginFailed,
  InvalidAuthnMethod,
  InvalidCaller,
  LoginSuccess,
  MissingGoogleClientId,
  NoRegistrationFlow,
  PinUserOtherDomain,
  PossiblyWrongWebAuthnFlow,
  RateLimitExceeded,
  RegisterNoSpace,
  UnexpectedCall,
  UnknownUser,
  WebAuthnFailed,
  bufferEqual,
} from "$lib/utils/iiConnection";
import { TemplateElement, withRef } from "$lib/utils/lit-html";
import {
  createAnonymousNonce,
  createGoogleRequestConfig,
  requestJWT,
} from "$lib/utils/openID";
import { parseUserNumber } from "$lib/utils/userNumber";
import {
  NonEmptyArray,
  isNonEmptyArray,
  unknownToString,
} from "$lib/utils/utils";
import { DerEncodedPublicKey } from "@dfinity/agent";
import { ECDSAKeyIdentity } from "@dfinity/identity";
import { isNullish, nonNullish } from "@dfinity/utils";
import { TemplateResult, html, render } from "lit-html";
import { infoToastTemplate } from "../infoToast";
import infoToastCopy from "../infoToast/copy.json";
import authnTemplatesCopy from "./authnTemplatesCopy.json";
import { LoginEvents, loginFunnel } from "$lib/utils/analytics/loginFunnel";

/** Template used for rendering specific authentication screens. See `authnScreens` below
 * for meaning of "firstTime", "useExisting" and "pick". */
export type AuthnTemplates = {
  firstTime: {
    slot: TemplateResult;
    useExistingText: TemplateElement /** text shown on the button leading to "useExisting" */;
    createAnchorText: TemplateElement /** text shown on the button leading to "useExisting" */;
    landingType?: "firstTime";
  };
  useExisting: {
    slot: TemplateResult;
    landingType?: "useExisting";
  };
  pick: {
    slot: TemplateResult;
    landingType?: "pick";
  };
};

export const authenticateBox = async ({
  connection,
  i18n,
  templates,
  allowPinLogin,
  allowPinRegistration,
  autoSelectionIdentity,
}: {
  connection: Connection;
  i18n: I18n;
  templates: AuthnTemplates;
  allowPinLogin: boolean;
  allowPinRegistration: boolean;
  autoSelectionIdentity?: bigint;
}): Promise<{
  userNumber: bigint;
  connection: AuthenticatedConnection;
  newAnchor: boolean;
  authnMethod: "pin" | "passkey" | "recovery";
  showAddCurrentDevice: boolean;
}> => {
  const getGoogleClientId = () =>
    connection.canisterConfig.openid_google[0]?.[0]?.client_id;

  const promptAuth = async (autoSelectIdentity?: bigint) =>
    authenticateBoxFlow<PinIdentityMaterial>({
      i18n,
      templates,
      addDevice: (userNumber) => asNewDevice(connection, userNumber),
      loginPasskey: (userNumber) => loginPasskey({ connection, userNumber }),
      loginPinIdentityMaterial: (opts) =>
        loginPinIdentityMaterial({ ...opts, connection }),
      recover: () => useRecovery(connection),
      registerFlowOpts: await getRegisterFlowOpts({
        connection,
        allowPinRegistration,
        getGoogleClientId,
      }),
      verifyPinValidity: ({ userNumber, pinIdentityMaterial }) =>
        pinIdentityAuthenticatorValidity({
          userNumber,
          pinIdentityMaterial,
          connection,
        }),
      retrievePinIdentityMaterial: ({ userNumber }) =>
        idbRetrievePinIdentityMaterial({ userNumber }),
      allowPinLogin: allowPinLogin,
      autoSelectIdentity,
      connection,
    });

  // Retry until user has successfully authenticated
  for (;;) {
    try {
      const result = await promptAuth(autoSelectionIdentity);

      // If the user canceled or just added a device, we retry
      if ("tag" in result) {
        result satisfies { tag: "canceled" | "deviceAdded" };
        continue;
      }

      const loginData = await handleLoginFlowResult(result);

      if (nonNullish(loginData)) {
        return loginData;
      }
    } catch (err) {
      await displayError({
        title: "Authentication Failed",
        message:
          "Something went wrong during authentication. Please try again.",
        detail: unknownToString(err, "unknown error"),
        primaryButton: "Try again",
      });
    }
    // clear out the auto-select so that after the first error / cancel
    // the identity number picker actually waits for input
    autoSelectionIdentity = undefined;
  }
};

// Check that the PIN identity has a corresponding authenticator
const pinIdentityAuthenticatorValidity = async ({
  pinIdentityMaterial,
  connection,
  userNumber,
}: {
  pinIdentityMaterial: PinIdentityMaterial;
  connection: Connection;
  userNumber: bigint;
}) => {
  const authenticators = await connection.lookupAuthenticators(userNumber);
  const pinPubkeyDer = await pinIdentityToDerPubkey(pinIdentityMaterial);
  // Check that the authenticator is still present on the identity.
  const hasAuthenticator = authenticators.some((authenticator) =>
    bufferEqual(
      new Uint8Array(authenticator.pubkey).buffer as DerEncodedPublicKey,
      pinPubkeyDer,
    ),
  );

  return hasAuthenticator ? "valid" : "expired";
};

/** Authentication box component which authenticates a user
 * to II or to another dapp */
export const authenticateBoxFlow = async <I>({
  i18n,
  templates,
  addDevice,
  loginPasskey,
  loginPinIdentityMaterial,
  recover,
  registerFlowOpts,
  verifyPinValidity,
  retrievePinIdentityMaterial,
  allowPinLogin,
  autoSelectIdentity,
  connection,
}: {
  i18n: I18n;
  templates: AuthnTemplates;
  addDevice: (
    userNumber?: bigint,
  ) => Promise<{ tag: "deviceAdded" } | { tag: "canceled" }>;
  loginPasskey: (
    userNumber: bigint,
  ) => Promise<
    | LoginSuccess
    | AuthFail
    | WebAuthnFailed
    | PossiblyWrongWebAuthnFlow
    | PinUserOtherDomain
    | UnknownUser
    | ApiError
  >;
  loginPinIdentityMaterial: ({
    userNumber,
    pin,
    pinIdentityMaterial,
  }: {
    userNumber: bigint;
    pin: string;
    pinIdentityMaterial: I;
  }) => Promise<LoginSuccess | BadPin>;
  recover: () => Promise<LoginSuccess | { tag: "canceled" }>;
  retrievePinIdentityMaterial: ({
    userNumber,
  }: {
    userNumber: bigint;
  }) => Promise<I | undefined>;
  allowPinLogin: boolean;
  autoSelectIdentity?: bigint;
  connection: Connection;
  verifyPinValidity: (opts: {
    userNumber: bigint;
    pinIdentityMaterial: I;
  }) => Promise<"valid" | "expired">;
  registerFlowOpts: RegisterFlowOpts;
}): Promise<
  | (LoginSuccess & {
      newAnchor: boolean;
      authnMethod: "pin" | "passkey" | "recovery";
    })
  | PossiblyWrongWebAuthnFlow
  | PinUserOtherDomain
  | FlowError
  | { tag: "canceled" }
  | { tag: "deviceAdded" }
> => {
  const pages = authnScreens(i18n, { ...templates });

  const getGoogleClientId = () =>
    connection.canisterConfig.openid_google[0]?.[0]?.client_id;

  // The registration flow for a new identity
  const doRegister = async (): Promise<
    | (LoginSuccess & {
        newAnchor: true;
        authnMethod: "pin" | "passkey" | "recovery";
      })
    | FlowError
    | { tag: "canceled" }
  > => {
    const result2 = await registerFlow(registerFlowOpts);

    if (result2 === "canceled") {
      return { tag: "canceled" } as const;
    }

    if (result2.kind !== "loginSuccess") {
      return result2;
    }

    result2 satisfies LoginSuccess;
    return {
      newAnchor: true,
      ...result2,
    };
  };

  const doLogin = ({ userNumber }: { userNumber: bigint }) =>
    useIdentityFlow({
      userNumber,
      retrievePinIdentityMaterial,

      loginPasskey,
      loginPinIdentityMaterial,
      verifyPinValidity,
      allowPinLogin,
    });

  const doLoginWithGoogle = async (
    connection: Connection,
  ): Promise<
    | (LoginSuccess & {
        newAnchor: false;
        authnMethod: "pin" | "passkey" | "recovery";
      })
    | FlowError
  > => {
    const i18n = new I18n();
    const copy = i18n.i18n(infoToastCopy);

    const googleClientId = getGoogleClientId();

    if (isNullish(googleClientId)) {
      toast.error(copy.sign_in_with_google_accounts_is_unavailable);
      return { kind: "missingGoogleClientId" } as FlowError;
    }

    const sessionIdentity = await ECDSAKeyIdentity.generate({
      extractable: false,
    });

    const googleRequestConfig = createGoogleRequestConfig(googleClientId);
    const { nonce, salt } = await createAnonymousNonce(
      sessionIdentity.getPrincipal(),
    );

    const jwt = await withLoader(() =>
      requestJWT(googleRequestConfig, {
        mediation: "required",
        nonce,
      }),
    );

    const authenticatedConnection = await connection.fromJwt(
      jwt,
      salt,
      sessionIdentity,
    );

    return {
      kind: "loginSuccess" as const,
      connection: authenticatedConnection,
      userNumber: authenticatedConnection.userNumber,
      showAddCurrentDevice: false,
      newAnchor: false,
      authnMethod: "passkey" as const, // we are returning passkey here because we don't want dapps to be able to block based on openID login
    };
  };

  // Prompt for an identity number
  const doPrompt = async (): Promise<
    | (LoginSuccess & {
        newAnchor: boolean;
        authnMethod: "pin" | "passkey" | "recovery";
      })
    | PossiblyWrongWebAuthnFlow
    | PinUserOtherDomain
    | FlowError
    | { tag: "canceled" }
    | { tag: "deviceAdded" }
  > => {
    const result = await pages.useExisting();
    if (result.tag === "submit") {
      loginFunnel.trigger(LoginEvents.TriggerUseExisting);
      return doLogin({ userNumber: result.userNumber });
    }

    if (result.tag === "add_device") {
      return await addDevice(result.userNumber);
    }

    if (result.tag === "register") {
      return await doRegister();
    }

    if (result.tag === "open_id_google") {
      const loginResult = await doLoginWithGoogle(connection);
      if (isNullish(loginResult)) return { kind: "googleLoginFailed" };
      return loginResult;
    }

    result satisfies { tag: "recover" };

    const recoverResult = await recover();
    if ("tag" in recoverResult) {
      recoverResult satisfies { tag: "canceled" };
      return { tag: "canceled" } as const;
    }

    recoverResult satisfies LoginSuccess;
    return {
      newAnchor:
        false /* If an anchor was recovered, then it's _not_ a new anchor */,
      authnMethod: "recovery",
      ...recoverResult,
    };
  };

  // If there _are_ some anchors, then we show the "pick" screen, otherwise
  // we assume a new user and show the "firstTime" screen.
  const anchors = await getAnchors();
  if (isNonEmptyArray(anchors)) {
    loginFunnel.trigger(LoginEvents.IdentitiesList);
    const result = await pages.pick({
      anchors,
      autoSelect: autoSelectIdentity,
    });

    if (result.tag === "pick") {
      loginFunnel.trigger(LoginEvents.TriggerListItem);
      return doLogin({ userNumber: result.userNumber });
    }

    result satisfies { tag: "more_options" };
    loginFunnel.trigger(LoginEvents.GoUseExisting);
    return await doPrompt();
  } else {
    loginFunnel.trigger(LoginEvents.NoIdentities);
    const result = await pages.firstTime();

    if (result.tag === "register") {
      return await doRegister();
    }

    result satisfies { tag: "use_existing" };
    return await doPrompt();
  }
};

// A type representing flow errors present in most flows
export type FlowError =
  | AuthFail
  | BadPin
  | { kind: "pinNotAllowed" }
  | WebAuthnFailed
  | UnknownUser
  | ApiError
  | InvalidCaller
  | AlreadyInProgress
  | RateLimitExceeded
  | NoRegistrationFlow
  | UnexpectedCall
  | InvalidAuthnMethod
  | RegisterNoSpace
  | MissingGoogleClientId
  | GoogleLoginFailed;

export const handleLoginFlowResult = async <E>(
  result:
    | (LoginSuccess & E)
    | PossiblyWrongWebAuthnFlow
    | PinUserOtherDomain
    | FlowError,
): Promise<
  ({ userNumber: bigint; connection: AuthenticatedConnection } & E) | undefined
> => {
  if (result.kind === "loginSuccess") {
    await setAnchorUsed(result.userNumber);
    return result;
  }

  if (result.kind === "possiblyWrongWebAuthnFlow") {
    const i18n = new I18n();
    const copy = i18n.i18n(infoToastCopy);
    toast.info(
      infoToastTemplate({
        title: copy.title_possibly_wrong_web_authn_flow,
        messages: [copy.message_possibly_wrong_web_authn_flow_1],
      }),
    );
    return undefined;
  }

  if (result.kind === "pinUserOtherDomain") {
    const i18n = new I18n();
    const copy = i18n.i18n(infoToastCopy);
    toast.info(
      infoToastTemplate({
        title: copy.title_pin_another_domain,
        messages: [
          copy.message_pin_another_domain_1,
          copy.message_pin_another_domain_2,
        ],
      }),
    );
    return undefined;
  }

  result satisfies FlowError;

  toast.error(flowErrorToastTemplate(result));
  return undefined;
};

const learnMoreBlock = html`<p class="l-stack t-centered">
  <a
    href="https://internetcomputer.org/internet-identity"
    target="_blank"
    rel="noopener noreferrer"
    >Learn more</a
  >
  about Internet Identity
</p>`;

/** The templates for the authentication pages */
export const authnTemplates = (i18n: I18n, props: AuthnTemplates) => {
  return {
    firstTime: (firstTimeProps: {
      useExisting: () => void;
      register: () => void;
    }) => {
      return html`${props.firstTime.slot}
        <div class="l-stack">
          <button
            type="button"
            @click=${() => firstTimeProps.register()}
            id="registerButton"
            class="c-button"
          >
            ${props.firstTime.createAnchorText}
          </button>
          <button
            type="button"
            @click=${() => firstTimeProps.useExisting()}
            id="loginButton"
            class="c-button c-button--secondary"
          >
            ${props.firstTime.useExistingText}
          </button>
        </div>
        ${learnMoreBlock}`;
    },
    useExisting: (useExistingProps: {
      register: () => void;
      onSubmit: (userNumber: bigint) => void;
      recover: (userNumber?: bigint) => void;
      addDevice: (userNumber?: bigint) => void;
      loginOpenIDGoogle: () => void;
    }) => {
      const copy = i18n.i18n(authnTemplatesCopy);

      const anchorInput = mkAnchorInput({
        onSubmit: useExistingProps.onSubmit,
      });
      const withUserNumber = (f: (arg: bigint | undefined) => void) => {
        const value = withRef(
          anchorInput.userNumberInput,
          (input) => input.value,
        );

        // XXX: we work around parseUserNumber returning "null" by defaulting to "undefined"
        const userNumber = parseUserNumber(value ?? "") ?? undefined;
        f(userNumber);
      };
      return html` ${props.useExisting.slot} ${anchorInput.template}
        <div class="c-button-group">
          <button
            data-action="continue"
            @click=${() => anchorInput.submit()}
            class="c-button"
          >
            ${copy.continue}
          </button>
        </div>
        <button
          @click=${() =>
            withUserNumber((userNumber) =>
              useExistingProps.addDevice(userNumber),
            )}
          id="addNewDeviceButton"
          class="c-button c-button--secondary"
        >
          ${copy.continue_with_another_device}
        </button>
        ${get(OPENID_AUTHENTICATION)
          ? html`
              <button
                @click=${() =>
                  withUserNumber(() => useExistingProps.loginOpenIDGoogle())}
                id="addNewDeviceButton"
                class="c-button c-button--secondary"
              >
                ${copy.continue_with_google}
              </button>
            `
          : ``}

        <ul class="c-link-group">
          <li>
            <button
              @click=${() => useExistingProps.register()}
              id="registerButton"
              class="t-link"
            >
              ${copy.create_new}
            </button>
          </li>
          <li>
            <a
              @click="${() =>
                withUserNumber((userNumber) =>
                  useExistingProps.recover(userNumber),
                )}"
              id="recoverButton"
              class="t-link"
              >${copy.lost_access}</a
            >
          </li>
        </ul>`;
    },
    pick: (pickProps: {
      anchors: NonEmptyArray<bigint>;
      onSubmit: (userNumber: bigint) => void;
      moreOptions: () => void;
    }) => {
      return html`
        ${props.pick.slot}
        ${mkAnchorPicker({
          savedAnchors: pickProps.anchors,
          pick: pickProps.onSubmit,
          moreOptions: pickProps.moreOptions,
        }).template}
        ${learnMoreBlock}
      `;
    },
  };
};

export const authnPages = (i18n: I18n, props: AuthnTemplates) => {
  const templates = authnTemplates(i18n, props);

  return {
    firstTime: (opts: Parameters<typeof templates.firstTime>[0]) =>
      page({
        slot: templates.firstTime(opts),
        useLandingPageTemplate: true,
        landingType: "firstTime",
      }),
    useExisting: (opts: Parameters<typeof templates.useExisting>[0]) =>
      page({
        slot: templates.useExisting(opts),
        useLandingPageTemplate: false,
        landingType: "useExisting",
      }),
    pick: (opts: Parameters<typeof templates.pick>[0]) =>
      page({
        slot: templates.pick(opts),
        useLandingPageTemplate: true,
        landingType: "pick",
      }),
  };
};

/** The authentication pages, namely "firstTime" (for new users), "useExisting" (for users who
 * don't have saved anchors or who wish to use non-saved anchors) and "pick" (for users
 * picking a saved anchor) */
export const authnScreens = (i18n: I18n, props: AuthnTemplates) => {
  const pages = authnPages(i18n, props);
  return {
    firstTime: () =>
      new Promise<{ tag: "use_existing" } | { tag: "register" }>((resolve) =>
        pages.firstTime({
          useExisting: () => resolve({ tag: "use_existing" }),
          register: () => resolve({ tag: "register" }),
        }),
      ),
    useExisting: () =>
      new Promise<
        | { tag: "register" }
        | { tag: "submit"; userNumber: bigint }
        | { tag: "add_device"; userNumber?: bigint }
        | { tag: "recover"; userNumber?: bigint }
        | { tag: "open_id_google" }
      >((resolve) =>
        pages.useExisting({
          register: () => resolve({ tag: "register" }),
          onSubmit: (userNumber: bigint) =>
            resolve({ tag: "submit", userNumber }),
          addDevice: (userNumber?: bigint) =>
            resolve({ tag: "add_device", userNumber }),
          recover: (userNumber?: bigint) =>
            resolve({ tag: "recover", userNumber }),
          loginOpenIDGoogle: () => resolve({ tag: "open_id_google" }),
        }),
      ),
    pick: (pickProps: {
      anchors: NonEmptyArray<bigint>;
      autoSelect?: bigint;
    }) =>
      new Promise<
        { tag: "more_options" } | { tag: "pick"; userNumber: bigint }
      >((resolve) => {
        // render page first so that when the identity is picked and the passkey
        // dialog pops up, the II page is not just blank.
        pages.pick({
          ...pickProps,
          onSubmit: (userNumber) => resolve({ tag: "pick", userNumber }),
          moreOptions: () => resolve({ tag: "more_options" }),
        });
        // If an existing autoSelect value is supplied immediately
        // resolve with the auto-selected identity number
        if (
          nonNullish(pickProps.autoSelect) &&
          pickProps.anchors.includes(pickProps.autoSelect)
        ) {
          resolve({ tag: "pick", userNumber: pickProps.autoSelect });
        }
      }),
  };
};

// Wrap the template with header & footer and render the page
const page = ({
  slot,
  useLandingPageTemplate,
  landingType,
}: {
  slot: TemplateResult;
  useLandingPageTemplate: boolean;
  landingType: "firstTime" | "useExisting" | "pick";
}) => {
  const template = useLandingPageTemplate
    ? landingPage({
        slot,
        dataPage: "authenticate",
        landingType,
      })
    : mainWindow({
        slot: html`<!-- The title is hidden but used for accessibility -->
          <h1 data-page="authenticate" class="is-hidden">Internet Identity</h1>
          ${slot}`,
      });
  const container = document.getElementById("pageContent") as HTMLElement;
  render(template, container);
};

const loginPasskey = ({
  connection,
  userNumber,
}: {
  connection: Connection;
  userNumber: bigint;
}) => connection.login(userNumber);

const loginPinIdentityMaterial = ({
  connection,
  userNumber,
  pin,
  pinIdentityMaterial,
}: {
  connection: Connection;
  userNumber: bigint;
  pin: string;
  pinIdentityMaterial: PinIdentityMaterial;
}): Promise<LoginSuccess | BadPin> => {
  return withLoader(async () => {
    try {
      const identity = await reconstructPinIdentity({
        pin,
        pinIdentityMaterial,
      });

      return connection.fromIdentity(() => userNumber, identity);
    } catch {
      // We handle all exceptions as wrong PIN because there is no nice way to check for that particular failure.
      // The best we could do is check that the error is a DOMException and that the name is "OperationError". However,
      // the "OperationError" names is still marked as experimental, so we should not rely on that.
      // See https://developer.mozilla.org/en-US/docs/Web/API/DOMException
      return { kind: "badPin" };
    }
  });
};

// Register this device as a new device with the anchor
const asNewDevice = async (
  connection: Connection,
  prefilledUserNumber?: bigint,
): Promise<{ tag: "deviceAdded" } | { tag: "canceled" }> => {
  // Prompt the user for an anchor and provide additional information about the flow.
  // If the user number is already known, it is prefilled in the screen.
  const userNumberResult = await promptUserNumber({
    title: "Continue with another device",
    message:
      "Is this your first time connecting to Internet Identity on this device? In the next steps, you will add this device as an Internet Identity passkey. Do you wish to continue?",
    userNumber: prefilledUserNumber,
  });
  if (userNumberResult === "canceled") {
    return { tag: "canceled" };
  }
  return await registerTentativeDevice(userNumberResult, connection);
};

// Helper to convert PIN identity material to a Der public-key
const pinIdentityToDerPubkey = async (
  pinIdentity: PinIdentityMaterial,
): Promise<DerEncodedPublicKey> => {
  return (await crypto.subtle.exportKey(
    "spki",
    pinIdentity.publicKey,
  )) as DerEncodedPublicKey;
};

// Find and use a passkey, whether PIN or webauthn
const useIdentityFlow = async <I>({
  userNumber,
  allowPinLogin,
  retrievePinIdentityMaterial,
  verifyPinValidity,
  loginPasskey,
  loginPinIdentityMaterial,
}: {
  userNumber: bigint;
  retrievePinIdentityMaterial: ({
    userNumber,
  }: {
    userNumber: bigint;
  }) => Promise<I | undefined>;
  loginPasskey: (
    userNumber: bigint,
  ) => Promise<
    | LoginSuccess
    | AuthFail
    | WebAuthnFailed
    | PossiblyWrongWebAuthnFlow
    | PinUserOtherDomain
    | UnknownUser
    | ApiError
  >;
  allowPinLogin: boolean;
  verifyPinValidity: (opts: {
    userNumber: bigint;
    pinIdentityMaterial: I;
  }) => Promise<"valid" | "expired">;
  loginPinIdentityMaterial: ({
    userNumber,
    pin,
    pinIdentityMaterial,
  }: {
    userNumber: bigint;
    pin: string;
    pinIdentityMaterial: I;
  }) => Promise<LoginSuccess | BadPin>;
}): Promise<
  | (LoginSuccess & {
      newAnchor: boolean;
      authnMethod: "pin" | "passkey" | "recovery";
    })
  | AuthFail
  | WebAuthnFailed
  | PossiblyWrongWebAuthnFlow
  | PinUserOtherDomain
  | UnknownUser
  | ApiError
  | BadPin
  | { kind: "pinNotAllowed" }
  | { tag: "canceled" }
> => {
  const pinIdentityMaterial: I | undefined = await withLoader(() =>
    retrievePinIdentityMaterial({
      userNumber,
    }),
  );

  const doLoginPasskey = async () => {
    const result = await withLoader(() => loginPasskey(userNumber));
    // We need to trigger the success here because later we don't know whether it was a registration or login.
    if (result.kind === "loginSuccess") {
      loginFunnel.trigger(LoginEvents.Success);
    }
    return { newAnchor: false, authnMethod: "passkey", ...result } as const;
  };

  if (isNullish(pinIdentityMaterial)) {
    // this user number does not have a browser storage identity
    return doLoginPasskey();
  }

  // Here we ensure the PIN identity is still valid, i.e. the user did not explicitly delete
  // that "passkey" (DeviceData).
  // XXX: we don't actually delete the identity material, because the current implementation
  // cannot certify the response from the node and a malicious node might pretend the PIN has
  // been removed.
  const isValid = await withLoader(() =>
    verifyPinValidity({
      pinIdentityMaterial,
      userNumber,
    }),
  );
  if (isValid === "expired") {
    // the PIN identity seems to have been expired
    return doLoginPasskey();
  }
  isValid satisfies "valid";

  // if there is a PIN but allowPinLogin is false, then error out
  if (!allowPinLogin) {
    return { kind: "pinNotAllowed" };
  }

  // Otherwise, attempt login with PIN
  const result = await usePin<LoginSuccess | BadPin>({
    verifyPin: async (pin) => {
      const result = await loginPinIdentityMaterial({
        userNumber,
        pin,
        pinIdentityMaterial,
      });

      if (result.kind !== "loginSuccess") {
        return { ok: false, error: "Invalid PIN" };
      }

      result satisfies LoginSuccess;
      return { ok: true, value: result };
    },
  });

  if (result.kind === "canceled") {
    return { tag: "canceled" } as const;
  }

  if (result.kind === "passkey") {
    // User still decided to use a passkey
    return doLoginPasskey();
  }

  result satisfies { kind: "pin" };
  const { result: pinResult } = result;

  if (pinResult.kind !== "loginSuccess") {
    pinResult satisfies BadPin;
    return { ...pinResult };
  }

  pinResult satisfies LoginSuccess;

  // We log in with an existing PIN anchor, meaning it is _not_ a new anchor
  return { newAnchor: false, authnMethod: "pin", ...pinResult };
};

// Use a passkey, with concrete impl.
export const useIdentity = ({
  userNumber,
  connection,
  allowPinLogin,
}: {
  userNumber: bigint;
  connection: Connection;
  allowPinLogin: boolean;
}) =>
  useIdentityFlow({
    userNumber,
    retrievePinIdentityMaterial: idbRetrievePinIdentityMaterial,
    allowPinLogin,

    verifyPinValidity: (opts) =>
      pinIdentityAuthenticatorValidity({ ...opts, connection }),

    loginPasskey: (userNumber) => loginPasskey({ connection, userNumber }),
    loginPinIdentityMaterial: (opts) =>
      loginPinIdentityMaterial({ ...opts, connection }),
  });
