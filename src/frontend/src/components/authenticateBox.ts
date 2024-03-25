import { ErrorOptions, displayError } from "$src/components/displayError";
import { withLoader } from "$src/components/loader";
import {
  PinIdentityMaterial,
  reconstructPinIdentity,
} from "$src/crypto/pinIdentity";
import { registerTentativeDevice } from "$src/flows/addDevice/welcomeView/registerTentativeDevice";
import { idbRetrievePinIdentityMaterial } from "$src/flows/pin/idb";
import { usePin } from "$src/flows/pin/usePin";
import { useRecovery } from "$src/flows/recovery/useRecovery";
import { getRegisterFlowOpts, registerFlow } from "$src/flows/register";
import { I18n } from "$src/i18n";
import { getAnchors, setAnchorUsed } from "$src/storage";
import {
  ApiError,
  AuthFail,
  AuthenticatedConnection,
  BadChallenge,
  BadPin,
  Connection,
  LoginSuccess,
  RegisterNoSpace,
  UnknownUser,
  WebAuthnFailed,
  bufferEqual,
} from "$src/utils/iiConnection";
import { TemplateElement, withRef } from "$src/utils/lit-html";
import { parseUserNumber } from "$src/utils/userNumber";
import {
  NonEmptyArray,
  isNonEmptyArray,
  unknownToString,
} from "$src/utils/utils";
import { isNullish, nonNullish } from "@dfinity/utils";
import { TemplateResult, html, render } from "lit-html";
import { mkAnchorInput } from "./anchorInput";
import { mkAnchorPicker } from "./anchorPicker";
import {
  controlIcon,
  githubBigIcon,
  participateIcon,
  privacyIcon,
  secureIcon,
  signInIcon,
} from "./icons";
import { mainWindow } from "./mainWindow";
import { promptUserNumber } from "./promptUserNumber";

import { DerEncodedPublicKey } from "@dfinity/agent";
import copyJson from "./authenticateBox.json";

/** Template used for rendering specific authentication screens. See `authnScreens` below
 * for meaning of "firstTime", "useExisting" and "pick". */
export type AuthnTemplates = {
  firstTime: {
    slot: TemplateResult;
    useExistingText: TemplateElement /** text shown on the button leading to "useExisting" */;
    createAnchorText: TemplateElement /** text shown on the button leading to "useExisting" */;
  };
  useExisting: {
    slot: TemplateResult;
  };
  pick: {
    slot: TemplateResult;
  };
};

export const authenticateBox = async ({
  connection,
  i18n,
  templates,
  allowPinAuthentication,
}: {
  connection: Connection;
  i18n: I18n;
  templates: AuthnTemplates;
  allowPinAuthentication: boolean;
}): Promise<{
  userNumber: bigint;
  connection: AuthenticatedConnection;
  newAnchor: boolean;
  authnMethod: "pin" | "passkey" | "recovery";
}> => {
  const promptAuth = () =>
    authenticateBoxFlow<AuthenticatedConnection, PinIdentityMaterial>({
      i18n,
      templates,
      addDevice: (userNumber) => asNewDevice(connection, userNumber),
      loginPasskey: (userNumber) => loginPasskey({ connection, userNumber }),
      loginPinIdentityMaterial: (opts) =>
        loginPinIdentityMaterial({ ...opts, connection }),
      recover: () => useRecovery(connection),
      registerFlowOpts: getRegisterFlowOpts({
        connection,
        allowPinAuthentication,
      }),
      verifyPinValidity: ({ userNumber, pinIdentityMaterial }) =>
        pinIdentityAuthenticatorValidity({
          userNumber,
          pinIdentityMaterial,
          connection,
        }),
      retrievePinIdentityMaterial: ({ userNumber }) =>
        idbRetrievePinIdentityMaterial({ userNumber }),
      allowPinAuthentication,
    });

  // Retry until user has successfully authenticated
  for (;;) {
    try {
      const result = await promptAuth();

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
      pinPubkeyDer
    )
  );

  return hasAuthenticator ? "valid" : "expired";
};

/** Authentication box component which authenticates a user
 * to II or to another dapp */
export const authenticateBoxFlow = async <T, I>({
  i18n,
  templates,
  addDevice,
  loginPasskey,
  loginPinIdentityMaterial,
  recover,
  registerFlowOpts,
  verifyPinValidity,
  retrievePinIdentityMaterial,
  allowPinAuthentication,
}: {
  i18n: I18n;
  templates: AuthnTemplates;
  addDevice: (
    userNumber?: bigint
  ) => Promise<{ tag: "deviceAdded" } | { tag: "canceled" }>;
  loginPasskey: (
    userNumber: bigint
  ) => Promise<
    LoginSuccess<T> | AuthFail | WebAuthnFailed | UnknownUser | ApiError
  >;
  loginPinIdentityMaterial: ({
    userNumber,
    pin,
    pinIdentityMaterial,
  }: {
    userNumber: bigint;
    pin: string;
    pinIdentityMaterial: I;
  }) => Promise<LoginSuccess<T> | BadPin>;
  recover: () => Promise<LoginSuccess<T> | { tag: "canceled" }>;
  retrievePinIdentityMaterial: ({
    userNumber,
  }: {
    userNumber: bigint;
  }) => Promise<I | undefined>;
  allowPinAuthentication: boolean;
  verifyPinValidity: (opts: {
    userNumber: bigint;
    pinIdentityMaterial: I;
  }) => Promise<"valid" | "expired">;
  registerFlowOpts: Parameters<typeof registerFlow<T>>[0];
}): Promise<
  | (LoginSuccess<T> & {
      newAnchor: boolean;
      authnMethod: "pin" | "passkey" | "recovery";
    })
  | FlowError
  | { tag: "canceled" }
  | { tag: "deviceAdded" }
> => {
  const pages = authnScreens(i18n, { ...templates });

  // The registration flow for a new identity
  const doRegister = async (): Promise<
    | (LoginSuccess<T> & {
        newAnchor: true;
        authnMethod: "pin" | "passkey" | "recovery";
      })
    | BadChallenge
    | ApiError
    | AuthFail
    | RegisterNoSpace
    | { tag: "canceled" }
  > => {
    const result2 = await registerFlow<T>(registerFlowOpts);

    if (result2 === "canceled") {
      return { tag: "canceled" } as const;
    }

    if (result2.kind !== "loginSuccess") {
      return result2;
    }

    result2 satisfies LoginSuccess<T>;
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
      allowPinAuthentication,
    });

  // Prompt for an identity number
  const doPrompt = async (): Promise<
    | (LoginSuccess<T> & {
        newAnchor: boolean;
        authnMethod: "pin" | "passkey" | "recovery";
      })
    | FlowError
    | { tag: "canceled" }
    | { tag: "deviceAdded" }
  > => {
    const result = await pages.useExisting();
    if (result.tag === "submit") {
      return doLogin({ userNumber: result.userNumber });
    }

    if (result.tag === "add_device") {
      return await addDevice(result.userNumber);
    }

    if (result.tag === "register") {
      return await doRegister();
    }

    result satisfies { tag: "recover" };

    const recoverResult = await recover();
    if ("tag" in recoverResult) {
      recoverResult satisfies { tag: "canceled" };
      return { tag: "canceled" } as const;
    }

    recoverResult satisfies LoginSuccess<T>;
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
    const result = await pages.pick({ anchors });

    if (result.tag === "pick") {
      return doLogin({ userNumber: result.userNumber });
    }

    result satisfies { tag: "more_options" };
    return await doPrompt();
  } else {
    const result = await pages.firstTime();

    if (result.tag === "register") {
      return await doRegister();
    }

    result satisfies { tag: "use_existing" };
    return await doPrompt();
  }
};

// A type representing flow errors present in most flows
type FlowError =
  | AuthFail
  | BadPin
  | { kind: "pinNotAllowed" }
  | BadChallenge
  | WebAuthnFailed
  | UnknownUser
  | ApiError
  | RegisterNoSpace;

// Maps all errors kinds to their error types (without kind field):
//  KindToError<'authFail'> = { ...fields of AuthFail with kind...};
// The 'Omit' seems to be a necessary step while looking up the error, otherwise typescript
// thinks the types conflict
type KindToError<K extends FlowError["kind"]> = Omit<
  FlowError & { kind: K },
  "kind"
>;

// Makes the error human readable
const clarifyError: {
  [K in FlowError["kind"]]: (err: KindToError<K>) => {
    title: string;
    message: string;
    detail?: string;
  };
} = {
  authFail: (err) => ({
    title: "Failed to authenticate",
    message:
      "We failed to authenticate you using your security device. If this is the first time you're trying to log in with this device, you have to add it as a new device first.",
    detail: err.error.message,
  }),
  webAuthnFailed: () => ({
    title: "Operation canceled",
    message:
      "The interaction with your security device was canceled or timed out. Please try again.",
  }),
  unknownUser: (err) => ({
    title: "Unknown Internet Identity",
    message: `Failed to find Internet Identity ${err.userNumber}. Please check your Internet Identity and try again.`,
  }),
  apiError: (err) => ({
    title: "We couldn't reach Internet Identity",
    message:
      "We failed to call the Internet Identity service, please try again.",
    detail: err.error.message,
  }),
  badPin: () => ({ title: "Could not authenticate", message: "Invalid PIN" }),
  badChallenge: () => ({
    title: "Failed to register",
    message:
      "Failed to register with Internet Identity, because the CAPTCHA challenge wasn't successful",
  }),
  registerNoSpace: () => ({
    title: "Failed to register",
    message:
      "Failed to register with Internet Identity, because there is no space left at the moment. We're working on increasing the capacity.",
  }),
  pinNotAllowed: () => ({
    title: "PIN method not allowed",
    message:
      "The Dapp you are authenticating to does not allow PIN identities and you only have a PIN identity. Please retry using a Passkey: open a new Internet Identity page, add a passkey and retry.",
  }),
};

const clarifyError_ = <K extends FlowError["kind"]>(
  flowError: KindToError<K> & { kind: K }
): Omit<ErrorOptions, "primaryButton"> =>
  clarifyError[flowError.kind](flowError);

export const handleLoginFlowResult = async <T, E>(
  result: (LoginSuccess<T> & E) | FlowError
): Promise<({ userNumber: bigint; connection: T } & E) | undefined> => {
  if (result.kind === "loginSuccess") {
    await setAnchorUsed(result.userNumber);
    return result;
  }

  result satisfies FlowError;

  await displayError({
    ...clarifyError_(result),
    errorCode: result.kind,
    primaryButton: "Try again",
  });
  return undefined;
};

/** The templates for the authentication pages */
export const authnTemplates = (i18n: I18n, props: AuthnTemplates) => {
  const copy = i18n.i18n(copyJson);

  const marketingBlocks = [
    {
      icon: secureIcon,
      title: copy.secure_and_convenient,
      body: copy.instead_of_passwords,
    },
    {
      icon: privacyIcon,
      title: copy.no_tracking,
      body: copy.get_privacy,
    },
    {
      icon: controlIcon,
      title: copy.control_your_identity,
      body: copy.securely_access,
    },
    {
      icon: participateIcon,
      title: copy.own_and_participate,
      body: copy.share_and_vote,
    },
    {
      icon: signInIcon,
      title: copy.sign_in_to_web3,
      body: copy.manages_keys,
    },
    {
      icon: githubBigIcon,
      title: copy.opensource_and_transparent,
      body: copy.internet_identity_codebase,
    },
  ];

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
        <div class="c-marketing-section">
          ${marketingBlocks.map(
            ({ title, body, icon }) =>
              html`
                <article class="c-marketing-block">
                  ${icon !== undefined
                    ? html`<i class="c-icon c-icon--marketing">${icon}</i>`
                    : undefined}
                  <h2 class="t-title t-title--main">${title}</h2>
                  <p class="t-paragraph t-paragraph--weak">${body}</p>
                </article>
              `
          )}
        </div> `;
    },
    useExisting: (useExistingProps: {
      register: () => void;
      onSubmit: (userNumber: bigint) => void;
      recover: (userNumber?: bigint) => void;
      addDevice: (userNumber?: bigint) => void;
    }) => {
      const anchorInput = mkAnchorInput({
        onSubmit: useExistingProps.onSubmit,
      });
      const withUserNumber = (f: (arg: bigint | undefined) => void) => {
        const value = withRef(
          anchorInput.userNumberInput,
          (input) => input.value
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
            Continue
          </button>
        </div>
        <button
          @click=${() =>
            withUserNumber((userNumber) =>
              useExistingProps.addDevice(userNumber)
            )}
          id="addNewDeviceButton"
          class="c-button c-button--textOnly"
        >
          Continue with another device
        </button>

        <ul class="c-link-group">
          <li>
            <button @click=${() => useExistingProps.register()} class="t-link">
              Create New
            </button>
          </li>
          <li>
            <a
              @click="${() =>
                withUserNumber((userNumber) =>
                  useExistingProps.recover(userNumber)
                )}"
              id="recoverButton"
              class="t-link"
              >Lost Access?</a
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
      `;
    },
  };
};

export const authnPages = (i18n: I18n, props: AuthnTemplates) => {
  const templates = authnTemplates(i18n, props);

  return {
    firstTime: (opts: Parameters<typeof templates.firstTime>[0]) =>
      page(templates.firstTime(opts)),
    useExisting: (opts: Parameters<typeof templates.useExisting>[0]) =>
      page(templates.useExisting(opts)),
    pick: (opts: Parameters<typeof templates.pick>[0]) =>
      page(templates.pick(opts)),
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
        })
      ),
    useExisting: () =>
      new Promise<
        | { tag: "register" }
        | { tag: "submit"; userNumber: bigint }
        | { tag: "add_device"; userNumber?: bigint }
        | { tag: "recover"; userNumber?: bigint }
      >((resolve) =>
        pages.useExisting({
          register: () => resolve({ tag: "register" }),
          onSubmit: (userNumber: bigint) =>
            resolve({ tag: "submit", userNumber }),
          addDevice: (userNumber?: bigint) =>
            resolve({ tag: "add_device", userNumber }),
          recover: (userNumber?: bigint) =>
            resolve({ tag: "recover", userNumber }),
        })
      ),
    pick: (pickProps: { anchors: NonEmptyArray<bigint> }) =>
      new Promise<
        { tag: "more_options" } | { tag: "pick"; userNumber: bigint }
      >((resolve) =>
        pages.pick({
          ...pickProps,
          onSubmit: (userNumber) => resolve({ tag: "pick", userNumber }),
          moreOptions: () => resolve({ tag: "more_options" }),
        })
      ),
  };
};

// Wrap the template with header & footer and render the page
const page = (slot: TemplateResult) => {
  const template = mainWindow({
    slot: html` <!-- The title is hidden but used for accessibility -->
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

      return connection.fromIdentity(userNumber, identity);
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
  prefilledUserNumber?: bigint
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
  pinIdentity: PinIdentityMaterial
): Promise<DerEncodedPublicKey> => {
  return (await crypto.subtle.exportKey(
    "spki",
    pinIdentity.publicKey
  )) as DerEncodedPublicKey;
};

// Find and use a passkey, whether PIN or webauthn
const useIdentityFlow = async <T, I>({
  userNumber,
  allowPinAuthentication,
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
    userNumber: bigint
  ) => Promise<
    LoginSuccess<T> | AuthFail | WebAuthnFailed | UnknownUser | ApiError
  >;
  allowPinAuthentication: boolean;
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
  }) => Promise<LoginSuccess<T> | BadPin>;
}): Promise<
  | (LoginSuccess<T> & {
      newAnchor: boolean;
      authnMethod: "pin" | "passkey" | "recovery";
    })
  | AuthFail
  | WebAuthnFailed
  | UnknownUser
  | ApiError
  | BadPin
  | { kind: "pinNotAllowed" }
  | { tag: "canceled" }
> => {
  const pinIdentityMaterial: I | undefined = await withLoader(() =>
    retrievePinIdentityMaterial({
      userNumber,
    })
  );

  const doLoginPasskey = async () => {
    const result = await withLoader(() => loginPasskey(userNumber));
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
    })
  );
  if (isValid === "expired") {
    // the PIN identity seems to have been expired
    return doLoginPasskey();
  }
  isValid satisfies "valid";

  // if there is a PIN but allowPinAuth is false, then error out
  if (!allowPinAuthentication) {
    return { kind: "pinNotAllowed" };
  }

  // Otherwise, attempt login with PIN
  const result = await usePin<LoginSuccess<T> | BadPin>({
    verifyPin: async (pin) => {
      const result = await loginPinIdentityMaterial({
        userNumber,
        pin,
        pinIdentityMaterial,
      });

      if (result.kind !== "loginSuccess") {
        return { ok: false, error: "Invalid PIN" };
      }

      result satisfies LoginSuccess<T>;
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

  pinResult satisfies LoginSuccess<T>;

  // We log in with an existing PIN anchor, meaning it is _not_ a new anchor
  return { newAnchor: false, authnMethod: "pin", ...pinResult };
};

// Use a passkey, with concrete impl.
export const useIdentity = ({
  userNumber,
  connection,
  allowPinAuthentication,
}: {
  userNumber: bigint;
  connection: Connection;
  allowPinAuthentication: boolean;
}) =>
  useIdentityFlow({
    userNumber,
    retrievePinIdentityMaterial: idbRetrievePinIdentityMaterial,
    allowPinAuthentication,

    verifyPinValidity: (opts) =>
      pinIdentityAuthenticatorValidity({ ...opts, connection }),

    loginPasskey: (userNumber) => loginPasskey({ connection, userNumber }),
    loginPinIdentityMaterial: (opts) =>
      loginPinIdentityMaterial({ ...opts, connection }),
  });
