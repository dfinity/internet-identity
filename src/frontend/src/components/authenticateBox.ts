import { registerTentativeDevice } from "$src/flows/addDevice/welcomeView/registerTentativeDevice";
import { useRecovery } from "$src/flows/recovery/useRecovery";
import { getRegisterFlowOpts, registerFlow } from "$src/flows/register";
import { I18n } from "$src/i18n";
import {
  LoginData,
  LoginFlowError,
  LoginFlowResult,
  LoginFlowSuccess,
  apiResultToLoginFlowResult,
} from "$src/utils/flowResult";
import { Connection, LoginResult } from "$src/utils/iiConnection";
import { TemplateElement, withRef } from "$src/utils/lit-html";
import {
  getAnchors,
  parseUserNumber,
  setAnchorUsed,
} from "$src/utils/userNumber";
import { NonEmptyArray, isNonEmptyArray, unreachable } from "$src/utils/utils";
import { nonNullish } from "@dfinity/utils";
import { TemplateResult, html, render } from "lit-html";
import { mkAnchorInput } from "./anchorInput";
import { mkAnchorPicker } from "./anchorPicker";
import { displayError } from "./displayError";
import {
  controlIcon,
  githubBigIcon,
  participateIcon,
  privacyIcon,
  secureIcon,
  signInIcon,
} from "./icons";
import { withLoader } from "./loader";
import { mainWindow } from "./mainWindow";
import { promptUserNumber } from "./promptUserNumber";

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
}: {
  connection: Connection;
  i18n: I18n;
  templates: AuthnTemplates;
}): Promise<LoginData & { newAnchor: boolean }> => {
  const promptAuth = () =>
    authenticateBoxFlow({
      i18n,
      templates,
      addDevice: (userNumber) => asNewDevice(connection, userNumber),
      login: (userNumber) => login({ connection, userNumber }),
      recover: () => useRecovery(connection),
      registerFlowOpts: getRegisterFlowOpts({ connection }),
    });

  // Retry until user has successfully authenticated
  for (;;) {
    const { newAnchor, ...result } = await promptAuth();
    const loginData = await handleLoginFlowResult(result);

    if (nonNullish(loginData)) {
      return { ...loginData, newAnchor };
    }
  }
};

/** Authentication box component which authenticates a user
 * to II or to another dapp */
export const authenticateBoxFlow = async <T>({
  i18n,
  templates,
  addDevice,
  login,
  recover,
  registerFlowOpts,
}: {
  i18n: I18n;
  templates: AuthnTemplates;
  addDevice: (userNumber?: bigint) => Promise<{ alias: string }>;
  login: (userNumber: bigint) => Promise<LoginFlowSuccess<T> | LoginFlowError>;
  recover: () => Promise<LoginFlowResult<T>>;
  registerFlowOpts: Parameters<typeof registerFlow<T>>[0];
}): Promise<LoginFlowResult<T> & { newAnchor: boolean }> => {
  const pages = authnScreens(i18n, { ...templates });

  // The registration flow for a new identity
  const doRegister = async (): Promise<
    LoginFlowResult<T> & { newAnchor: boolean }
  > => {
    const result2 = await registerFlow<T>(registerFlowOpts);

    if (result2 === "canceled") {
      return {
        newAnchor: true,
        tag: "canceled",
      } as const;
    }

    return {
      newAnchor: true,
      ...apiResultToLoginFlowResult<T>(result2),
    };
  };

  // Prompt for an identity number
  const doPrompt = async (): Promise<
    LoginFlowResult<T> & { newAnchor: boolean }
  > => {
    const result = await pages.useExisting();
    if (result.tag === "submit") {
      const result2 = await withLoader(() => login(result.userNumber));
      return { newAnchor: false, ...result2 };
    }

    if (result.tag === "add_device") {
      const _ = await addDevice(result.userNumber);
      // XXX: we don't currently do anything with the result from adding a device and
      // we let the flow hang.
      const hang = await new Promise<never>((_) => {
        /* hang forever */
      });
      return hang;
    }

    if (result.tag === "register") {
      return await doRegister();
    }

    result satisfies { tag: "recover" };
    const result2 = await recover();
    return {
      newAnchor: false,
      ...result2,
    };
  };

  // If there _are_ some anchors, then we show the "pick" screen, otherwise
  // we assume a new user and show the "firstTime" screen.
  const anchors = getAnchors();
  if (isNonEmptyArray(anchors)) {
    const result = await pages.pick({ anchors });

    if (result.tag === "pick") {
      const result1 = await withLoader(() => login(result.userNumber));
      return { newAnchor: false, ...result1 };
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

export const handleLoginFlowResult = async <T>(
  result: LoginFlowResult<T>
): Promise<LoginData<T> | undefined> => {
  switch (result.tag) {
    case "ok":
      setAnchorUsed(result.userNumber);
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

export const handleLogin = async <T>({
  login,
}: {
  login: () => Promise<LoginResult<T>>;
}): Promise<LoginFlowSuccess<T> | LoginFlowError> => {
  try {
    const result = await login();
    return apiResultToLoginFlowResult<T>(result);
  } catch (error) {
    return {
      tag: "err",
      title: "Authentication Failed",
      message: "An error occurred during authentication.",
      detail: error instanceof Error ? error.message : JSON.stringify(error),
    };
  }
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

const login = ({
  connection,
  userNumber,
}: {
  connection: Connection;
  userNumber: bigint;
}) =>
  handleLogin({
    login: () => connection.login(userNumber),
  });

// Register this device as a new device with the anchor
const asNewDevice = async (
  connection: Connection,
  prefilledUserNumber?: bigint
) => {
  // Prompt the user for an anchor and provide additional information about the flow.
  // If the user number is already known, it is prefilled in the screen.
  const promptUserNumberWithInfo = async (prefilledUserNumber?: bigint) => {
    const result = await promptUserNumber({
      title: "Continue with another device",
      message:
        "Is this your first time connecting to Internet Identity on this device? In the next steps, you will add this device as an Internet Identity passkey. Do you wish to continue?",
      userNumber: prefilledUserNumber,
    });
    if (result === "canceled") {
      // TODO L2-309: do this without reload
      return window.location.reload() as never;
    }

    return result;
  };

  return registerTentativeDevice(
    await promptUserNumberWithInfo(prefilledUserNumber),
    connection
  );
};
