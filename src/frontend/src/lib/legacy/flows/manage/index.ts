import type {
  DeviceData,
  DeviceWithUsage,
  IdentityAnchorInfo,
  OpenIdCredential,
  OpenIdCredentialAddError,
  OpenIdCredentialRemoveError,
} from "$lib/generated/internet_identity_types";
import identityCardBackground from "$lib/legacy/assets/identityCardBackground.png?url";
import {
  AuthnTemplates,
  authenticateBox,
} from "$lib/templates/authenticateBox";
import { displayError } from "$lib/templates/displayError";
import { identityCard } from "$lib/templates/identityCard";
import { withLoader } from "$lib/templates/loader";
import { logoutSection } from "$lib/templates/logout";
import { mainWindow } from "$lib/templates/mainWindow";
import { toast } from "$lib/templates/toast";
import { ENABLE_PIN_QUERY_PARAM_KEY, LEGACY_II_URL } from "$lib/config";
import {
  DOMAIN_COMPATIBILITY,
  OPENID_AUTHENTICATION,
} from "$lib/state/featureFlags";
import { get } from "svelte/store";
import { addDevice } from "$lib/legacy/flows/addDevice/manage/addDevice";
import { dappsExplorer } from "$lib/legacy/flows/dappsExplorer";
import { KnownDapp, getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
import {
  dappsHeader,
  dappsTeaser,
} from "$lib/legacy/flows/dappsExplorer/teaser";
import { confirmUnlinkAccount } from "$lib/legacy/flows/manage/confirmUnlinkAccount";
import { linkedAccountsSection } from "$lib/legacy/flows/manage/linkedAccountsSection";
import copyJson from "$lib/legacy/flows/manage/linkedAccountsSection.json";
import {
  TempKeyWarningAction,
  tempKeyWarningBox,
  tempKeysSection,
} from "$lib/legacy/flows/manage/tempKeys";
import {
  addPhrase,
  recoveryWizard,
} from "$lib/legacy/flows/recovery/recoveryWizard";
import {
  setupKey,
  setupPhrase,
} from "$lib/legacy/flows/recovery/setupRecovery";
import { I18n } from "$lib/legacy/i18n";
import { getCredentialsOrigin } from "$lib/utils/credential-devices";
import {
  AuthenticatedConnection,
  Connection,
  bufferEqual,
} from "$lib/utils/iiConnection";
import { TemplateElement, renderPage } from "$lib/utils/lit-html";
import {
  createAnonymousNonce,
  createGoogleRequestConfig,
  decodeJWT,
  isOpenIdCancelError,
  requestJWT,
} from "$lib/utils/openID";
import { PreLoadImage } from "$lib/utils/preLoadImage";
import {
  isProtected,
  isRecoveryDevice,
  isRecoveryPhrase,
} from "$lib/utils/recoveryDevice";
import { userSupportsWebauthRoR } from "$lib/utils/rorSupport";
import {
  OmitParams,
  isCanisterError,
  shuffleArray,
  unreachable,
} from "$lib/utils/utils";
import { DerEncodedPublicKey } from "@dfinity/agent";
import { Principal } from "@dfinity/principal";
import { isNullish, nonNullish } from "@dfinity/utils";
import { TemplateResult, html } from "lit-html";
import { registerCurrentDeviceCurrentOrigin } from "../addDevice/registerCurrentDeviceCurrentOrigin";
import { authenticatorsSection } from "./authenticatorsSection";
import { confirmRemoveDevice } from "./confirmRemoveDevice";
import {
  protectDevice,
  renameDevice,
  resetPhrase,
  unprotectDevice,
} from "./deviceSettings";
import { recoveryMethodsSection } from "./recoveryMethodsSection";
import {
  Authenticator,
  Devices,
  Protection,
  RecoveryKey,
  RecoveryPhrase,
} from "./types";

/* Template for the authbox when authenticating to II */
export const authnTemplateManage = ({
  dapps,
}: {
  dapps: KnownDapp[];
}): AuthnTemplates => {
  const title = (title: string) =>
    html`<h1 class="t-title t-title--main">${title}</h1>`;

  const subtitle = (subtitle: string) =>
    html`<p class="t-lead l-stack">${subtitle}</p>`;

  return {
    firstTime: {
      slot: html`
        ${dappsHeader({ dapps, clickable: false })}
        <header class="l-stack">
          ${title("Securely connect to dapps on the Internet Computer")}
        </header>
      `,
      useExistingText: "Use existing",
      createAnchorText: "Create Internet Identity",
      landingType: "firstTime",
    },
    useExisting: {
      slot: html`
        <header class="l-stack">
          ${title("Enter Identity 🔑")} ${subtitle("to continue")}
        </header>
      `,
      landingType: "useExisting",
    },

    pick: {
      slot: html`
        <header>
          ${title("Choose Identity 🔑")} ${subtitle("you want to manage")}
        </header>
      `,
      landingType: "pick",
    },
  };
};

/* the II authentication flow */
export const authFlowManage = async (connection: Connection) => {
  const i18n = new I18n();
  const dapps = shuffleArray(getDapps());

  const params = new URLSearchParams(window.location.search);
  const allowPinRegistration = params.get(ENABLE_PIN_QUERY_PARAM_KEY) !== null;

  const identityBackground = new PreLoadImage(identityCardBackground);
  // Go through the login flow, potentially creating an anchor.
  const {
    userNumber,
    connection: authenticatedConnection,
    newAnchor,
    showAddCurrentDevice,
  } = await authenticateBox({
    connection,
    i18n,
    templates: authnTemplateManage({ dapps }),
    allowPinLogin: true,
    allowPinRegistration,
  });

  if (showAddCurrentDevice && get(DOMAIN_COMPATIBILITY)) {
    await registerCurrentDeviceCurrentOrigin(
      userNumber,
      authenticatedConnection,
    );
  }

  // Here, if the user is returning & doesn't have any recovery device, we prompt them to add
  // one. The exact flow depends on the device they use.
  if (!newAnchor) {
    await recoveryWizard(userNumber, authenticatedConnection);
  }
  // From here on, the user is authenticated to II.
  return renderManage({
    userNumber,
    connection: authenticatedConnection,
    identityBackground,
  });
};

const displayFailedToListDevices = (error: Error) =>
  displayError({
    title: "Failed to list your devices",
    message:
      "An unexpected error occurred when displaying your devices. Please try again",
    detail: error.toString(),
    primaryButton: "Try again",
  });

// Actual page content. We display the Identity Anchor and the list of
// (non-recovery) devices. Additionally, if the user does _not_ have any
// recovery devices, we display a warning "nag box" and suggest to the user
// that they add a recovery device. If the user _does_ have at least one
// recovery device, then we do not display a "nag box", but we list the
// recovery devices.
const displayManageTemplate = ({
  userNumber,
  devices: { authenticators, recoveries, pinAuthenticators },
  onAddDevice,
  onRemoveDevice,
  addRecoveryPhrase,
  addRecoveryKey,
  credentials,
  onLinkAccount,
  onUnlinkAccount,
  dapps,
  exploreDapps,
  identityBackground,
  tempKeysWarning,
  currentCredential,
}: {
  userNumber: bigint;
  devices: Devices;
  onAddDevice: () => void;
  onRemoveDevice: (device: DeviceWithUsage) => void;
  addRecoveryPhrase: () => void;
  addRecoveryKey: () => void;
  credentials: OpenIdCredential[];
  onLinkAccount: () => void;
  onUnlinkAccount: (credential: OpenIdCredential) => void;
  dapps: KnownDapp[];
  exploreDapps?: () => void;
  identityBackground: PreLoadImage;
  tempKeysWarning?: TempKeyWarningAction;
  currentCredential?: Pick<OpenIdCredential, "iss" | "sub">;
}): TemplateResult => {
  // Nudge the user to add a passkey if there is none
  const warnNoPasskeys = authenticators.length === 0;
  // Recommend the user to clean up passkeys if there are
  // authenticators registered across multiple domains.
  const cleanupRecommended =
    new Set(authenticators.map((authenticator) => authenticator.rpId)).size > 1;
  const i18n = new I18n();

  const pageContentSlot = html`<section data-role="identity-management">
    <hgroup>
      <h1 class="t-title t-title--main">Manage your<br />Internet Identity</h1>
    </hgroup>
    ${anchorSection({ userNumber, identityBackground })}
    ${nonNullish(tempKeysWarning)
      ? tempKeyWarningBox({ i18n, warningAction: tempKeysWarning })
      : ""}
    ${pinAuthenticators.length > 0
      ? tempKeysSection({
          authenticators: pinAuthenticators,
          i18n,
          onRemoveDevice,
        })
      : ""}
    ${authenticatorsSection({
      authenticators,
      onAddDevice,
      onRemoveDevice,
      warnNoPasskeys,
      cleanupRecommended,
      i18n,
    })}
    ${get(OPENID_AUTHENTICATION)
      ? linkedAccountsSection({
          credentials,
          onLinkAccount,
          onUnlinkAccount,
          hasOtherAuthMethods: authenticators.length > 0,
          currentCredential,
        })
      : ""}
    ${recoveryMethodsSection({
      recoveries,
      addRecoveryPhrase,
      addRecoveryKey,
      onRemoveDevice,
    })}
    ${nonNullish(exploreDapps)
      ? html`<aside class="l-stack">
          ${dappsTeaser({
            dapps,
            click: () => exploreDapps(),
            copy: {
              dapps_explorer: "Dapps explorer",
              sign_into_dapps: "Connect to these dapps",
            },
          })}
        </aside>`
      : undefined}
    ${logoutSection()}
  </section>`;

  return mainWindow({
    slot: pageContentSlot,
  });
};

const anchorSection = ({
  userNumber,
  identityBackground,
}: {
  userNumber: bigint;
  identityBackground: PreLoadImage;
}): TemplateResult => html`
  <aside class="l-stack">
    <div
      class="c-input c-input--stack c-input--fullwidth c-input--textarea c-input--readonly c-input--icon c-input--id"
    >
      ${identityCard({
        userNumber,
        identityBackground,
      }) satisfies TemplateElement}
    </div>
  </aside>
`;

export const renderManageWarmup = (): OmitParams<
  typeof renderManage,
  "identityBackground"
> => {
  const identityBackground = new PreLoadImage(identityCardBackground);
  return async (opts) => {
    return await renderManage({ ...opts, identityBackground });
  };
};

// Get the list of devices from canister and actually display the page
export const renderManage = async ({
  userNumber,
  connection: origConnection,
  identityBackground,
}: {
  userNumber: bigint;
  connection: AuthenticatedConnection;
  identityBackground: PreLoadImage;
}): Promise<never> => {
  let connection = origConnection;

  // There's nowhere to go from here (i.e. all flows lead to/start from this page), so we
  // loop forever
  for (;;) {
    let anchorInfo: IdentityAnchorInfo;
    try {
      // Ignore the `commitMetadata` response, it's not critical for the application.
      void connection.commitMetadata();
      anchorInfo = await withLoader(() => connection.getAnchorInfo());
    } catch (error: unknown) {
      await displayFailedToListDevices(
        error instanceof Error ? error : unknownError(),
      );
      continue;
    }
    if (anchorInfo.device_registration.length !== 0) {
      // we are actually in a device registration process
      await addDevice({ userNumber, connection, origin: window.origin });
      continue;
    }

    // If some flow e.g. replaced the current device, use the new connection
    const newConnection = await displayManage(
      userNumber,
      connection,
      anchorInfo.devices,
      anchorInfo.openid_credentials[0] ?? [],
      identityBackground,
    );
    connection = newConnection ?? connection;
  }
};

export const displayManagePage = renderPage(displayManageTemplate);

function isPinAuthenticated(
  devices_: DeviceData[],
  connection: AuthenticatedConnection,
): boolean {
  const connectionPrincipal = connection.identity.getPrincipal();
  const currentDevice = devices_.find(({ pubkey }) => {
    const devicePrincipal = Principal.selfAuthenticating(
      new Uint8Array(pubkey),
    );
    return devicePrincipal.toText() === connectionPrincipal.toText();
  });
  return (
    nonNullish(currentDevice) && "browser_storage_key" in currentDevice.key_type
  );
}

export const displayManage = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices_: DeviceWithUsage[],
  credentials: OpenIdCredential[],
  identityBackground: PreLoadImage,
): Promise<void | AuthenticatedConnection> => {
  const i18n = new I18n();
  const copy = i18n.i18n(copyJson);

  // Fetch the dapps used in the teaser & explorer
  // (dapps are suffled to encourage discovery of new dapps)
  const dapps = shuffleArray(getDapps());

  // Create anonymous nonce and salt for calling principal from connection
  const { nonce, salt } = await createAnonymousNonce(
    connection.delegationIdentity.getPrincipal(),
  );

  const googleClientId =
    connection.canisterConfig.openid_google[0]?.[0]?.client_id;

  return new Promise((resolve) => {
    const devices = devicesFromDevicesWithUsage({
      devices: devices_,
      userNumber,
      connection,
      reload: resolve,
      hasOtherAuthMethods: credentials.length > 0,
    });

    if (devices.dupPhrase) {
      toast.error(
        "More than one recovery phrases are registered, which is unexpected. Only one will be shown.",
      );
    }
    if (devices.dupKey) {
      toast.error(
        "More than one recovery keys are registered, which is unexpected. Only one will be shown.",
      );
    }

    const onAddDevice = async () => {
      const newDeviveOrigin =
        userSupportsWebauthRoR() && get(DOMAIN_COMPATIBILITY)
          ? getCredentialsOrigin({
              credentials: devices_,
            })
          : undefined;
      await addDevice({
        userNumber,
        connection,
        origin: newDeviveOrigin ?? window.origin,
      });
      resolve();
    };

    const onRemoveDevice = async (device: DeviceWithUsage) => {
      const pubKey: DerEncodedPublicKey = new Uint8Array(device.pubkey)
        .buffer as DerEncodedPublicKey;
      const isCurrentDevice = bufferEqual(
        connection.identity.getPublicKey().toDer(),
        pubKey,
      );
      const action = await confirmRemoveDevice({
        i18n,
        purpose: device.purpose,
        lastUsedNanoseconds: device.last_usage[0],
        originRegistered: device.origin[0],
        alias: device.alias,
        isCurrentDevice,
      });
      if (action === "cancelled") {
        // This triggers a spinner which refetches the data.
        // But this is the UX we alreay have now.
        // We will improve this UX with the Svelte integration. Not worth improving now.
        resolve();
        return;
      }
      await withLoader(() => {
        return Promise.all([connection.remove(device.pubkey)]);
      });

      if (isCurrentDevice) {
        // reload the page.
        // do not call "reload", otherwise the management page will try to reload the list of devices which will cause an error
        location.reload();
        return;
      } else {
        resolve();
      }
    };

    const addRecoveryPhrase = async () => {
      const doAdd = await addPhrase({ intent: "userInitiated" });
      if (doAdd === "cancel") {
        resolve();
        return;
      }
      doAdd satisfies "ok";
      // Recovery phrase doesn't need ROR, this is for consistency reasons.
      const newDeviceOrigin = get(DOMAIN_COMPATIBILITY)
        ? getCredentialsOrigin({
            credentials: devices_,
          })
        : undefined;
      await setupPhrase(
        userNumber,
        connection,
        newDeviceOrigin ?? window.origin,
      );
      resolve();
    };

    const onLinkAccount = async () => {
      if (isNullish(googleClientId)) {
        toast.error(copy.linking_google_accounts_is_unavailable);
        return;
      }
      try {
        const jwt = await withLoader(() =>
          requestJWT(createGoogleRequestConfig(googleClientId), {
            mediation: "required",
            nonce,
          }),
        );
        const { iss, sub } = decodeJWT(jwt);
        if (credentials.find((c) => c.iss === iss && c.sub === sub)) {
          toast.error(copy.account_already_linked);
          return;
        }
        await connection.addOpenIdCredential(jwt, salt);
        resolve();
      } catch (error) {
        if (isOpenIdCancelError(error)) {
          toast.error(copy.third_party_sign_in_permission_required);
          return;
        }
        if (isCanisterError<OpenIdCredentialAddError>(error)) {
          switch (error.type) {
            case "Unauthorized":
              toast.error(copy.authentication_failed);
              console.error(
                `Authentication unexpectedly failed: ${error
                  .value(error.type)
                  .toText()}`,
              );
              break;
            case "JwtVerificationFailed":
              toast.error(copy.jwt_signature_invalid);
              break;
            case "JwtExpired":
              toast.error(copy.jwt_signature_invalid);
              break;
            case "OpenIdCredentialAlreadyRegistered":
              toast.error(copy.account_already_linked);
              break;
            case "InternalCanisterError":
              toast.error(`Unexpected error: ${error.value(error.type)}`);
              break;
            default: {
              // Make sure all error cases are covered,
              // else this will throw a TS error here.
              const _ = error.type satisfies never;
            }
          }
          return;
        }
        throw error;
      }
    };
    const onUnlinkAccount = async (credential: OpenIdCredential) => {
      const isCurrentCredential =
        nonNullish(connection.credential) &&
        credential.iss === connection.credential.iss &&
        credential.sub === connection.credential.sub;
      const action = await confirmUnlinkAccount({
        i18n,
        credential,
        isCurrentCredential,
      });
      if (action === "cancelled") {
        resolve();
        return;
      }
      try {
        await connection.removeOpenIdCredential(credential.iss, credential.sub);
        if (isCurrentCredential) {
          location.reload(); // Reload page to go back to sign in
          return;
        } else {
          resolve();
        }
      } catch (error) {
        if (isCanisterError<OpenIdCredentialRemoveError>(error)) {
          switch (error.type) {
            case "Unauthorized":
              toast.error(copy.authentication_failed);
              console.error(
                `Authentication unexpectedly failed: ${error
                  .value(error.type)
                  .toText()}`,
              );
              break;
            case "OpenIdCredentialNotFound":
              toast.error(copy.account_not_found);
              break;
            case "InternalCanisterError":
              toast.error(`Unexpected error: ${error.value(error.type)}`);
              break;
            default: {
              // Make sure all error cases are covered,
              // else this will throw a TS error here.
              const _ = error.type satisfies never;
            }
          }
        }
      }
    };

    // Function to figure out what temp keys warning should be shown, if any.
    const determineTempKeysWarning = (): TempKeyWarningAction | undefined => {
      if (!isPinAuthenticated(devices_, connection)) {
        // Don't show the warning, if the user is not authenticated using a PIN
        // protected browser storage key
        return undefined;
      }
      // First priority, nudge to add a recovery phrase
      if (devices.recoveries.recoveryPhrase === undefined) {
        return {
          tag: "add_recovery",
          action: addRecoveryPhrase,
        };
      }
      // Second priority, nudge to add a passkey
      if (devices.authenticators.length === 0) {
        return {
          tag: "add_passkey",
          action: onAddDevice,
        };
      }
      // If both, recovery phrase and passkey are present, don't show a warning
      return undefined;
    };

    const dappsExplorerEnabled: boolean =
      connection.canisterConfig.enable_dapps_explorer[0] ?? false;
    const onExploreDapps = async () => {
      await dappsExplorer({ dapps });
      // We know that the user couldn't have changed anything (the user can't delete e.g. delete
      // a device from the explorer), so we just re-display without reloading devices etc.
      // the page without
      display();
    };

    const display = () =>
      displayManagePage({
        userNumber,
        devices,
        onAddDevice,
        onRemoveDevice,
        addRecoveryPhrase,
        addRecoveryKey: async () => {
          await setupKey({ connection });
          resolve();
        },
        credentials,
        onLinkAccount,
        onUnlinkAccount,
        dapps,
        exploreDapps: dappsExplorerEnabled ? onExploreDapps : undefined,
        identityBackground,
        tempKeysWarning: determineTempKeysWarning(),
        currentCredential: connection.credential,
      });

    display();
  });
};

// Try to read a DeviceData as a recovery
export const readRecovery = ({
  userNumber,
  connection,
  reload,
  device,
}: {
  device: DeviceWithUsage;
  userNumber: bigint;
  connection: AuthenticatedConnection;
  reload: () => void;
}):
  | { recoveryPhrase: RecoveryPhrase }
  | { recoveryKey: RecoveryKey }
  | undefined => {
  if (isRecoveryDevice(device)) {
    if (isRecoveryPhrase(device)) {
      const protection: Protection = isProtected(device)
        ? {
            isProtected: true,
            unprotect: () => unprotectDevice(connection, device, reload),
          }
        : {
            isProtected: false,
            protect: () =>
              protectDevice({
                connection,
                device,
                reload,
              }),
          };
      return {
        recoveryPhrase: {
          reset: () =>
            resetPhrase({
              userNumber,
              connection,
              device,
              reload,
            }),
          ...protection,
        },
      };
    } else {
      return {
        recoveryKey: {
          device,
        },
      };
    }
  }
};

// Convert devices read from the canister into types that are easier to work with
// and that better represent what we expect.
// Exported for testing purposes
export const devicesFromDevicesWithUsage = ({
  devices: devices_,
  reload,
  connection,
  userNumber,
  hasOtherAuthMethods,
}: {
  devices: DeviceWithUsage[];
  reload: (connection?: AuthenticatedConnection) => void;
  connection: AuthenticatedConnection;
  userNumber: bigint;
  hasOtherAuthMethods: boolean;
}): Devices & { dupPhrase: boolean; dupKey: boolean } => {
  const hasSingleDevice = devices_.length <= 1;
  const currentPublicKey = connection.getSignIdentityPubKey();

  return devices_.reduce<Devices & { dupPhrase: boolean; dupKey: boolean }>(
    (acc, device) => {
      const recovery = readRecovery({ userNumber, connection, reload, device });
      if (nonNullish(recovery)) {
        if ("recoveryPhrase" in recovery) {
          if (nonNullish(acc.recoveries.recoveryPhrase)) {
            acc.dupPhrase = true;
          }
          acc.recoveries.recoveryPhrase = recovery.recoveryPhrase;
        } else if ("recoveryKey" in recovery) {
          if (nonNullish(acc.recoveries.recoveryKey)) {
            acc.dupKey = true;
          }
          acc.recoveries.recoveryKey = recovery.recoveryKey;
        } else {
          unreachable(recovery, "returned unexpected recovery");
        }
        return acc;
      }

      const canBeRemoved = !(hasSingleDevice && !hasOtherAuthMethods);
      const authenticator: Authenticator = {
        alias: device.alias,
        rpId: rpIdFromDevice(device),
        last_usage: device.last_usage,
        warn: domainWarning(device),
        rename: () => renameDevice({ connection, device, reload }),
        canBeRemoved,
        isCurrent: bufferEqual(
          currentPublicKey,
          new Uint8Array(device.pubkey).buffer as ArrayBuffer,
        ),
        device,
      };

      if ("browser_storage_key" in device.key_type) {
        acc.pinAuthenticators.push(authenticator);
      } else {
        acc.authenticators.push(authenticator);
      }
      return acc;
    },
    {
      authenticators: [],
      recoveries: {},
      pinAuthenticators: [],
      dupPhrase: false,
      dupKey: false,
    },
  );
};

// Show a domain-related warning, if necessary.
export const domainWarning = (
  device: DeviceData,
): TemplateResult | undefined => {
  if (get(DOMAIN_COMPATIBILITY)) {
    return undefined;
  }
  // Recovery phrases are not FIDO devices, meaning they are not tied to a particular origin (unless most authenticators like TouchID, etc, and e.g. recovery _devices_ in the case of YubiKeys and the like)
  if (isRecoveryPhrase(device)) {
    return undefined;
  }

  // XXX: work around didc-generated oddities in types
  const deviceOrigin =
    device.origin.length === 0 ? undefined : device.origin[0];

  // If this is the _old_ II (ic0.app) and no origin was recorded, then we can't infer much and don't show a warning.
  if (window.location.origin === LEGACY_II_URL && isNullish(deviceOrigin)) {
    return undefined;
  }

  // If this is the _old_ II (ic0.app) and the device has an origin that is _not_ ic0.app, then the device was probably migrated and can't be used on ic0.app anymore.
  if (
    window.location.origin === LEGACY_II_URL &&
    deviceOrigin !== window.location.origin
  ) {
    return html`This Passkey may not be usable on the current URL
    (${window.location.origin})`;
  }

  // In general, if this is _not_ the _old_ II, then it's most likely the _new_ II, meaning all devices should have an origin attached.
  if (isNullish(deviceOrigin)) {
    return html`This Passkey may not be usable on the current URL
    (${window.location.origin})`;
  }

  // Finally, in general if the device has an origin but this is not _this_ origin, we issue a warning
  if (deviceOrigin !== window.location.origin) {
    return html`This Passkey may not be usable on the current URL
    (${window.location.origin})`;
  }
};

const rpIdFromDevice = (device: DeviceWithUsage) =>
  new URL(device.origin[0] ?? LEGACY_II_URL).hostname;

const unknownError = (): Error => {
  return new Error("Unknown error");
};
