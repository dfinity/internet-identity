import {
  DeviceData,
  IdentityAnchorInfo,
} from "$generated/internet_identity_types";
import { showWarning } from "$src/banner";
import {
  AuthnTemplates,
  authenticateBox,
} from "$src/components/authenticateBox";
import { displayError } from "$src/components/displayError";
import {
  IdentityBackground,
  identityCard,
  loadIdentityBackground,
} from "$src/components/identityCard";
import { withLoader } from "$src/components/loader";
import { logoutSection } from "$src/components/logout";
import { mainWindow } from "$src/components/mainWindow";
import { toast } from "$src/components/toast";
import { LEGACY_II_URL } from "$src/config";
import { addDevice } from "$src/flows/addDevice/manage/addDevice";
import { dappsExplorer } from "$src/flows/dappsExplorer";
import { KnownDapp, getDapps } from "$src/flows/dappsExplorer/dapps";
import { dappsHeader, dappsTeaser } from "$src/flows/dappsExplorer/teaser";
import { addPhrase, recoveryWizard } from "$src/flows/recovery/recoveryWizard";
import { setupKey, setupPhrase } from "$src/flows/recovery/setupRecovery";
import { I18n } from "$src/i18n";
import { AuthenticatedConnection, Connection } from "$src/utils/iiConnection";
import { TemplateElement, renderPage } from "$src/utils/lit-html";
import {
  hasRecoveryPhrase,
  isProtected,
  isRecoveryDevice,
  isRecoveryPhrase,
} from "$src/utils/recoveryDevice";
import { OmitParams, shuffleArray, unreachable } from "$src/utils/utils";
import { isNullish, nonNullish } from "@dfinity/utils";
import { TemplateResult, html } from "lit-html";
import { authenticatorsSection } from "./authenticatorsSection";
import {
  deleteDevice,
  protectDevice,
  renameDevice,
  resetPhrase,
  unprotectDevice,
} from "./deviceSettings";
import { recoveryMethodsSection } from "./recoveryMethodsSection";
import { Devices, Protection, RecoveryKey, RecoveryPhrase } from "./types";

/* Template for the authbox when authenticating to II */
export const authnTemplateManage = ({
  dapps,
}: {
  dapps: KnownDapp[];
}): AuthnTemplates => {
  const wrap = ({
    title,
    subtitle,
    showDapps = false,
  }: {
    title: TemplateElement;
    subtitle?: string;
    showDapps?: boolean;
  }): TemplateResult => html`
    ${showDapps ? dappsHeader({ dapps, clickable: false }) : undefined}
    <header class="l-stack">
      <h1 class="t-title t-title--main">${title}</h1>
      ${nonNullish(subtitle)
        ? html` <p class="t-lead l-stack">${subtitle}</p>`
        : undefined}
    </header>
  `;
  return {
    firstTime: {
      slot: wrap({
        title: "Securely connect to dapps on the Internet Computer",
        showDapps: true,
      }),
      useExistingText: "Use existing",
      createAnchorText: "Create Internet Identity",
    },
    useExisting: {
      slot: wrap({
        title: html`Enter your <br />Internet Identity`,
        subtitle: "to continue",
      }),
    },

    pick: {
      slot: wrap({
        title: html`Choose your <br />Internet Identity`,
        subtitle: "to continue",
      }),
    },
  };
};

/* the II authentication flow */
export const authFlowManage = async (connection: Connection) => {
  const i18n = new I18n();
  const dapps = shuffleArray(getDapps());

  const identityBackground = loadIdentityBackground();
  // Go through the login flow, potentially creating an anchor.
  const {
    userNumber,
    connection: authenticatedConnection,
    newAnchor,
  } = await authenticateBox(connection, i18n, authnTemplateManage({ dapps }));

  // Here, if the user is returning & doesn't have any recovery device, we prompt them to add
  // one. The exact flow depends on the device they use.
  if (!newAnchor) {
    await recoveryWizard(userNumber, authenticatedConnection);
  }
  // From here on, the user is authenticated to II.
  void renderManage({
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
  devices: { authenticators, recoveries },
  onAddDevice,
  addRecoveryPhrase,
  addRecoveryKey,
  dapps,
  exploreDapps,
  identityBackground,
}: {
  userNumber: bigint;
  devices: Devices;
  onAddDevice: () => void;
  addRecoveryPhrase: () => void;
  addRecoveryKey: () => void;
  dapps: KnownDapp[];
  exploreDapps: () => void;
  identityBackground: IdentityBackground;
}): TemplateResult => {
  // Nudge the user to add a device iff there is one or fewer authenticators and no recoveries
  const warnFewDevices =
    authenticators.length <= 1 &&
    isNullish(recoveries.recoveryPhrase) &&
    isNullish(recoveries.recoveryKey);

  const pageContentSlot = html` <section data-role="identity-management">
    <hgroup>
      <h1 class="t-title t-title--main">Manage your<br />Internet Identity</h1>
    </hgroup>
    ${anchorSection({ userNumber, identityBackground })}
    <p class="t-paragraph">
      ${dappsTeaser({
        dapps,
        click: () => exploreDapps(),
        copy: {
          dapps_explorer: "Dapps explorer",
          sign_into_dapps: "Connect to these dapps",
        },
      })}
    </p>
    ${authenticatorsSection({
      authenticators,
      onAddDevice,
      warnFewDevices,
    })}
    ${recoveryMethodsSection({ recoveries, addRecoveryPhrase, addRecoveryKey })}
    ${logoutSection()}
  </section>`;

  return mainWindow({
    isWideContainer: true,
    slot: pageContentSlot,
  });
};

const anchorSection = ({
  userNumber,
  identityBackground,
}: {
  userNumber: bigint;
  identityBackground: IdentityBackground;
}): TemplateResult => html`
  <aside class="l-stack">
    <div
      class="c-input c-input--textarea c-input--readonly c-input--icon c-input--id"
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
  const identityBackground = loadIdentityBackground();
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
  identityBackground: IdentityBackground;
}): Promise<never> => {
  let connection = origConnection;

  // There's nowhere to go from here (i.e. all flows lead to/start from this page), so we
  // loop forever
  for (;;) {
    let anchorInfo: IdentityAnchorInfo;
    try {
      anchorInfo = await withLoader(() => connection.getAnchorInfo());
    } catch (error: unknown) {
      await displayFailedToListDevices(
        error instanceof Error ? error : unknownError()
      );
      continue;
    }
    if (anchorInfo.device_registration.length !== 0) {
      // we are actually in a device registration process
      await addDevice({ userNumber, connection });
      continue;
    }

    // If some flow e.g. replaced the current device, use the new connection
    const newConnection = await displayManage(
      userNumber,
      connection,
      anchorInfo.devices,
      identityBackground
    );
    connection = newConnection ?? connection;
  }
};

export const displayManagePage = renderPage(displayManageTemplate);

export const displayManage = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices_: DeviceData[],
  identityBackground: IdentityBackground
): Promise<void | AuthenticatedConnection> => {
  // Fetch the dapps used in the teaser & explorer
  // (dapps are suffled to encourage discovery of new dapps)
  const dapps = shuffleArray(getDapps());
  return new Promise((resolve) => {
    const devices = devicesFromDeviceDatas({
      devices: devices_,
      userNumber,
      connection,
      reload: resolve,
    });
    if (devices.dupPhrase) {
      toast.error(
        "More than one recovery phrases are registered, which is unexpected. Only one will be shown."
      );
    }
    if (devices.dupKey) {
      toast.error(
        "More than one recovery keys are registered, which is unexpected. Only one will be shown."
      );
    }
    const display = () =>
      displayManagePage({
        userNumber,
        devices,
        onAddDevice: async () => {
          await addDevice({ userNumber, connection });
          resolve();
        },
        addRecoveryPhrase: async () => {
          const doAdd = await addPhrase({ intent: "userInitiated" });
          if (doAdd === "cancel") {
            resolve();
            return;
          }
          doAdd satisfies "ok";
          await setupPhrase(userNumber, connection);
          resolve();
        },
        addRecoveryKey: async () => {
          const confirmed = confirm(
            "Add a Recovery Device\n\nUse a FIDO Security Key, like a YubiKey, as an additional recovery method."
          );
          if (!confirmed) {
            // No resolve here because we don't need to reload the screen
            return;
          }
          await setupKey({ connection });
          resolve();
        },
        dapps,
        exploreDapps: async () => {
          await dappsExplorer({ dapps });
          // We know that the user couldn't have changed anything (the user can't delete e.g. delete
          // a device from the explorer), so we just re-display without reloading devices etc.
          // the page without
          display();
        },
        identityBackground,
      });

    display();

    // When visiting the legacy URL (ic0.app) we extra-nudge the users to create a recovery phrase,
    // if they don't have one already. We lead them straight to recovery phrase creation, because
    // recovery _device_ would be tied to the domain (which we want to avoid).
    if (
      window.location.origin === LEGACY_II_URL &&
      !hasRecoveryPhrase(devices_)
    ) {
      const elem = showWarning(html`<strong class="t-strong">Important!</strong>
        Create a recovery phrase.
        <button
          class="features-warning-btn"
          @click=${async () => {
            await setupPhrase(userNumber, connection);
            elem.remove();
            resolve();
          }}
        >
          Create
        </button> `);
    }
  });
};

// Try to read a DeviceData as a recovery
export const readRecovery = ({
  userNumber,
  connection,
  reload,
  device,
}: {
  device: DeviceData;
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
          remove: () => deleteDevice({ connection, device, reload }),
        },
      };
    }
  }
};

// Convert devices read from the canister into types that are easier to work with
// and that better represent what we expect.
export const devicesFromDeviceDatas = ({
  devices: devices_,
  reload,
  connection,
  userNumber,
}: {
  devices: DeviceData[];
  reload: (connection?: AuthenticatedConnection) => void;
  connection: AuthenticatedConnection;
  userNumber: bigint;
}): Devices & { dupPhrase: boolean; dupKey: boolean } => {
  const hasSingleDevice = devices_.length <= 1;

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

      acc.authenticators.push({
        alias: device.alias,
        warn: domainWarning(device),
        rename: () => renameDevice({ connection, device, reload }),
        remove: hasSingleDevice
          ? undefined
          : () => deleteDevice({ connection, device, reload }),
      });
      return acc;
    },
    { authenticators: [], recoveries: {}, dupPhrase: false, dupKey: false }
  );
};

// Show a domain-related warning, if necessary.
export const domainWarning = (
  device: DeviceData
): TemplateResult | undefined => {
  // Recovery phrases are not FIDO devices, meaning they are not tied to a particular origin (unless most authenticators like TouchID, etc, and e.g. recovery _devices_ in the case of YubiKeys and the like)
  if (isRecoveryPhrase(device)) {
    return undefined;
  }

  // XXX: work around didc-generated oddities in types
  const deviceOrigin =
    device.origin.length === 0 ? undefined : device.origin[0];

  // If this is the _old_ II (ic0.app) and no origin was recorded, then we can't infer much and don't show a warning.
  if (window.origin === LEGACY_II_URL && isNullish(deviceOrigin)) {
    return undefined;
  }

  // If this is the _old_ II (ic0.app) and the device has an origin that is _not_ ic0.app, then the device was probably migrated and can't be used on ic0.app anymore.
  if (window.origin === LEGACY_II_URL && deviceOrigin !== window.origin) {
    return html`This Passkey may not be usable on the current URL
    (${window.origin})`;
  }

  // In general, if this is _not_ the _old_ II, then it's most likely the _new_ II, meaning all devices should have an origin attached.
  if (isNullish(deviceOrigin)) {
    return html`This Passkey may not be usable on the current URL
    (${window.origin})`;
  }

  // Finally, in general if the device has an origin but this is not _this_ origin, we issue a warning
  if (deviceOrigin !== window.origin) {
    return html`This Passkey may not be usable on the current URL
    (${window.origin})`;
  }
};

const unknownError = (): Error => {
  return new Error("Unknown error");
};
