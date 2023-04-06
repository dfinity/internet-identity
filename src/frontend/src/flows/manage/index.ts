import { html, TemplateResult } from "lit-html";
import {
  DeviceData,
  IdentityAnchorInfo,
} from "../../../generated/internet_identity_types";
import { showWarning } from "../../banner";
import {
  authenticateBox,
  AuthnTemplates,
} from "../../components/authenticateBox";
import { displayError } from "../../components/displayError";
import { withLoader } from "../../components/loader";
import { logoutSection } from "../../components/logout";
import { mainWindow } from "../../components/mainWindow";
import { toast } from "../../components/toast";
import { LEGACY_II_URL } from "../../config";
import { AuthenticatedConnection, Connection } from "../../utils/iiConnection";
import { renderPage } from "../../utils/lit-html";
import {
  hasRecoveryPhrase,
  isProtected,
  isRecoveryDevice,
  isRecoveryPhrase,
} from "../../utils/recoveryDevice";
import { unreachable } from "../../utils/utils";
import { chooseDeviceAddFlow } from "../addDevice/manage";
import { addLocalDevice } from "../addDevice/manage/addLocalDevice";
import { addRemoteDevice } from "../addDevice/manage/addRemoteDevice";
import { recoveryWizard } from "../recovery/recoveryWizard";
import { setupKey, setupPhrase } from "../recovery/setupRecovery";
import { authenticatorsSection } from "./authenticatorsSection";
import {
  deleteDevice,
  protectDevice,
  resetPhrase,
  unprotectDevice,
} from "./deviceSettings";
import { recoveryMethodsSection } from "./recoveryMethodsSection";
import { Devices, Protection, RecoveryKey, RecoveryPhrase } from "./types";

/* Template for the authbox when authenticating to II */
export const authnTemplateManage = (): AuthnTemplates => {
  const wrap = ({
    slot,
    title,
  }: {
    slot: string;
    title: string;
  }): TemplateResult => html`
    <header class="t-centered">
      <h1 class="t-title t-title--main">${title}</h1>
      <p class="t-lead">${slot}</p>
    </header>
  `;
  return {
    firstTime: {
      slot: wrap({
        slot: `to dapps on the Internet Computer`,
        title: "Securely Connect",
      }),
      useExistingText: "Manage Existing",
      createAnchorText: "Create Identity Anchor",
    },
    useExisting: {
      slot: wrap({
        slot: `to continue to Internet Identity`,
        title: "Enter your Anchor",
      }),
    },

    pick: {
      slot: wrap({
        slot: "to continue to Internet Identity",
        title: "Choose an Anchor",
      }),
    },
  };
};

/* the II authentication flow */
export const authFlowManage = async (connection: Connection) => {
  // Go through the login flow, potentially creating an anchor.
  const { userNumber, connection: authenticatedConnection } =
    await authenticateBox(connection, authnTemplateManage());

  // Here, if the user doesn't have any recovery device, we prompt them to add
  // one. The exact flow depends on the device they use.
  await recoveryWizard(userNumber, authenticatedConnection);
  // From here on, the user is authenticated to II.
  void renderManage(userNumber, authenticatedConnection);
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
}: {
  userNumber: bigint;
  devices: Devices;
  onAddDevice: () => void;
  addRecoveryPhrase: () => void;
  addRecoveryKey: () => void;
}): TemplateResult => {
  // Nudge the user to add a device iff there is one or fewer authenticators and no recoveries
  const warnFewDevices =
    authenticators.length <= 1 &&
    recoveries.recoveryPhrase === undefined &&
    recoveries.recoveryKey === undefined;

  const pageContentSlot = html` <section>
    <hgroup>
      <h1 class="t-title t-title--main">Manage your Anchor</h1>
      <p class="t-lead">
        Add devices and recovery methods to make your anchor more secure.
      </p>
    </hgroup>
    ${anchorSection(userNumber)}
    ${authenticatorsSection({
      authenticators,
      onAddDevice,
      warnFewDevices,
    })}
    ${recoveryMethodsSection({ recoveries, addRecoveryPhrase, addRecoveryKey })}
    ${logoutSection()}
  </section>`;

  return mainWindow({
    slot: pageContentSlot,
  });
};

const anchorSection = (userNumber: bigint): TemplateResult => html`
  <aside class="l-stack">
    <h2 class="t-title">Identity Anchor</h2>
    <output
      class="c-input c-input--vip c-input--readonly t-vip"
      aria-label="User Number"
      data-usernumber
      >${userNumber}</output
    >
  </aside>
`;

// Get the list of devices from canister and actually display the page
export const renderManage = async (
  userNumber: bigint,
  origConnection: AuthenticatedConnection
): Promise<never> => {
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
      await addRemoteDevice({ userNumber, connection });
      continue;
    }

    // If some flow e.g. replaced the current device, use the new connection
    const newConnection = await displayManage(
      userNumber,
      connection,
      anchorInfo.devices
    );
    connection = newConnection ?? connection;
  }
};

export const displayManagePage = renderPage(displayManageTemplate);

export const displayManage = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices_: DeviceData[]
): Promise<void | AuthenticatedConnection> =>
  new Promise((resolve) => {
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
    displayManagePage({
      userNumber,
      devices,
      onAddDevice: async () => {
        const nextAction = await chooseDeviceAddFlow();
        switch (nextAction) {
          case "canceled": {
            resolve();
            break;
          }
          case "local": {
            await addLocalDevice(userNumber, connection, devices_);
            resolve();
            break;
          }
          case "remote": {
            await addRemoteDevice({ userNumber, connection });
            resolve();
            break;
          }
          default:
            unreachable(nextAction);
            resolve();
            break;
        }
      },
      addRecoveryPhrase: async () => {
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
    });

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
            unprotect: () =>
              unprotectDevice(userNumber, connection, device, reload),
          }
        : {
            isProtected: false,
            protect: () =>
              protectDevice({
                userNumber,
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
  return undefined;
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
      if (recovery !== undefined) {
        if ("recoveryPhrase" in recovery) {
          if (acc.recoveries.recoveryPhrase !== undefined) {
            acc.dupPhrase = true;
          }
          acc.recoveries.recoveryPhrase = recovery.recoveryPhrase;
        } else if ("recoveryKey" in recovery) {
          if (acc.recoveries.recoveryKey !== undefined) {
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
  if (window.origin === LEGACY_II_URL && deviceOrigin === undefined) {
    return undefined;
  }

  // If this is the _old_ II (ic0.app) and the device has an origin that is _not_ ic0.app, then the device was probably migrated and can't be used on ic0.app anymore.
  if (window.origin === LEGACY_II_URL && deviceOrigin !== window.origin) {
    return html`This device may not be usable on the current URL
    (${window.origin})`;
  }

  // In general, if this is _not_ the _old_ II, then it's most likely the _new_ II, meaning all devices should have an origin attached.
  if (deviceOrigin === undefined) {
    return html`This device may not be usable on the current URL
    (${window.origin})`;
  }

  // Finally, in general if the device has an origin but this is not _this_ origin, we issue a warning
  if (deviceOrigin !== window.origin) {
    return html`This device may not be usable on the current URL
    (${window.origin})`;
  }

  return undefined;
};

const unknownError = (): Error => {
  return new Error("Unknown error");
};
