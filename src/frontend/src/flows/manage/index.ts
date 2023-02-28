import { TemplateResult, render, html } from "lit-html";
import { LEGACY_II_URL } from "../../config";
import { Connection, AuthenticatedConnection } from "../../utils/iiConnection";
import { withLoader } from "../../components/loader";
import { unreachable } from "../../utils/utils";
import { logoutSection } from "../../components/logout";
import { deviceSettings } from "./deviceSettings";
import { showWarning } from "../../banner";
import {
  DeviceData,
  IdentityAnchorInfo,
} from "../../../generated/internet_identity_types";
import { displayError } from "../../components/displayError";
import {
  authenticateBox,
  AuthnTemplates,
} from "../../components/authenticateBox";
import { setupRecovery, setupPhrase } from "../recovery/setupRecovery";
import { recoveryWizard } from "../recovery/recoveryWizard";
import { pollForTentativeDevice } from "../addDevice/manage/pollForTentativeDevice";
import { chooseDeviceAddFlow } from "../addDevice/manage";
import { addLocalDevice } from "../addDevice/manage/addLocalDevice";
import { warnBox } from "../../components/warnBox";
import { Device } from "../../components/deviceListItem";
import { recoveryMethods } from "../../components/recoveryMethods";
import { devicesSection } from "../../components/devicesSection";
import { mainWindow } from "../../components/mainWindow";
import {
  isRecoveryDevice,
  recoveryDeviceToLabel,
  isProtected,
  hasRecoveryPhrase,
  isRecoveryPhrase,
} from "../../utils/recoveryDevice";

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
  renderManage(userNumber, authenticatedConnection);
};

const displayFailedToListDevices = (error: Error) =>
  displayError({
    title: "Failed to list your devices",
    message:
      "An unexpected error occurred when displaying your devices. Please try again",
    detail: error.toString(),
    primaryButton: "Try again",
  });

// The maximum number of authenticator (non-recovery) devices we allow.
// The canister limits the _total_ number of devices (recovery included) to 10,
// and we (the frontend) only allow user one recovery device per type (phrase, fob),
// which leaves room for 8 authenticator devices.
const MAX_AUTHENTICATORS = 8;

// Actual page content. We display the Identity Anchor and the list of
// (non-recovery) devices. Additionally, if the user does _not_ have any
// recovery devices, we display a warning "nag box" and suggest to the user
// that they add a recovery device. If the user _does_ have at least one
// recovery device, then we do not display a "nag box", but we list the
// recovery devices.
const displayManageTemplate = ({
  userNumber,
  authenticators,
  recoveries,
  onAddDevice,
  onAddRecovery,
}: {
  userNumber: bigint;
  authenticators: Device[];
  recoveries: Device[];
  onAddDevice: () => void;
  onAddRecovery: () => void;
}): TemplateResult => {
  const pageContentSlot = html` <section>
    <hgroup>
      <h1 class="t-title t-title--main">Manage your Anchor</h1>
      <p class="t-lead">
        Add devices and recovery methods to make your anchor more secure.
      </p>
    </hgroup>
    ${anchorSection(userNumber)}
    ${devicesSection({
      authenticators,
      onAddDevice,
      maxAuthenticators: MAX_AUTHENTICATORS,
    })}
    ${recoveries.length === 0 ? recoveryNag({ onAddRecovery }) : undefined}
    ${recoveryMethods({ recoveries, onAddRecovery })} ${logoutSection()}
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

// The regular, "authenticator" devices

const recoveryNag = ({ onAddRecovery }: { onAddRecovery: () => void }) =>
  warnBox({
    title: "Recovery method",
    message: "Add a recovery method to help protect this Identity Anchor.",
    additionalClasses: ["l-stack"],
    slot: html`<button
      @click="${onAddRecovery}"
      id="addRecovery"
      class="c-button"
    >
      Add Recovery
    </button>`,
  });

// Get the list of devices from canister and actually display the page
export const renderManage = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> => {
  let anchorInfo: IdentityAnchorInfo;
  try {
    anchorInfo = await withLoader(() => connection.getAnchorInfo());
  } catch (error: unknown) {
    await displayFailedToListDevices(
      error instanceof Error ? error : unknownError()
    );
    return renderManage(userNumber, connection);
  }
  if (anchorInfo.device_registration.length !== 0) {
    // we are actually in a device registration process
    await pollForTentativeDevice(userNumber, connection);
  } else {
    displayManage(userNumber, connection, anchorInfo.devices);
  }
};

export const displayManagePage = (
  props: Parameters<typeof displayManageTemplate>[0],
  container?: HTMLElement
): void => {
  const contain =
    container ?? (document.getElementById("pageContent") as HTMLElement);
  render(displayManageTemplate(props), contain);
};

export const displayManage = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices: DeviceData[]
): void => {
  const hasSingleDevice = devices.length <= 1;

  const _devices: Device[] = devices.map((device) => {
    return {
      settings: deviceSettings({
        userNumber,
        connection,
        device,
        isOnlyDevice: hasSingleDevice,
        reload: () => renderManage(userNumber, connection),
      }),
      label: isRecoveryDevice(device)
        ? recoveryDeviceToLabel(device)
        : device.alias,
      isRecovery: isRecoveryDevice(device),
      isProtected: isProtected(device),
      warn: domainWarning(device),
    };
  });

  displayManagePage({
    userNumber,
    authenticators: _devices.filter((device) => !device.isRecovery),
    recoveries: _devices.filter((device) => device.isRecovery),
    onAddDevice: async () => {
      const nextAction = await chooseDeviceAddFlow();
      switch (nextAction) {
        case "canceled": {
          await renderManage(userNumber, connection);
          break;
        }
        case "local": {
          await addLocalDevice(userNumber, connection, devices);
          return;
        }
        case "remote": {
          await pollForTentativeDevice(userNumber, connection);
          return;
        }
        default:
          unreachable(nextAction);
          break;
      }
    },
    onAddRecovery: async () => {
      await addNewRecovery(userNumber, connection);
      renderManage(userNumber, connection);
    },
  });

  // When visiting the legacy URL (ic0.app) we extra-nudge the users to create a recovery phrase,
  // if they don't have one already. We lead them straight to recovery phrase creation, because
  // recovery _device_ would be tied to the domain (which we want to avoid).
  if (window.location.origin === LEGACY_II_URL && !hasRecoveryPhrase(devices)) {
    const elem = showWarning(html`<strong class="t-strong">Important!</strong>
      Create a recovery phrase.
      <button
        class="features-warning-btn"
        @click=${async () => {
          await setupPhrase(userNumber, connection);
          elem.remove();
          renderManage(userNumber, connection);
        }}
      >
        Create
      </button> `);
  }
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

const addNewRecovery = (
  userNumber: bigint,
  connection: AuthenticatedConnection
) =>
  setupRecovery({
    userNumber,
    connection,
    title: html`Add a Recovery Method`,
    message: html`Use a FIDO device or connected phone to create an additional
    recovery method.`,
    cancelText: html`Back`,
  });
