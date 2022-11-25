import { TemplateResult, render, html } from "lit-html";
import { Connection, AuthenticatedConnection } from "../../utils/iiConnection";
import { withLoader } from "../../components/loader";
import { unreachable } from "../../utils/utils";
import { logoutSection } from "../../components/logout";
import { footer } from "../../components/footer";
import { deviceSettings } from "./deviceSettings";
import {
  DeviceData,
  IdentityAnchorInfo,
} from "../../../generated/internet_identity_types";
import { settingsIcon } from "../../components/icons";
import { displayError } from "../../components/displayError";
import {
  authenticateBox,
  AuthnTemplates,
} from "../../components/authenticateBox";
import { setupRecovery } from "../recovery/setupRecovery";
import { recoveryWizard } from "../recovery/recoveryWizard";
import { hasOwnProperty } from "../../utils/utils";
import { pollForTentativeDevice } from "../addDevice/manage/pollForTentativeDevice";
import { chooseDeviceAddFlow } from "../addDevice/manage";
import { addLocalDevice } from "../addDevice/manage/addLocalDevice";
import { warnBox } from "../../components/warnBox";

/* Template for the authbox when authenticating to II */
export const authnTemplateManage = (): AuthnTemplates => {
  const wrap = (slot: string): TemplateResult => html`
    <div class="t-centered l-stack">${slot}</div>
  `;
  return {
    firstTime: {
      slot: wrap(`Anonymously connect to dapps on the Internet Computer`),
      useExistingText: "Manage Existing",
      createAnchorText: "Create Identity Anchor",
    },
    useExisting: {
      slot: wrap(`Enter your Anchor to continue to Internet Identity`),
    },

    pick: { slot: wrap("Choose an Anchor") },
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
const numAuthenticators = (devices: DeviceData[]) =>
  devices.filter((device) => hasOwnProperty(device.purpose, "authentication"))
    .length;

// Actual page content. We display the Identity Anchor and the list of
// (non-recovery) devices. Additionally, if the user does _not_ have any
// recovery devices, we display a warning "nag box" and suggest to the user
// that they add a recovery device. If the user _does_ have at least one
// recovery device, then we do not display a "nag box", but we list the
// recovery devices.
const pageContent = (
  userNumber: bigint,
  devices: DeviceData[],
  onAddDevice: (next: "canceled" | "local" | "remote") => void,
  onAddRecovery: () => void
): TemplateResult => html`
  <section class="l-container c-card c-card--highlight">
    <hgroup>
      <h1 class="t-title t-title--main">Anchor Management</h1>
      <p class="t-lead">
        You can view and manage this Identity Anchor and its added devices here.
      </p>
    </hgroup>
    ${!hasRecoveryDevice(devices) ? recoveryNag({ onAddRecovery }) : undefined}
    ${anchorSection(userNumber)} ${devicesSection(devices, onAddDevice)}
    ${recoverySection(devices, onAddRecovery)} ${logoutSection()}
  </section>
  ${footer}
`;

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

const devicesSection = (
  devices: DeviceData[],
  onAddDevice: (next: "canceled" | "local" | "remote") => void
): TemplateResult => html`
    <aside class="l-stack">
      <div class="t-title t-title--complications">
        <h2 class="t-title">Added devices</h2>
        <span class="t-title__complication c-tooltip">
          <span class="c-tooltip__message c-card c-card--narrow">
            You can register up to ${MAX_AUTHENTICATORS} authenticator
            devices (recovery devices excluded)</span>
            (${numAuthenticators(devices)}/${MAX_AUTHENTICATORS})
          </span>
        </span>
        <button
          ?disabled=${numAuthenticators(devices) >= MAX_AUTHENTICATORS}
          class="t-title__complication t-title__complication--end c-tooltip c-tooltip--onDisabled"
          @click="${async () => onAddDevice(await chooseDeviceAddFlow())}"
          id="addAdditionalDevice"
        >
          <span class="c-tooltip__message c-tooltip__message--right c-card c-card--narrow"
            >You can register up to ${MAX_AUTHENTICATORS} authenticator devices.
            Remove a device before you can add a new one.</span
          >
          <span class="t-link t-link--discreet">
            <i class="t-link__icon" aria-hidden=true>+</i>
            Add new device
          </span>
        </button>
      </div>
      <div id="deviceList" class="c-action-list"></div>
    </aside>
`;

const recoverySection = (
  devices: DeviceData[],
  onAddRecovery: () => void
): TemplateResult => {
  return html`
    <aside class="l-stack">
      ${!hasRecoveryDevice(devices)
        ? undefined
        : html`
            <div class="t-title t-title--complications">
              <h2>Recovery mechanisms</h2>
              <button
                @click="${onAddRecovery}"
                class="t-title__complication t-title__complication--end"
                id="addRecovery"
              >
                <i class="t-link__icon" aria-hidden="true">+</i>
                <span class="t-link t-link--discreet"
                  >Add recovery mechanism</span
                >
              </button>
            </div>
            <div id="recoveryList" class="c-action-list"></div>
          `}
    </aside>
  `;
};

const deviceListItem = (device: DeviceData) => html`
  <div class="c-action-list__label">${device.alias}</div>
  <button
    type="button"
    aria-label="settings"
    data-action="settings"
    class="c-action-list__action"
  >
    ${settingsIcon}
  </button>
`;

const recoveryNag = ({ onAddRecovery }: { onAddRecovery: () => void }) =>
  warnBox({
    title: "Recovery Mechanism",
    message: "Add a recovery mechanism to help protect this Identity Anchor.",
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

export const displayManage = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices: DeviceData[]
): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  const template = pageContent(
    userNumber,
    devices,
    async (nextAction) => {
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
    async () => {
      await setupRecovery(userNumber, connection);
      renderManage(userNumber, connection);
    }
  );
  render(template, container);
  renderDevices(userNumber, connection, devices);
};

const renderDevices = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices: DeviceData[]
) => {
  const list = document.createElement("ul");
  const recoveryList = document.createElement("ul");
  const isOnlyDevice = devices.length < 2;

  devices.forEach((device) => {
    const identityElement = document.createElement("li");
    identityElement.className = "c-action-list__item";

    render(deviceListItem(device), identityElement);
    const buttonSettings = identityElement.querySelector(
      "button[data-action=settings]"
    ) as HTMLButtonElement;
    if (buttonSettings !== null) {
      buttonSettings.onclick = async () => {
        await deviceSettings(
          userNumber,
          connection,
          device,
          isOnlyDevice
        ).catch((e) =>
          displayError({
            title: "Could not edit device",
            message: "An error happened on the settings page.",
            detail: e.toString(),
            primaryButton: "Ok",
          })
        );
        await renderManage(userNumber, connection);
      };
    }
    hasOwnProperty(device.purpose, "recovery")
      ? recoveryList.appendChild(identityElement)
      : list.appendChild(identityElement);
  });
  const deviceList = document.getElementById("deviceList") as HTMLElement;
  deviceList.innerHTML = ``;
  deviceList.appendChild(list);

  const recoveryDevices = document.getElementById(
    "recoveryList"
  ) as HTMLElement;

  if (recoveryDevices !== null) {
    recoveryDevices.innerHTML = ``;
    recoveryDevices.appendChild(recoveryList);
  }
};

// Whether or the user has registered a device as recovery
const hasRecoveryDevice = (devices: DeviceData[]): boolean =>
  devices.some((device) => hasOwnProperty(device.purpose, "recovery"));

const unknownError = (): Error => {
  return new Error("Unknown error");
};
