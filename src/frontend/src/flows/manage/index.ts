import { TemplateResult, render, html } from "lit-html";
import { Connection, AuthenticatedConnection } from "../../utils/iiConnection";
import { withLoader } from "../../components/loader";
import { unreachable } from "../../utils/utils";
import { logoutSection } from "../../components/logout";
import { deviceSettings } from "./deviceSettings";
import {
  DeviceData,
  IdentityAnchorInfo,
} from "../../../generated/internet_identity_types";
import { settingsIcon, warningIcon } from "../../components/icons";
import { displayError } from "../../components/displayError";
import {
  authenticateBox,
  AuthnTemplates,
} from "../../components/authenticateBox";
import { setupRecovery } from "../recovery/setupRecovery";
import { recoveryWizard } from "../recovery/recoveryWizard";
import { pollForTentativeDevice } from "../addDevice/manage/pollForTentativeDevice";
import { chooseDeviceAddFlow } from "../addDevice/manage";
import { addLocalDevice } from "../addDevice/manage/addLocalDevice";
import { warnBox } from "../../components/warnBox";
import { mainWindow } from "../../components/mainWindow";

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
const numAuthenticators = (devices: DeviceData[]) =>
  devices.filter((device) => "authentication" in device.purpose).length;

// Actual page content. We display the Identity Anchor and the list of
// (non-recovery) devices. Additionally, if the user does _not_ have any
// recovery devices, we display a warning "nag box" and suggest to the user
// that they add a recovery device. If the user _does_ have at least one
// recovery device, then we do not display a "nag box", but we list the
// recovery devices.
const pageContent = ({
  userNumber,
  devices,
  onAddDevice,
  onAddRecovery,
}: {
  userNumber: bigint;
  devices: DeviceData[];
  onAddDevice: (next: "canceled" | "local" | "remote") => void;
  onAddRecovery: () => void;
}): TemplateResult => {
  const pageContentSlot = html` <section>
    <hgroup>
      <h1 class="t-title t-title--main">Manage your Anchor</h1>
      <p class="t-lead">
        Add devices and recovery methods to make your anchor more secure.
      </p>
    </hgroup>
    ${anchorSection(userNumber)} ${devicesSection(devices, onAddDevice)}
    ${!hasRecoveryDevice(devices) ? recoveryNag({ onAddRecovery }) : undefined}
    ${recoverySection(devices, onAddRecovery)} ${logoutSection()}
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

const devicesSection = (
  devices: DeviceData[],
  onAddDevice: (next: "canceled" | "local" | "remote") => void
): TemplateResult => {
  const wrapClasses = ["l-stack"];
  const isWarning = devices.length < 2;

  if (isWarning === true) {
    wrapClasses.push("c-card", "c-card--narrow", "c-card--warning");
  }

  return html`
    <aside class="${wrapClasses.join(" ")}">
      ${
        isWarning === true
          ? html`<span class="c-card__icon" aria-hidden="true"
              >${warningIcon}</span
            >`
          : undefined
      }
      <div class="${isWarning === true ? "c-card__content" : undefined}">
        <div class="t-title t-title--complications">
          <h2 class="t-title">Added devices</h2>
          <span class="t-title__complication c-tooltip">
            <span class="c-tooltip__message c-card c-card--narrow">
              You can register up to ${MAX_AUTHENTICATORS} authenticator
              devices (recovery devices excluded)</span>
              (${numAuthenticators(devices)}/${MAX_AUTHENTICATORS})
            </span>
          </span>
        </div>
        ${
          isWarning === true
            ? html`<p class="warning-message t-paragraph t-lead">
                We recommend that you have at least two devices (for example,
                your computer and your phone).
              </p>`
            : undefined
        }

        <div class="c-action-list">
          <div id="deviceList"></div>
          <div class="c-action-list__actions">
            <button 
              ?disabled=${numAuthenticators(devices) >= MAX_AUTHENTICATORS}
              class="c-button c-button--primary c-tooltip c-tooltip--onDisabled"
              @click="${async () => onAddDevice(await chooseDeviceAddFlow())}"
              id="addAdditionalDevice"
            >
              <span class="c-tooltip__message c-tooltip__message--right c-card c-card--narrow"
                >You can register up to ${MAX_AUTHENTICATORS} authenticator devices.
                Remove a device before you can add a new one.</span
              >
              <span>Add new device</span>
            </button>
          </div>

        </div>
      </div>
    </aside>`;
};

const recoverySection = (
  devices: DeviceData[],
  onAddRecovery: () => void
): TemplateResult => {
  return html`
    <aside class="l-stack">
      ${!hasRecoveryDevice(devices)
        ? undefined
        : html`
            <div class="t-title">
              <h2>Recovery methods</h2>
            </div>
            <div class="c-action-list">
              <div id="recoveryList"></div>
              <div class="c-action-list__actions">
                <button
                  @click="${onAddRecovery}"
                  class="c-button c-button--primary"
                  id="addRecovery"
                >
                  Add recovery method
                </button>
              </div>
            </div>
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

export const displayManage = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices: DeviceData[]
): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  const template = pageContent({
    userNumber,
    devices,
    onAddDevice: async (nextAction) => {
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
      await setupRecovery(userNumber, connection);
      renderManage(userNumber, connection);
    },
  });
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
    "recovery" in device.purpose
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
  devices.some((device) => "recovery" in device.purpose);

const unknownError = (): Error => {
  return new Error("Unknown error");
};
