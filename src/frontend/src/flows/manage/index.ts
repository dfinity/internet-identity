import { TemplateResult, render, html } from "lit-html";
import { LEGACY_II_URL } from "../../config";
import { Connection, AuthenticatedConnection } from "../../utils/iiConnection";
import { withLoader } from "../../components/loader";
import { unreachable, unknownToString } from "../../utils/utils";
import { logoutSection } from "../../components/logout";
import { deviceSettings } from "./deviceSettings";
import { showWarning } from "../../banner";
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
import { setupRecovery, setupPhrase } from "../recovery/setupRecovery";
import { recoveryWizard } from "../recovery/recoveryWizard";
import { pollForTentativeDevice } from "../addDevice/manage/pollForTentativeDevice";
import { chooseDeviceAddFlow } from "../addDevice/manage";
import { addLocalDevice } from "../addDevice/manage/addLocalDevice";
import { warnBox } from "../../components/warnBox";
import { mainWindow } from "../../components/mainWindow";
import {
  isRecoveryDevice,
  recoveryDeviceToLabel,
} from "../../utils/recoveryDevice";

// A simple representation of "device"s used on the manage page.
export type Device = {
  // Open the settings screen for that particular device
  openSettings: () => Promise<void>;
  // The displayed name of a device (not exactly the "alias") because
  // recovery devices handle aliases differently.
  label: string;
  isRecovery: boolean;
  warn?: TemplateResult;
};

export type DedupDevice = Device & { dupCount?: number };

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
    ${anchorSection(userNumber)}
    ${devicesSection({ authenticators, onAddDevice })}
    ${recoveries.length === 0 ? recoveryNag({ onAddRecovery }) : undefined}
    ${recoverySection({ recoveries, onAddRecovery })} ${logoutSection()}
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

const dedupLabels = (authenticators: Device[]): DedupDevice[] => {
  return authenticators.reduce<Device[]>((acc, authenticator) => {
    const _authenticator: DedupDevice = { ...authenticator };
    const sameName = acc.filter((a) => a.label === _authenticator.label);
    if (sameName.length >= 1) {
      _authenticator.dupCount = sameName.length + 1;
    }

    acc.push(_authenticator);
    return acc;
  }, []);
};

// The regular, "authenticator" devices
const devicesSection = ({
  authenticators,
  onAddDevice,
}: {
  authenticators: Device[];
  onAddDevice: (next: "canceled" | "local" | "remote") => void;
}): TemplateResult => {
  const wrapClasses = ["l-stack"];
  const isWarning = authenticators.length < 2;

  if (isWarning === true) {
    wrapClasses.push("c-card", "c-card--narrow", "c-card--warning");
  }

  const _authenticators = dedupLabels(authenticators);

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
          <span class="t-title__complication c-tooltip" tabindex="0">
            <span class="c-tooltip__message c-card c-card--narrow">
              You can register up to ${MAX_AUTHENTICATORS} authenticator
              devices (recovery devices excluded)</span>
              (${_authenticators.length}/${MAX_AUTHENTICATORS})
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
          <div id="deviceList">
          <ul>
          ${_authenticators.map((device) => {
            return html`
              <li class="c-action-list__item">
                ${deviceListItem({
                  device,
                })}
              </li>
            `;
          })}</ul>
          </div>
          <div class="c-action-list__actions">
            <button
              ?disabled=${_authenticators.length >= MAX_AUTHENTICATORS}
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

// The list of recovery devices
const recoverySection = ({
  recoveries,
  onAddRecovery,
}: {
  recoveries: Device[];
  onAddRecovery: () => void;
}): TemplateResult => {
  return html`
    <aside class="l-stack">
      ${recoveries.length === 0
        ? undefined
        : html`
            <div class="t-title">
              <h2>Recovery methods</h2>
            </div>
            <div class="c-action-list">
              <div id="recoveryList">
                <ul>
                  ${recoveries.map(
                    (device) =>
                      html`
                        <li class="c-action-list__item">
                          ${deviceListItem({ device })}
                        </li>
                      `
                  )}
                </ul>
              </div>
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

const deviceListItem = ({ device }: { device: DedupDevice }) => {
  return html`
    <div class="c-action-list__label">
      ${device.label}
      ${device.dupCount
        ? html`<i class="t-muted">&nbsp;(${device.dupCount})</i>`
        : undefined}
    </div>
    ${device.warn !== undefined
      ? html`<div class="c-action-list__action">
          <span class="c-tooltip c-icon c-icon--warning" tabindex="0"
            >${warningIcon}<span
              class="c-tooltip__message c-card c-card--narrow"
              >${device.warn}</span
            ></span
          >
        </div>`
      : undefined}
    <button
      type="button"
      aria-label="settings"
      data-action="settings"
      class="c-action-list__action"
      @click=${() => device.openSettings()}
    >
      ${settingsIcon}
    </button>
  `;
};

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

  const _devices = devices.map((device) => ({
    openSettings: async () => {
      try {
        await deviceSettings(userNumber, connection, device, hasSingleDevice);
      } catch (e: unknown) {
        await displayError({
          title: "Could not edit device",
          message: "An error happened on the settings page.",
          detail: unknownToString(e, "unknown error"),
          primaryButton: "Ok",
        });
      }

      await renderManage(userNumber, connection);
    },
    label: isRecoveryDevice(device)
      ? recoveryDeviceToLabel(device)
      : device.alias,
    isRecovery: isRecoveryDevice(device),
    warn:
      device.origin.length === 1 &&
      device.origin[0] !== window.origin &&
      "platform" in device.key_type
        ? html`This device was registered on another domain
          $(${device.origin[0]}).`
        : undefined,
  }));

  displayManagePage({
    userNumber,
    authenticators: _devices.filter((device) => !device.isRecovery),
    recoveries: _devices.filter((device) => device.isRecovery),
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

// Whether the user has a recovery phrase or not
const hasRecoveryPhrase = (devices: DeviceData[]): boolean =>
  devices.some((device) => device.alias === "Recovery phrase");

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
