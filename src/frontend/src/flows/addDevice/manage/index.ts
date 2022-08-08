import { html, render } from "lit-html";
import {
  networkIcon,
  securityKeyIcon,
  warningIcon,
} from "../../../components/icons";

const pageContent = () => html`
  <article class="l-container c-card c-card--highlight">
    <h1 class="t-title">Add New Device</h1>
    <aside class="c-card c-card--warning c-card--icon">
      <div class="c-card__icon">${warningIcon}</div>
      <div class="c-card__content">
        <h2 class="t-title">Security Warning</h2>
        <p class="t-paragraph">
          You are in the process of adding a new device. Any device added here
          will have <strong class="t-strong">full control over your identity</strong>. Only
          continue the process if you want to add a new device that you
          <em>personally own</em>.
        </p>
      </div>
    </aside>
    <aside class="c-card c-card--warning c-card--icon">
      <div class="c-card__icon">${warningIcon}</div>
      <div class="c-card__content">
        <h2 class="t-title">Security Warning</h2>
        <p class="t-paragraph">
          Do not continue if you were prompted to do this by any website other
          than <strong class="t-strong">https://identity.ic0.app</strong>!
        </p>
      </div>
    </aside>
    <p class="t-paragraph">
      Is the device you want to add available on this machine (local device) or
      on a different one (remote device)?
    </p>
    <div class="l-horizontal l-section">
      <button class="c-button c-button--secondary" id="local">
        <span class="flowIcon">${securityKeyIcon}</span>
        <div class="t-strong">Local Device</div>
        <div class="t-weak">
          Add a new device available on <em>this machine</em>.
        </div>
      </button>
      <button class="c-button c-button--secondary" id="remote">
        <span class="flowIcon">${networkIcon}</span>
        <div class="t-strong">Remote Device</div>
        <div class="t-weak">
          Add a new device available on <em>another machine</em>.
        </div>
      </button>
    </div>
    <div class="l-section">
      <button id="cancelAddDevice" class="c-button">Cancel</button>
    </div>
    
  </article>
`;

export type DeviceAddFlow = "local" | "remote";

/**
 * Entry point for the flow of adding a new authenticator when starting form the management view (by clicking 'add new device' there).
 * This file is responsible for the next view shown, which is giving the user the choice of
 * - adding a local authenticator (e.g. a Yubikey attached to the computer) -> if chosen flow is continued with {@link addLocalDevice}
 * - adding a new remote device (e.g. a different computer with platform biometrics) -> if chosen flow is continued with {@link pollForTentativeDevice}
 */
export const chooseDeviceAddFlow = async (): Promise<DeviceAddFlow | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init();
};

const init = (): Promise<DeviceAddFlow | null> =>
  new Promise((resolve) => {
    const localDeviceFlow = document.getElementById(
      "local"
    ) as HTMLButtonElement;
    const remoteDeviceFlow = document.getElementById(
      "remote"
    ) as HTMLButtonElement;
    const cancel = document.getElementById(
      "cancelAddDevice"
    ) as HTMLButtonElement;
    localDeviceFlow.onclick = () => resolve("local");
    remoteDeviceFlow.onclick = () => resolve("remote");
    cancel.onclick = () => resolve(null);
  });
