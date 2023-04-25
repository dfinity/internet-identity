import { DeviceData } from "$generated/internet_identity_types";
import { spinner } from "$root/components/icons";
import { mainWindow } from "$root/components/mainWindow";
import { features } from "$root/features";
import {
  creationOptions,
  DummyIdentity,
  IIWebAuthnIdentity,
} from "$root/utils/iiConnection";
import { WebAuthnIdentity } from "@dfinity/identity";
import { isNullish } from "@dfinity/utils";
import { html, render } from "lit-html";
import { nextTick } from "process";

/* Anchor construction component (for creating WebAuthn credentials) */

const constructingContentSlot = ({
  message,
}: {
  message?: string;
}) => html` <div class="c-spinner-wrapper">
    <div class="c-spinner">${spinner}</div>
  </div>
  <p class="t-lead t-paragraph l-stack">
    ${message ?? "Creating your Identity Anchor."}
  </p>
  <p><strong class="t-strong">Do not refresh the page</strong></p>`;

const constructingContent = (props: { message?: string }) =>
  mainWindow({
    additionalContainerClasses: ["t-centered"],
    showFooter: false,
    showLogo: false,
    slot: constructingContentSlot(props),
  });

export const renderConstructing = (props: { message?: string }): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(constructingContent(props), container);
};

export const constructIdentity = async ({
  devices,
  message,
}: {
  devices?: () => Promise<Array<DeviceData>>;
  message?: string;
}): Promise<IIWebAuthnIdentity> => {
  renderConstructing({ message });
  await tick();

  const opts = isNullish(devices)
    ? creationOptions()
    : creationOptions(await devices());

  /* The Identity (i.e. key pair) used when creating the anchor.
   * If the "DUMMY_AUTH" feature is set, we create a dummy identity. The same identity must then be used in iiConnection when authenticating.
   */
  const createIdentity = features.DUMMY_AUTH
    ? () => Promise.resolve(new DummyIdentity())
    : () => WebAuthnIdentity.create({ publicKey: opts });

  return createIdentity();
};

const tick = (): Promise<void> => new Promise((resolve) => nextTick(resolve));
