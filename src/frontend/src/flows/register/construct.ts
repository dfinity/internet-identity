import { WebAuthnIdentity } from "@dfinity/identity";
import { html, render } from "lit-html";
import {
  IdentifiableIdentity,
  DummyIdentity,
  creationOptions,
} from "../../utils/iiConnection";
import { nextTick } from "process";
import { icLogo } from "../../components/icons";

/* Anchor construction component (for creating WebAuthn credentials) */

const constructingContent = html`
  <div class="l-container c-card c-card--highlight">
    <div class="c-logo">${icLogo}</div>
    <p class="t-lead t-centered t-paragraph l-stack">
      Creating your Identity Anchor. Do not refresh the page.
    </p>
  </div>
`;

export const renderConstructing = (): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(constructingContent, container);
};

export const constructIdentity = async (): Promise<IdentifiableIdentity> => {
  renderConstructing();
  await tick();

  /* The Identity (i.e. key pair) used when creating the anchor.
   * If "II_DUMMY_AUTH" is set, we create a dummy identity. The same identity must then be used in iiConnection when authenticating.
   */
  const createIdentity =
    process.env.II_DUMMY_AUTH === "1"
      ? () => Promise.resolve(new DummyIdentity())
      : () =>
          WebAuthnIdentity.create({
            publicKey: creationOptions(),
          });

  return createIdentity();
};

const tick = (): Promise<void> => new Promise((resolve) => nextTick(resolve));
