import { WebAuthnIdentity } from "@dfinity/identity";
import { html, render } from "lit-html";
import {
  IdentifiableIdentity,
  DummyIdentity,
  creationOptions,
  AuthenticatedConnection,
  Connection,
} from "../../utils/iiConnection";
import { nextTick } from "process";
import { spinner } from "../../components/icons";

/* Anchor construction component (for creating WebAuthn credentials) */

const constructingContent = html`
  <div class="l-container c-card c-card--highlight t-centered">
    <div class="c-spinner">${spinner}</div>
    <p class="t-lead t-paragraph l-stack">Creating your Identity Anchor.</p>
    <p><strong class="t-strong">Do not refresh the page</strong></p>
  </div>
`;

export const renderConstructing = (): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(constructingContent, container);
};

export const constructIdentity = async (
  connection: Connection
): Promise<[AuthenticatedConnection, IdentifiableIdentity]> => {
  renderConstructing();
  await tick();

  const [delegationIdentity, webauthnIdentity] =
    await connection.createFEDelegation();

  return [
    new AuthenticatedConnection(
      connection.canisterId,
      webauthnIdentity,
      delegationIdentity,
      BigInt(0),
      await connection.createActor(delegationIdentity)
    ),
    webauthnIdentity,
  ];
};

const tick = (): Promise<void> => new Promise((resolve) => nextTick(resolve));
