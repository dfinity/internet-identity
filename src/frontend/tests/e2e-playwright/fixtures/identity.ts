import { Page, test as base, expect } from "@playwright/test";
import { dummyAuth, DummyAuthFn, getRandomIndex, II_URL } from "../utils";
import { Actor, type ActorSubclass, HttpAgent } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { Agent } from "undici";
import { Principal } from "@icp-sdk/core/principal";

// Fetch that's compatible with Vite server's self-signed certs
const insecureAgent = new Agent({
  connect: {
    rejectUnauthorized: false,
  },
});
const insecureFetch: typeof fetch = (url, options = {}) =>
  fetch(url, {
    dispatcher: insecureAgent,
    ...options,
  } as RequestInit);

export const DEFAULT_HOST = "https://localhost:5173"; // Vite dev server
export const DEFAULT_NAME = "Test User";

export type IdentityConfig = {
  host?: string;
  createIdentities: Array<{
    name: string;
    generateDummyAuthIndex?: () => bigint;
  }>;
};

interface Identity {
  canisterId: Principal;
  identityNumber: bigint;
  name: string;
  dummyAuthIndex: bigint;
}

class IdentityWizard {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async signIn(auth: DummyAuthFn): Promise<void> {
    await this.#goto();
    auth(this.#page);
    const dialog = this.#page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await dialog.getByRole("button", { name: "Use existing identity" }).click();
    await expect(dialog).toBeHidden();
  }

  async signUp(auth: DummyAuthFn, name: string): Promise<void> {
    await this.#goto();
    const dialog = this.#page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await dialog.getByRole("button", { name: "Create new identity" }).click();
    await this.#page.getByLabel("Identity name").fill(name);
    auth(this.#page);
    await this.#page.getByRole("button", { name: "Create identity" }).click();
    await expect(dialog).toBeHidden();
  }

  /**
   * Used to navigate to identity sign in/up view, attempts to click a sequence
   * of buttons in order (if visible) to get to the intended view from any page.
   */
  async #goto(): Promise<void> {
    // Wait for any of the buttons we need to click to show up
    const buttons = [
      "Sign in",
      "Switch identity",
      "Use another identity",
      "Continue with passkey",
    ];
    await this.#page
      .getByRole("button", { name: new RegExp(buttons.join("|")) })
      .first()
      .waitFor();
    // If continue with passkey button is already visible,
    // e.g. after /login redirect we only need to click that.
    const continueWithPasskeyButton = this.#page.getByRole("button", {
      name: "Continue with passkey",
    });
    if (await continueWithPasskeyButton.isVisible()) {
      await continueWithPasskeyButton.click();
      return;
    }
    // Else click all buttons in order (if visible)
    for (const button of buttons) {
      const locator = this.#page.getByRole("button", { name: button });
      if (await locator.isVisible()) {
        await locator.click();
      }
    }
  }
}

export const toSeed = (dummyAuthIndex: bigint): Uint8Array => {
  // Same implementation as found in `DiscoverableDummyIdentity`
  const buffer = new ArrayBuffer(32);
  const view = new DataView(buffer);
  view.setBigUint64(0, dummyAuthIndex);
  return new Uint8Array(buffer);
};

const createActor = async (
  host: string,
  canisterId: Principal,
  dummyAuthIndex: bigint,
): Promise<ActorSubclass<_SERVICE>> =>
  Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent: await HttpAgent.create({
      host,
      shouldFetchRootKey: true,
      verifyQuerySignatures: false,
      fetch: insecureFetch,
      identity: Ed25519KeyIdentity.generate(toSeed(dummyAuthIndex)),
    }),
    canisterId,
  });

export const test = base.extend<{
  identityConfig: IdentityConfig;
  identities: Identity[];
  signInWithIdentity: (page: Page, identityNumber: bigint) => Promise<void>;
  actorForIdentity: (
    identityNumber: bigint,
  ) => Promise<ActorSubclass<_SERVICE>>;
  replaceAuthForIdentity: (
    identityNumber: bigint,
    newDummyAuthIndex: bigint,
  ) => void;
}>({
  identityConfig: {
    createIdentities: [
      {
        name: DEFAULT_NAME,
        generateDummyAuthIndex: getRandomIndex,
      },
    ],
  },
  identities: async ({ browser, identityConfig }, use) =>
    use(
      await Promise.all(
        identityConfig.createIdentities.map(
          async ({ name, generateDummyAuthIndex = getRandomIndex }) => {
            const dummyAuthIndex = generateDummyAuthIndex();
            const context = await browser.newContext();
            const page = await context.newPage();
            await page.goto(II_URL);
            const element = (await page.$("[data-canister-id]"))!;
            const canisterId = Principal.fromText(
              (await element.getAttribute("data-canister-id"))!,
            );
            const wizard = new IdentityWizard(page);
            await wizard.signUp(dummyAuth(dummyAuthIndex), name);
            await page.close();
            await context.close();
            const actor = await createActor(
              identityConfig.host ?? DEFAULT_HOST,
              canisterId,
              dummyAuthIndex,
            );
            const [deviceKeyWithAnchor] = await actor.lookup_device_key(
              toSeed(dummyAuthIndex),
            );
            const { anchor_number: identityNumber } = deviceKeyWithAnchor!;
            return {
              canisterId,
              name,
              dummyAuthIndex,
              identityNumber,
            };
          },
        ),
      ),
    ),
  signInWithIdentity: ({ identities }, use) =>
    use(async (page: Page, identityNumber: bigint) => {
      const identity = identities.find(
        (identity) => identity.identityNumber === identityNumber,
      );
      if (identity === undefined) {
        throw new Error("Identity not found");
      }
      const wizard = new IdentityWizard(page);
      await wizard.signIn(dummyAuth(identity.dummyAuthIndex));
    }),
  actorForIdentity: ({ identityConfig, identities }, use) =>
    use((identityNumber: bigint) => {
      const identity = identities.find(
        (identity) => identity.identityNumber === identityNumber,
      );
      if (identity === undefined) {
        throw new Error("Identity not found");
      }
      return createActor(
        identityConfig.host ?? DEFAULT_HOST,
        identity.canisterId,
        identity.dummyAuthIndex,
      );
    }),
  replaceAuthForIdentity: ({ identities }, use) =>
    use((identityNumber: bigint, newDummyAuthIndex: bigint) => {
      const identity = identities.find(
        (identity) => identity.identityNumber === identityNumber,
      );
      if (identity === undefined) {
        throw new Error("Identity not found");
      }
      identity.dummyAuthIndex = newDummyAuthIndex;
    }),
});
