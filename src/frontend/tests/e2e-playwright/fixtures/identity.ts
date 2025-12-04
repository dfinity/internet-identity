import { Page, test as base } from "@playwright/test";
import { dummyAuth, DummyAuthFn, getRandomIndex, II_URL } from "../utils";
import { Actor, type ActorSubclass, HttpAgent } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { Agent } from "undici";

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

interface Identity {
  name: string;

  signIn(): Promise<void>;

  signOut(): Promise<void>;

  createActor(): Promise<{
    actor: ActorSubclass<_SERVICE>;
    identityNumber: bigint;
  }>;
}

const DEFAULT_NAME = "Test User";

class IdentityWizard {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async signInWithPasskey(auth: DummyAuthFn): Promise<void> {
    await this.#goto();
    await this.#page
      .getByRole("button", { name: "Continue with passkey" })
      .click();
    auth(this.#page);
    await this.#page
      .getByRole("button", { name: "Use existing identity" })
      .click();
    await this.#page.waitForURL((url) => url.pathname.startsWith("/manage"));
  }

  async signUpWithPasskey(auth: DummyAuthFn, name: string): Promise<void> {
    await this.#goto();
    await this.#page
      .getByRole("button", { name: "Continue with passkey" })
      .click();
    await this.#page
      .getByRole("button", { name: "Create new identity" })
      .click();
    await this.#page.getByLabel("Identity name").fill(name);
    auth(this.#page);
    await this.#page.getByRole("button", { name: "Create identity" }).click();
    await this.#page.waitForURL((url) => url.pathname.startsWith("/manage"));
  }

  /**
   * Used to navigate to identity sign in/up view, attempts to click a sequence
   * of buttons in order (if visible) to get to the intended view from any page.
   */
  async #goto(): Promise<void> {
    const buttons = [
      this.#page.getByRole("button", { name: "Manage identity" }),
      this.#page.getByRole("button", { name: "Switch identity" }),
      this.#page.getByRole("button", { name: "Use another identity" }),
    ];
    for (const button of buttons) {
      if (await button.isVisible()) {
        await button.click();
      }
    }
  }
}

export const test = base.extend<{
  identity: Identity;
}>({
  identity: async ({ page, browser }, use) => {
    const authIndex = getRandomIndex();
    const auth = dummyAuth(authIndex);
    const tempContext = await browser.newContext();
    const tempPage = await tempContext.newPage();
    await tempPage.goto(II_URL + "/login");
    const script = (await tempPage.$("[data-canister-id]"))!;
    const canisterId = (await script.getAttribute("data-canister-id"))!;
    const tempWizard = new IdentityWizard(tempPage);
    await tempWizard.signUpWithPasskey(auth, DEFAULT_NAME);
    await tempPage.waitForURL(II_URL + "/manage");
    await tempPage.close();
    await use({
      name: DEFAULT_NAME,
      signIn: async () => {
        const wizard = new IdentityWizard(page);
        await wizard.signInWithPasskey(auth);
      },
      signOut: async () => {
        // Navigating to landing page (reloading) is sufficient for now to sign out
        await page.goto(II_URL);
      },
      createActor: async () => {
        // Same implementation as found in `DiscoverableDummyIdentity`
        const buffer = new ArrayBuffer(32);
        const view = new DataView(buffer);
        view.setBigUint64(0, authIndex);
        const seed = new Uint8Array(buffer);
        const agent = await HttpAgent.create({
          host: "https://localhost:5173", // Vite dev server
          shouldFetchRootKey: true,
          verifyQuerySignatures: false,
          fetch: insecureFetch,
        });
        const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
          agent,
          canisterId,
        });
        const [deviceKeyWithAnchor] = await actor.lookup_device_key(seed);
        const { anchor_number: identityNumber } = deviceKeyWithAnchor!;
        agent.replaceIdentity(Ed25519KeyIdentity.generate(seed));
        return { actor, identityNumber };
      },
    });
  },
});
