import { Browser, Page, test as base } from "@playwright/test";
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

const DEFAULT_NAME = "Test User";
const ALT_NAME = "Alt User";

class IdentityWizard {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async signIn(auth: DummyAuthFn): Promise<void> {
    await this.#goto();
    auth(this.#page);
    await this.#page
      .getByRole("button", { name: "Use existing identity" })
      .click();
    await this.#page.waitForURL((url) => url.pathname.startsWith("/manage"));
  }

  async signUp(auth: DummyAuthFn, name: string): Promise<void> {
    await this.#goto();
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

interface IdentityOptions {
  canisterId: Principal;
  page: Page;
  authIndex: bigint;
  name: string;
}

export class Identity {
  #options: IdentityOptions;

  constructor(options: IdentityOptions) {
    this.#options = options;
  }

  get auth(): DummyAuthFn {
    return dummyAuth(this.#options.authIndex);
  }

  get name(): string {
    return this.#options.name;
  }

  async signIn(): Promise<void> {
    const wizard = new IdentityWizard(this.#options.page);
    await wizard.signIn(this.auth);
  }

  async signOut(): Promise<void> {
    // Navigating to landing page (reloading) is sufficient for now to sign out
    await this.#options.page.goto(II_URL);
  }

  async createActor(): Promise<{
    actor: ActorSubclass<_SERVICE>;
    identityNumber: bigint;
  }> {
    // Same implementation as found in `DiscoverableDummyIdentity`
    const buffer = new ArrayBuffer(32);
    const view = new DataView(buffer);
    view.setBigUint64(0, this.#options.authIndex);
    const seed = new Uint8Array(buffer);
    const agent = await HttpAgent.create({
      host: "https://localhost:5173", // Vite dev server
      shouldFetchRootKey: true,
      verifyQuerySignatures: false,
      fetch: insecureFetch,
    });
    const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId: this.#options.canisterId,
    });
    const [deviceKeyWithAnchor] = await actor.lookup_device_key(seed);
    const { anchor_number: identityNumber } = deviceKeyWithAnchor!;
    agent.replaceIdentity(Ed25519KeyIdentity.generate(seed));
    return { actor, identityNumber };
  }

  replaceAuth(authIndex: bigint): void {
    this.#options.authIndex = authIndex;
  }

  static async create(
    browser: Browser,
    page: Page,
    name: string,
  ): Promise<Identity> {
    const authIndex = getRandomIndex();
    const auth = dummyAuth(authIndex);
    const tempContext = await browser.newContext();
    const tempPage = await tempContext.newPage();
    await tempPage.goto(II_URL);
    const script = (await tempPage.$("[data-canister-id]"))!;
    const canisterId = Principal.fromText(
      (await script.getAttribute("data-canister-id"))!,
    );
    const tempWizard = new IdentityWizard(tempPage);
    await tempWizard.signUp(auth, name);
    await tempPage.waitForURL(II_URL + "/manage");
    await tempPage.close();
    return new Identity({ canisterId, page, authIndex, name });
  }
}

export const test = base.extend<{
  identity: Identity;
  altIdentity: Identity;
}>({
  identity: async ({ page, browser }, use) => {
    const identity = await Identity.create(browser, page, DEFAULT_NAME);
    await use(identity);
  },
  altIdentity: async ({ page, browser }, use) => {
    const identity = await Identity.create(browser, page, ALT_NAME);
    await use(identity);
  },
});
