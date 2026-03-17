import { Page, test as base, expect } from "@playwright/test";
import {
  addCredentialToVirtualAuthenticator,
  addVirtualAuthenticator,
  fromBase64,
  getCredentialsFromVirtualAuthenticator,
  II_URL,
} from "../utils";
import { Actor, type ActorSubclass, HttpAgent } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { Agent } from "undici";
import { Principal } from "@icp-sdk/core/principal";
import Protocol from "devtools-protocol";

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
  }>;
};

interface Identity {
  canisterId: Principal;
  identityNumber: bigint;
  name: string;
  credentials: Protocol.WebAuthn.Credential[];
}

type CredentialSyncState = { syncCredentials: () => Promise<void> };

const credentialCreateHookInstalled = new WeakSet<Page>();
const credentialCreateSyncState = new WeakMap<Page, CredentialSyncState>();
const CREDENTIAL_CREATE_HOOK = "__iiOnCredentialCreate";
const CREDENTIAL_CREATE_HOOK_FLAG = "__iiCredentialCreateHookInstalled";

const refreshIdentityCredentials = async (
  page: Page,
  authenticatorId: string,
  identity: Identity,
): Promise<void> => {
  identity.credentials = await getCredentialsFromVirtualAuthenticator(
    page,
    authenticatorId,
  );
};

const registerCredentialCreateSync = (
  page: Page,
  syncState: CredentialSyncState,
): void => {
  credentialCreateSyncState.set(page, syncState);
};

const clearCredentialCreateSync = (page: Page): void => {
  credentialCreateSyncState.delete(page);
};

const installCredentialCreateHook = ({
  hookName,
  installedFlag,
}: {
  hookName: string;
  installedFlag: string;
}) => {
  const global = window as typeof window & Record<string, unknown>;

  if (global[installedFlag] === true) {
    return;
  }

  const credentialsContainer = navigator.credentials;
  if (
    credentialsContainer === undefined ||
    typeof credentialsContainer.create !== "function"
  ) {
    return;
  }

  const originalCreate = credentialsContainer.create.bind(credentialsContainer);
  credentialsContainer.create = async (...args) => {
    const result = await originalCreate(...args);
    const hook = (window as unknown as Record<string, () => Promise<void>>)[
      hookName
    ];
    if (typeof hook === "function") {
      await hook();
    }
    return result;
  };

  global[installedFlag] = true;
};

const isInternetIdentityUrl = (url: string): boolean => {
  try {
    return new URL(url).origin === new URL(II_URL).origin;
  } catch {
    return false;
  }
};

const installCredentialCreateHookOnCurrentDocument = async (
  page: Page,
): Promise<void> => {
  if (!isInternetIdentityUrl(page.url())) {
    return;
  }

  try {
    await page.evaluate(installCredentialCreateHook, {
      hookName: CREDENTIAL_CREATE_HOOK,
      installedFlag: CREDENTIAL_CREATE_HOOK_FLAG,
    });
  } catch {
    // Navigations can replace the execution context while the listener runs.
  }
};

const ensureCredentialCreateSyncHook = async (page: Page): Promise<void> => {
  if (credentialCreateHookInstalled.has(page)) {
    return;
  }

  await page.exposeBinding(CREDENTIAL_CREATE_HOOK, async (source) => {
    const sourcePage = source.page;
    if (sourcePage === undefined) {
      return;
    }
    const state = credentialCreateSyncState.get(sourcePage);
    if (state !== undefined) {
      try {
        await state.syncCredentials();
      } catch {
        // The page hook can outlive the authenticator it was tracking.
        clearCredentialCreateSync(sourcePage);
      }
    }
  });

  // Install for future navigations and current document.
  await page.addInitScript(installCredentialCreateHook, {
    hookName: CREDENTIAL_CREATE_HOOK,
    installedFlag: CREDENTIAL_CREATE_HOOK_FLAG,
  });
  page.on("framenavigated", (frame) => {
    if (frame !== page.mainFrame()) {
      return;
    }
    void installCredentialCreateHookOnCurrentDocument(page);
  });

  await installCredentialCreateHookOnCurrentDocument(page);

  credentialCreateHookInstalled.add(page);
};

export class IdentityWizard {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async signIn(): Promise<void> {
    await this.#goto();
    const dialog = this.#page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await dialog.getByRole("button", { name: "Use existing identity" }).click();
    await expect(dialog).toBeHidden();
  }

  async signUp(name: string): Promise<void> {
    await this.#goto();
    const dialog = this.#page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await dialog.getByRole("button", { name: "Create new identity" }).click();
    await this.#page.getByLabel("Identity name").fill(name);
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
      "Add another identity",
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

const createEcdsaIdentityFromCredential = async (
  credential: Protocol.WebAuthn.Credential,
): Promise<ECDSAKeyIdentity> => {
  const privateKey = await crypto.subtle.importKey(
    "pkcs8",
    Buffer.from(credential.privateKey, "base64"),
    { name: "ECDSA", namedCurve: "P-256" },
    true,
    ["sign"],
  );

  const jwk = (await crypto.subtle.exportKey("jwk", privateKey)) as JsonWebKey;
  if (
    jwk.kty !== "EC" ||
    jwk.crv !== "P-256" ||
    jwk.x === undefined ||
    jwk.y === undefined
  ) {
    throw new Error("Expected an EC P-256 credential key");
  }

  const publicKey = await crypto.subtle.importKey(
    "jwk",
    {
      kty: "EC",
      crv: "P-256",
      x: jwk.x,
      y: jwk.y,
      ext: true,
      key_ops: ["verify"],
    },
    { name: "ECDSA", namedCurve: "P-256" },
    true,
    ["verify"],
  );

  return await ECDSAKeyIdentity.fromKeyPair({
    publicKey,
    privateKey,
  });
};

const createActor = async (
  host: string,
  canisterId: Principal,
  credential: Protocol.WebAuthn.Credential,
): Promise<ActorSubclass<_SERVICE>> => {
  const identity = await createEcdsaIdentityFromCredential(credential);
  const agent = await HttpAgent.create({
    host,
    shouldFetchRootKey: true,
    verifyQuerySignatures: false,
    fetch: insecureFetch,
    identity,
  });
  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent,
    canisterId,
  });
  return actor;
};

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
  addAuthenticatorForIdentity: (
    page: Page,
    identityNumber: bigint,
  ) => Promise<string>;
}>({
  identityConfig: {
    createIdentities: [{ name: DEFAULT_NAME }],
  },
  identities: async ({ browser, identityConfig }, use) =>
    use(
      await Promise.all(
        identityConfig.createIdentities.map(async ({ name }) => {
          const context = await browser.newContext();
          const page = await context.newPage();
          await page.goto(II_URL);
          const element = (await page.$("[data-canister-id]"))!;
          const canisterId = Principal.fromText(
            (await element.getAttribute("data-canister-id"))!,
          );
          const authenticatorId = await addVirtualAuthenticator(page);
          const wizard = new IdentityWizard(page);
          await wizard.signUp(name);
          const credentials = await getCredentialsFromVirtualAuthenticator(
            page,
            authenticatorId,
          );
          await page.close();
          await context.close();
          const actor = await createActor(
            identityConfig.host ?? DEFAULT_HOST,
            canisterId,
            credentials[0],
          );
          const [deviceKeyWithAnchor] = await actor.lookup_device_key(
            fromBase64(credentials[0].credentialId),
          );
          const { anchor_number: identityNumber } = deviceKeyWithAnchor!;
          return {
            canisterId,
            name,
            credentials,
            identityNumber,
          };
        }),
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
      await wizard.signIn();
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
        identity.credentials[0],
      );
    }),
  replaceAuthForIdentity: ({ identities }, use) =>
    use((identityNumber: bigint, _newDummyAuthIndex: bigint) => {
      const identity = identities.find(
        (identity) => identity.identityNumber === identityNumber,
      );
      if (identity === undefined) {
        throw new Error("Identity not found");
      }
      // No-op for passkey-based fixture identities.
    }),
  addAuthenticatorForIdentity: ({ identities }, use) =>
    use(async (page: Page, identityNumber: bigint) => {
      const identity = identities.find(
        (identity) => identity.identityNumber === identityNumber,
      );
      if (identity === undefined) {
        throw new Error("Identity not found");
      }
      const authenticatorId = await addVirtualAuthenticator(page);

      // Keep fixture credentials synchronized with passkeys created in page code.
      registerCredentialCreateSync(page, {
        syncCredentials: async () => {
          await refreshIdentityCredentials(page, authenticatorId, identity);
        },
      });

      await ensureCredentialCreateSyncHook(page);

      for (const credential of identity.credentials) {
        await addCredentialToVirtualAuthenticator(
          page,
          authenticatorId,
          credential,
        );
      }

      await refreshIdentityCredentials(page, authenticatorId, identity);

      return authenticatorId;
    }),
});
