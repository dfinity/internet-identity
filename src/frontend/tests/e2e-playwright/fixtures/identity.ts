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

/**
 * Patches `navigator.credentials.create` in the browser page so tests can react
 * when a new passkey is created and mirror it into the virtual authenticator.
 *
 * The patch also constrains passkey creation to ES256 (`alg: -7`), i.e. ECDSA
 * on P-256, so created credentials match fixture expectations.
 */
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
    const createArgs = [...args] as [CredentialCreationOptions?];
    const [options] = createArgs;
    if (options?.publicKey !== undefined) {
      createArgs[0] = {
        ...options,
        publicKey: {
          ...options.publicKey,
          pubKeyCredParams: [{ type: "public-key", alg: -7 }],
        },
      };
    }

    const result = await originalCreate(...createArgs);
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

/**
 * Installs the credential-create hook in the currently loaded document when
 * the page points at Internet Identity. Safe to call repeatedly.
 */
const installCredentialCreateHookOnCurrentDocument = async (
  page: Page,
): Promise<void> => {
  try {
    if (new URL(page.url()).origin !== new URL(II_URL).origin) {
      return;
    }
  } catch {
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

/**
 * Exposes a single Playwright binding per page and keeps the in-page
 * credential-create hook installed across navigations.
 */
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
        credentialCreateSyncState.delete(sourcePage);
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
    const signInLocator = this.#page.getByRole("button", {
      name: "Sign in",
    });
    const switchIdentityLocator = this.#page.getByRole("button", {
      name: "Switch identity",
    });
    const addAnotherIdentityLocator = this.#page.getByRole("button", {
      name: "Add another identity",
    });
    const continueWithPasskeyLocator = this.#page.getByRole("button", {
      name: "Continue with passkey",
    });
    await signInLocator
      .or(switchIdentityLocator)
      .or(continueWithPasskeyLocator)
      .first()
      .waitFor();

    // Either continue to passkey flow immediately
    if (await continueWithPasskeyLocator.isVisible()) {
      await continueWithPasskeyLocator.click();
      return;
    }
    // Or go first trough identity switcher/sign in if necessary
    if (await switchIdentityLocator.isVisible()) {
      await switchIdentityLocator.click();
      await addAnotherIdentityLocator.click();
    } else if (await signInLocator.isVisible()) {
      await signInLocator.click();
    }
    await continueWithPasskeyLocator.click();
  }
}

/**
 * Builds an ECDSA identity from a WebAuthn credential's private key.
 *
 * @param credential WebAuthn credential containing a base64 PKCS#8 P-256 private key.
 * @returns ECDSA identity that can sign requests as this credential.
 * @throws Error If the decoded key is not an EC P-256 key.
 */
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

/**
 * Creates an Internet Identity actor authenticated as the supplied credential.
 */
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
  addAuthenticatorForIdentity: ({ identities }, use) =>
    use(async (page: Page, identityNumber: bigint) => {
      const identity = identities.find(
        (identity) => identity.identityNumber === identityNumber,
      );
      if (identity === undefined) {
        throw new Error("Identity not found");
      }

      // Add virtual authenticator and populate it with the identity's credentials
      const authenticatorId = await addVirtualAuthenticator(page);
      for (const credential of identity.credentials) {
        await addCredentialToVirtualAuthenticator(
          page,
          authenticatorId,
          credential,
        );
      }

      // Set up sync hook for this virtual authenticator so credentials created in tests
      // will be automatically added to the identity's credentials keeping both in sync.
      credentialCreateSyncState.set(page, {
        syncCredentials: async () => {
          identity.credentials = await getCredentialsFromVirtualAuthenticator(
            page,
            authenticatorId,
          );
        },
      });
      await ensureCredentialCreateSyncHook(page);

      return authenticatorId;
    }),
});
