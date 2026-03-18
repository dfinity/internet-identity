import { Page, test as base, expect } from "@playwright/test";
import {
  addCredentialToVirtualAuthenticator,
  addVirtualAuthenticator,
  createActorForCredential,
  fromBase64,
  getCredentialsFromVirtualAuthenticator,
  II_URL,
  removeCredentialFromVirtualAuthenticator,
} from "../utils";
import { Principal } from "@icp-sdk/core/principal";
import Protocol from "devtools-protocol";

export const DEFAULT_HOST = "https://localhost:5173"; // Vite dev server
export const DEFAULT_NAME = "Test User";

export type IdentityConfig = {
  host?: string;
  createIdentities: Array<{
    name: string;
  }>;
};

interface Identity {
  host: string;
  canisterId: Principal;
  identityNumber: bigint;
  name: string;
  credentials: Protocol.WebAuthn.Credential[];
  authenticatorIds: WeakMap<Page, string>;
}

const credentialCreateTrackingInstalled = new WeakSet<Page>();
const trackedIdentityByPage = new WeakMap<Page, Identity>();
const CREDENTIAL_CREATE_HOOK = "__iiOnCredentialCreate";
const CREDENTIAL_CREATE_HOOK_FLAG = "__iiCredentialCreateHookInstalled";

/**
 * Patches `navigator.credentials.create` in the browser page so tests can react
 * when a new passkey is created and mirror it into the virtual authenticator.
 *
 * The patch also constrains passkey creation to ES256 (`alg: -7`), i.e. ECDSA
 * on P-256, so created credentials match fixture expectations.
 */
const installCredentialCreateOverride = ({
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
const installCredentialCreateOverrideOnCurrentDocument = async (
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
    await page.evaluate(installCredentialCreateOverride, {
      hookName: CREDENTIAL_CREATE_HOOK,
      installedFlag: CREDENTIAL_CREATE_HOOK_FLAG,
    });
  } catch {
    // Navigations can replace the execution context while the listener runs.
  }
};

/**
 * Refreshes fixture credentials from the authenticator currently tracked for
 * the binding source page. If the authenticator is no longer valid, tracking
 * is cleared.
 */
const refreshTrackedIdentityCredentials = async (source: {
  page?: Page;
}): Promise<void> => {
  const page = source.page;
  if (page === undefined) {
    return;
  }

  const identity = trackedIdentityByPage.get(page);
  if (identity === undefined) {
    return;
  }
  const authenticatorId = identity.authenticatorIds.get(page);
  if (authenticatorId === undefined) {
    return;
  }

  try {
    // Add any new credentials created since the last refresh to the fixture identity.
    const newCredentials = await getCredentialsFromVirtualAuthenticator(
      page,
      authenticatorId,
    );
    const credentialIds = new Set(
      identity.credentials.map((credential) => credential.credentialId),
    );
    identity.credentials = [
      ...identity.credentials,
      ...newCredentials.filter(
        (credential) => !credentialIds.has(credential.credentialId),
      ),
    ];
  } catch {
    // The page hook can outlive the authenticator it was tracking.
    trackedIdentityByPage.delete(page);
  }
};

/**
 * Tracks passkeys created on this page for the given identity and ensures the
 * browser-side credential-create override stays installed across navigations.
 */
const trackIdentityCredentialCreation = async (
  page: Page,
  identity: Identity,
): Promise<void> => {
  trackedIdentityByPage.set(page, identity);

  if (credentialCreateTrackingInstalled.has(page)) {
    return;
  }

  await page.exposeBinding(
    CREDENTIAL_CREATE_HOOK,
    refreshTrackedIdentityCredentials,
  );

  // Install for future navigations and current document.
  await page.addInitScript(installCredentialCreateOverride, {
    hookName: CREDENTIAL_CREATE_HOOK,
    installedFlag: CREDENTIAL_CREATE_HOOK_FLAG,
  });
  page.on("framenavigated", (frame) => {
    if (frame !== page.mainFrame()) {
      return;
    }
    void installCredentialCreateOverrideOnCurrentDocument(page);
  });

  await installCredentialCreateOverrideOnCurrentDocument(page);

  credentialCreateTrackingInstalled.add(page);
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

const getIdentityByNumber = (
  identities: Identity[],
  identityNumber: bigint,
): Identity => {
  const identity = identities.find(
    (identity) => identity.identityNumber === identityNumber,
  );
  if (identity === undefined) {
    throw new Error(`Identity with number ${identityNumber} not found`);
  }
  return identity;
};

export const test = base.extend<{
  identityConfig: IdentityConfig;
  identities: Identity[];
  signInWithIdentity: (page: Page, identityNumber: bigint) => Promise<void>;
  addAuthenticatorForIdentity: (
    page: Page,
    identityNumber: bigint,
  ) => Promise<void>;
  setCredentialsForIdentity: (
    page: Page,
    identityNumber: bigint,
    credentials: Protocol.WebAuthn.Credential[],
  ) => void;
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
          const host = identityConfig.host ?? DEFAULT_HOST;
          const actor = await createActorForCredential(
            host,
            canisterId,
            credentials[0],
          );
          const [deviceKeyWithAnchor] = await actor.lookup_device_key(
            fromBase64(credentials[0].credentialId),
          );
          const { anchor_number: identityNumber } = deviceKeyWithAnchor!;
          const identity: Identity = {
            host,
            canisterId,
            name,
            credentials,
            identityNumber,
            authenticatorIds: new WeakMap(),
          };
          return identity;
        }),
      ),
    ),
  signInWithIdentity: ({ addAuthenticatorForIdentity }, use) =>
    use(async (page: Page, identityNumber: bigint) => {
      await addAuthenticatorForIdentity(page, identityNumber);
      const wizard = new IdentityWizard(page);
      await wizard.signIn();
    }),
  addAuthenticatorForIdentity: ({ identities }, use) =>
    use(async (page: Page, identityNumber: bigint) => {
      const identity = getIdentityByNumber(identities, identityNumber);

      // Add virtual authenticator and populate it with the identity's credentials
      const authenticatorId = await addVirtualAuthenticator(page);
      identity.authenticatorIds.set(page, authenticatorId);
      for (const credential of identity.credentials) {
        await addCredentialToVirtualAuthenticator(
          page,
          authenticatorId,
          credential,
        );
      }

      // Keep this identity synchronized with passkeys created in page code.
      await trackIdentityCredentialCreation(page, identity);
    }),
  setCredentialsForIdentity: ({ identities }, use) =>
    use(
      async (
        page,
        identityNumber: bigint,
        credentials: Protocol.WebAuthn.Credential[],
      ) => {
        const identity = getIdentityByNumber(identities, identityNumber);
        const authenticatorId = identity.authenticatorIds.get(page);
        if (authenticatorId !== undefined) {
          const existingCredentialIds = new Set(
            identity.credentials.map((cred) => cred.credentialId),
          );
          const newCredentialIds = new Set(
            credentials.map((cred) => cred.credentialId),
          );
          const credentialToRemove = identity.credentials.filter(
            (existingCredential) =>
              !newCredentialIds.has(existingCredential.credentialId),
          );
          const credentialToAdd = credentials.filter(
            (newCredential) =>
              !existingCredentialIds.has(newCredential.credentialId),
          );
          for (const credential of credentialToRemove) {
            await removeCredentialFromVirtualAuthenticator(
              page,
              authenticatorId,
              credential.credentialId,
            );
          }
          for (const credential of credentialToAdd) {
            await addCredentialToVirtualAuthenticator(
              page,
              authenticatorId,
              credential,
            );
          }
        }
        identity.credentials = credentials;
      },
    ),
});
