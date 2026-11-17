import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createRawSnippet, flushSync, mount, unmount } from "svelte";
import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import { PopupBlockedError } from "$lib/utils/openID";

// The pop-up an OpenID or SSO identity signs in with has to open while the
// click is being handled: Safari blocks a pop-up that opens after anything was
// awaited, be it IndexedDB, the network or WebCrypto.
//
// These tests press the buttons that start a sign-in and look at `window.open`
// as soon as the click returns, before any promise has had a chance to settle.
// An `await` put in front of the sign-in fails them, whatever it awaits.

vi.mock("$app/environment", () => ({ browser: true, building: false }));
vi.mock("$app/navigation", () => ({ goto: vi.fn(), onNavigate: vi.fn() }));

function createSubscribableStore<T>(initial: T) {
  let value = initial;
  const subs = new Set<(v: T) => void>();
  return {
    subscribe: (fn: (v: T) => void) => {
      subs.add(fn);
      fn(value);
      return () => {
        subs.delete(fn);
      };
    },
    set: (v: T) => {
      value = v;
      subs.forEach((fn) => fn(value));
    },
  };
}

const lastUsedIdentitiesStoreValue = vi.hoisted(() => ({
  ...createSubscribableStore<{
    identities: Record<string, unknown>;
    selected?: unknown;
  }>({ identities: {} }),
  addLastUsedIdentity: vi.fn(),
  setAuthorizeOptions: vi.fn(),
  syncLastUsedAccounts: vi.fn(),
}));
const actorForIdentityMock = vi.hoisted(() => vi.fn());
const purgeSessionMock = vi.hoisted(() => vi.fn());
const fetchIdentityCredentialsMock = vi.hoisted(() => vi.fn());
const handleErrorMock = vi.hoisted(() => vi.fn());

vi.mock("$lib/stores/last-used-identities.store", () => ({
  lastUsedIdentitiesStore: lastUsedIdentitiesStoreValue,
}));
// Nobody is signed in unless a test says so, and every sign-in below is left
// pending.
const authenticationStoreValue = vi.hoisted(() =>
  createSubscribableStore<unknown>(undefined),
);
const isAuthenticatedStoreValue = vi.hoisted(() =>
  createSubscribableStore(false),
);
vi.mock("$lib/stores/authentication.store", () => ({
  authenticationStore: authenticationStoreValue,
  isAuthenticatedStore: isAuthenticatedStoreValue,
}));
vi.mock("$lib/stores/session.store", () => ({
  sessionStore: createSubscribableStore({
    nonce: "nonce",
    salt: new Uint8Array(32),
    identity: { getPublicKey: () => ({ toDer: () => new Uint8Array(32) }) },
    agent: {},
    actor: {},
  }),
}));
vi.mock("$lib/stores/session-delegation.store", () => ({
  actorForIdentity: actorForIdentityMock,
  purgeSession: purgeSessionMock,
}));
vi.mock("$lib/stores/app-metadata.store", () => ({
  getAppMetadataStore: () => createSubscribableStore({ name: undefined }),
}));
vi.mock("$lib/utils/fetchCredentials", () => ({
  fetchIdentityCredentials: fetchIdentityCredentialsMock,
}));
vi.mock("$lib/components/utils/error", () => ({
  handleError: handleErrorMock,
}));
vi.mock("$lib/globals", () => ({
  canisterId: "rdmx6-jaaaa-aaaaa-aaadq-cai",
  getPrimaryOrigin: () => undefined,
  frontendCanisterConfig: { dummy_auth: [] },
  backendCanisterConfig: {
    openid_configs: [
      [
        {
          name: "Google",
          issuer: "https://accounts.google.com",
          client_id: "client",
          auth_uri: "https://accounts.google.com/o/oauth2/v2/auth",
          auth_scope: ["openid"],
          // As in production: no FedCM, so the sign-in is a pop-up.
          fedcm_uri: [""],
        },
      ],
    ],
  },
}));
vi.mock("$lib/legacy/features", () => ({ features: { DUMMY_AUTH: false } }));
const discoverSsoConfigMock = vi.hoisted(() => vi.fn());
vi.mock("$lib/utils/ssoDiscovery", () => ({
  discoverSsoConfig: discoverSsoConfigMock,
}));
vi.mock("$lib/utils/analytics/authenticationV2Funnel", () => ({
  AuthenticationV2Events: {},
  authenticationV2Funnel: { trigger: vi.fn(), addProperties: vi.fn() },
}));
vi.mock("$lib/state/featureFlags", () => ({
  READ_ONLY_MODE: createSubscribableStore(false),
}));
vi.mock("$lib/stores/locale.store", () => {
  // Not preprocessed in unit tests, so templates arrive as written.
  const asStore = <T extends object>(value: T): T =>
    Object.assign(value, {
      subscribe: (fn: (v: T) => void) => {
        fn(value);
        return () => {};
      },
    });
  return {
    t: asStore((strings: TemplateStringsArray | string, ...parts: unknown[]) =>
      typeof strings === "string"
        ? strings
        : String.raw({ raw: strings }, ...parts),
    ),
    plural: asStore(
      (_value: number, options: { other: string }) => options.other,
    ),
    formatDuration: asStore(() => ""),
  };
});

import ContinueView from "./ContinueView.svelte";

const IDENTITY_NUMBER = BigInt(10000);
const OTHER_IDENTITY_NUMBER = BigInt(10001);
const TOGGLE_KEY = `ii:multi-accounts:${IDENTITY_NUMBER}`;
const OTHER_TOGGLE_KEY = `ii:multi-accounts:${OTHER_IDENTITY_NUMBER}`;

const identity = (
  authMethod: LastUsedIdentity["authMethod"],
): LastUsedIdentity => ({
  identityNumber: IDENTITY_NUMBER,
  name: "Test",
  authMethod,
  lastUsedTimestampMillis: 0,
});

const identities: Record<string, LastUsedIdentity> = {
  OpenID: identity({
    openid: { iss: "https://accounts.google.com", sub: "sub", metadata: [] },
  }),
  SSO: identity({ sso: { domain: "example.com" } }),
};

const primaryAccount = {
  account_number: [],
  name: [],
  last_used: [],
  origin: "https://app.example",
};

const workAccount = {
  account_number: [BigInt(1)],
  name: ["Work"],
  last_used: [],
  origin: "https://app.example",
};

/**
 * A stored session, which the canister lets read the accounts and nothing
 * else: it refuses to change one.
 */
const sessionActor = () => ({
  get_accounts: vi
    .fn()
    .mockResolvedValue({ Ok: [primaryAccount, workAccount] }),
  get_default_account: vi.fn().mockResolvedValue({ Ok: primaryAccount }),
  set_default_account: vi
    .fn()
    .mockResolvedValue({ Err: { Unauthorized: "session" } }),
});

/** A promise for a test to settle when it gets to that point. */
const deferred = <T>() => {
  let resolve!: (value: T) => void;
  let reject!: (reason: unknown) => void;
  const promise = new Promise<T>((res, rej) => {
    resolve = res;
    reject = rej;
  });
  return { promise, resolve, reject };
};

describe("ContinueView starts a sign-in inside the click that asks for it", () => {
  let component: ReturnType<typeof mount> | undefined;
  let open: ReturnType<typeof vi.spyOn>;
  let onAuthorize: ReturnType<typeof vi.fn<(...args: unknown[]) => void>>;

  const select = (selected: LastUsedIdentity) => {
    lastUsedIdentitiesStoreValue.set({
      identities: { [`${selected.identityNumber}`]: selected },
      selected,
    });
    flushSync();
  };

  const render = (selected: LastUsedIdentity) => {
    select(selected);
    component = mount(ContinueView, {
      target: document.body,
      props: {
        effectiveOrigin: "https://app.example",
        displayOrigin: "https://app.example",
        onAuthorize,
        header: createRawSnippet(() => ({ render: () => "<span></span>" })),
      },
    });
    flushSync();
  };

  const button = (name: string): HTMLButtonElement => {
    const found = Array.from(document.body.querySelectorAll("button")).find(
      (candidate) => candidate.textContent?.trim() === name,
    );
    if (found === undefined) {
      throw new Error(`No button named ${name}`);
    }
    return found;
  };

  const showAllOptions = (): HTMLInputElement =>
    document.body.querySelector<HTMLInputElement>('input[role="switch"]')!;

  const signInsStarted = (): number => open.mock.calls.length;

  /** Lets everything that can settle without the network do so. */
  const settle = async () => {
    await new Promise((resolve) => setTimeout(resolve));
    flushSync();
  };

  /**
   * Presses the switch the way a browser delivers a press: the box is checked,
   * the click is handled and what it started settles, and only then does the
   * box report its change. `click()` reports the change before anything
   * settles, which hides a failure that is handled in between.
   */
  const pressShowAllOptions = async () => {
    const toggle = showAllOptions();
    toggle.checked = !toggle.checked;
    toggle.dispatchEvent(new Event("click", { bubbles: true }));
    await settle();
    toggle.dispatchEvent(new Event("change", { bubbles: true }));
    await settle();
  };

  beforeEach(() => {
    localStorage.clear();
    onAuthorize = vi.fn<(...args: unknown[]) => void>();
    authenticationStoreValue.set(undefined);
    isAuthenticatedStoreValue.set(false);
    // jsdom has no Popover API, which the tooltips drive, no modal dialogs,
    // no visual viewport for them to follow and no form submission for their
    // buttons, no Web Animations API, which transitions run on, and no
    // ResizeObserver, which the spinner of the button uses.
    HTMLElement.prototype.showPopover = () => {};
    HTMLElement.prototype.hidePopover = () => {};
    HTMLDialogElement.prototype.showModal = () => {};
    HTMLDialogElement.prototype.close = () => {};
    vi.stubGlobal("visualViewport", null);
    HTMLFormElement.prototype.requestSubmit = () => {};
    // An animation that is over at once, so that what a transition takes off
    // the screen is gone from the document as well.
    Element.prototype.animate = () =>
      ({
        cancel: () => {},
        finished: Promise.resolve(),
        set onfinish(finish: () => void) {
          queueMicrotask(finish);
        },
      }) as unknown as Animation;
    vi.stubGlobal(
      "ResizeObserver",
      class {
        observe() {}
        unobserve() {}
        disconnect() {}
      },
    );
    vi.stubGlobal(
      "BroadcastChannel",
      class {
        addEventListener() {}
        close() {}
      },
    );
    // A pop-up that stays open: the sign-in stays pending, as it is while the
    // user looks at it.
    open = vi.spyOn(window, "open").mockReturnValue({
      closed: false,
      close: vi.fn(),
      location: { href: "" },
    } as unknown as Window);
    // The lookup of the stored session has not answered by the time of the
    // press: a sign-in must not wait for it.
    actorForIdentityMock.mockReturnValue(new Promise(() => {}));
    fetchIdentityCredentialsMock.mockReturnValue(new Promise(() => {}));
    // Never answers, unless a test has it answer: the pop-up has to open
    // before discovery has.
    discoverSsoConfigMock.mockReturnValue(new Promise(() => {}));
  });

  afterEach(async () => {
    if (component !== undefined) {
      await unmount(component);
      component = undefined;
    }
    document.body.innerHTML = "";
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
    vi.clearAllMocks();
  });

  describe.each(Object.keys(identities))("with a %s identity", (kind) => {
    it("signs in when Continue is pressed", () => {
      render(identities[kind]);

      button("Continue").click();

      expect(signInsStarted()).toBe(1);
    });

    it("signs in when all options are switched on with no session at hand", () => {
      render(identities[kind]);

      showAllOptions().click();

      expect(signInsStarted()).toBe(1);
    });

    it("signs in when Continue is pressed with an account from a stored session", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(sessionActor());
      render(identities[kind]);
      await settle();
      expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
      expect(signInsStarted()).toBe(0);

      button("Continue").click();

      expect(signInsStarted()).toBe(1);
    });

    it("signs in when an account is created with a stored session", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(sessionActor());
      render(identities[kind]);
      await settle();
      button("Add another account").click();
      flushSync();
      const name = document.body.querySelector<HTMLInputElement>(
        'input[aria-label="Account name"]',
      )!;
      name.value = "Demo";
      name.dispatchEvent(new Event("input", { bubbles: true }));
      flushSync();

      button("Create account").click();

      expect(signInsStarted()).toBe(1);
    });

    it("signs in when the default account is changed, and keeps the stored session", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      actorForIdentityMock.mockResolvedValue(actor);
      render(identities[kind]);
      await settle();
      document.body
        .querySelector<HTMLButtonElement>('button[aria-label="Edit Work"]')!
        .click();
      flushSync();
      document.body
        .querySelector<HTMLInputElement>('dialog input[type="checkbox"]')!
        .click();
      flushSync();

      button("Save changes").click();

      expect(signInsStarted()).toBe(1);
      // Changing the default takes a sign-in. The stored session is not asked
      // to do it, and so not thrown away for being refused.
      await settle();
      expect(actor.set_default_account).not.toHaveBeenCalled();
      expect(purgeSessionMock).not.toHaveBeenCalled();
    });

    it("reports a pop-up the browser refused, and lets Continue be pressed again", async () => {
      open.mockReturnValue(null);
      render(identities[kind]);

      button("Continue").click();
      await settle();

      expect(handleErrorMock).toHaveBeenCalledTimes(1);
      expect(handleErrorMock.mock.calls[0][0]).toBeInstanceOf(
        PopupBlockedError,
      );
      expect(button("Continue").disabled).toBe(false);
    });

    it("switches all options back off when the sign-in fails within the press", async () => {
      open.mockReturnValue(null);
      render(identities[kind]);

      await pressShowAllOptions();

      expect(handleErrorMock).toHaveBeenCalledTimes(1);
      expect(showAllOptions().checked).toBe(false);
      expect(localStorage.getItem(TOGGLE_KEY)).toBeNull();
    });

    it("signs in on the next press once the canister refused the stored session", async () => {
      const actor = sessionActor();
      actor.get_accounts.mockResolvedValue({
        Err: { Unauthorized: "session" },
      });
      actorForIdentityMock.mockResolvedValue(actor);
      // The sign-in the refusal falls back to fails too, so that the option
      // can be pressed a second time.
      open.mockReturnValue(null);
      render(identities[kind]);
      await settle();

      await pressShowAllOptions();
      expect(purgeSessionMock).toHaveBeenCalledWith(IDENTITY_NUMBER);
      expect(signInsStarted()).toBe(1);

      showAllOptions().click();

      expect(signInsStarted()).toBe(2);
      expect(actor.get_accounts).toHaveBeenCalledTimes(1);
    });

    it("does not read with a session that failed to read, and looks it up again", async () => {
      const actor = sessionActor();
      actor.get_accounts.mockRejectedValue(new Error("Invalid delegation"));
      actorForIdentityMock.mockResolvedValueOnce(actor);
      // Looked up again, the session turns out to have expired.
      actorForIdentityMock.mockResolvedValue(undefined);
      render(identities[kind]);
      await settle();

      await pressShowAllOptions();
      expect(handleErrorMock).toHaveBeenCalledTimes(1);
      expect(showAllOptions().checked).toBe(false);
      expect(actorForIdentityMock).toHaveBeenCalledTimes(2);

      showAllOptions().click();

      expect(signInsStarted()).toBe(1);
      expect(actor.get_accounts).toHaveBeenCalledTimes(1);
    });

    it("does not read with the stored session of the identity selected before", async () => {
      const actor = sessionActor();
      actorForIdentityMock.mockResolvedValueOnce(actor);
      render(identities[kind]);
      await settle();
      // The lookup for the identity switched to has not answered yet.
      actorForIdentityMock.mockReturnValue(new Promise(() => {}));
      select({ ...identities[kind], identityNumber: OTHER_IDENTITY_NUMBER });

      showAllOptions().click();

      expect(signInsStarted()).toBe(1);
      expect(actor.get_accounts).not.toHaveBeenCalled();
    });

    it("ignores a lookup that answers after another identity was selected", async () => {
      const actor = sessionActor();
      const lookup = deferred<typeof actor>();
      actorForIdentityMock.mockReturnValueOnce(lookup.promise);
      render(identities[kind]);
      select({ ...identities[kind], identityNumber: OTHER_IDENTITY_NUMBER });
      lookup.resolve(actor);
      await settle();

      showAllOptions().click();

      expect(signInsStarted()).toBe(1);
      expect(actor.get_accounts).not.toHaveBeenCalled();
    });

    it("starts no sign-in when the screen comes up with all options on and no session", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(undefined);
      render(identities[kind]);
      await settle();

      // Nothing was pressed, so nothing may open, and nothing has failed.
      expect(signInsStarted()).toBe(0);
      expect(handleErrorMock).not.toHaveBeenCalled();
      expect(showAllOptions().checked).toBe(true);
      expect(localStorage.getItem(TOGGLE_KEY)).toBe("1");
      expect(button("Continue").getAttribute("aria-disabled")).toBe("false");

      // The options take a sign-in, which a press of Continue starts.
      button("Continue").click();

      expect(signInsStarted()).toBe(1);
    });

    it("starts no sign-in when the screen comes up with a stored session the canister refuses", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      actor.get_accounts.mockResolvedValue({
        Err: { Unauthorized: "session" },
      });
      actorForIdentityMock.mockResolvedValue(actor);
      render(identities[kind]);
      await settle();

      expect(purgeSessionMock).toHaveBeenCalledWith(IDENTITY_NUMBER);
      expect(signInsStarted()).toBe(0);
      expect(handleErrorMock).not.toHaveBeenCalled();
      expect(showAllOptions().checked).toBe(true);

      button("Continue").click();

      expect(signInsStarted()).toBe(1);
    });

    it("lets Continue be pressed again when the sign-in for the options fails", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(undefined);
      open.mockReturnValue(null);
      render(identities[kind]);
      await settle();

      button("Continue").click();
      await settle();

      expect(handleErrorMock).toHaveBeenCalledTimes(1);
      expect(handleErrorMock.mock.calls[0][0]).toBeInstanceOf(
        PopupBlockedError,
      );
      // The choice to see all options is kept, and so is the way to them.
      expect(showAllOptions().checked).toBe(true);
      expect(localStorage.getItem(TOGGLE_KEY)).toBe("1");
      expect(button("Continue").disabled).toBe(false);

      button("Continue").click();

      expect(signInsStarted()).toBe(2);
    });

    it("shows the options with the press of Continue they waited for, without continuing", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(undefined);
      render(identities[kind]);
      await settle();
      // Signed in since, so that this press has no sign-in left to wait for.
      authenticationStoreValue.set({
        identityNumber: IDENTITY_NUMBER,
        actor: sessionActor(),
      });
      isAuthenticatedStoreValue.set(true);
      flushSync();

      button("Continue").click();
      await settle();

      expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
      expect(onAuthorize).not.toHaveBeenCalled();

      // With the options on the screen, the next press continues.
      button("Continue").click();
      await settle();

      expect(onAuthorize).toHaveBeenCalledTimes(1);
    });

    it("continues with the account and duration last chosen, after the press that showed them", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(undefined);
      render({
        ...identities[kind],
        authorizeOptions: {
          "https://app.example": {
            accountNumber: BigInt(1),
            sessionDurationSeconds: 86400,
          },
        },
      });
      await settle();
      authenticationStoreValue.set({
        identityNumber: IDENTITY_NUMBER,
        actor: sessionActor(),
      });
      isAuthenticatedStoreValue.set(true);
      flushSync();

      button("Continue").click();
      await settle();
      button("Continue").click();
      await settle();

      expect(onAuthorize).toHaveBeenCalledTimes(1);
      const [accountNumber, accessLevel, maxTimeToLive] =
        onAuthorize.mock.calls[0];
      await expect(accountNumber).resolves.toBe(BigInt(1));
      expect(accessLevel).toBe("full-access");
      expect(maxTimeToLive).toBe(BigInt(86400) * BigInt(1_000_000_000));
    });

    it("does not show the accounts of the identity selected before", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      localStorage.setItem(OTHER_TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValueOnce(sessionActor());
      render(identities[kind]);
      await settle();
      expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
      // The identity switched to has no session to show its own with.
      actorForIdentityMock.mockResolvedValue(undefined);

      select({ ...identities[kind], identityNumber: OTHER_IDENTITY_NUMBER });
      await settle();

      expect(document.body.querySelector('input[type="radio"]')).toBeNull();
      expect(showAllOptions().checked).toBe(true);
      // Nor are they remembered as accounts of the identity switched to.
      expect(
        lastUsedIdentitiesStoreValue.syncLastUsedAccounts,
      ).not.toHaveBeenCalledWith(
        OTHER_IDENTITY_NUMBER,
        expect.anything(),
        expect.anything(),
      );
    });

    it("continues with one press for an identity whose options are shown, selected after one that waited", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      localStorage.setItem(OTHER_TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValueOnce(undefined);
      render(identities[kind]);
      await settle();
      const actor = sessionActor();
      actorForIdentityMock.mockResolvedValue(actor);
      select({ ...identities[kind], identityNumber: OTHER_IDENTITY_NUMBER });
      await settle();
      expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
      authenticationStoreValue.set({
        identityNumber: OTHER_IDENTITY_NUMBER,
        actor,
      });
      isAuthenticatedStoreValue.set(true);
      flushSync();

      button("Continue").click();
      await settle();

      expect(onAuthorize).toHaveBeenCalledTimes(1);
    });

    it("does not make Continue sign in for an identity selected while another's session was being refused", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      localStorage.setItem(OTHER_TOGGLE_KEY, "1");
      const actor = sessionActor();
      const answer = deferred<{ Err: { Unauthorized: string } }>();
      actor.get_accounts.mockReturnValue(answer.promise);
      actorForIdentityMock.mockResolvedValueOnce(actor);
      render(identities[kind]);
      await settle();
      // The lookup for the identity switched to has not answered yet.
      actorForIdentityMock.mockReturnValue(new Promise(() => {}));
      select({ ...identities[kind], identityNumber: OTHER_IDENTITY_NUMBER });
      answer.resolve({ Err: { Unauthorized: "session" } });
      await settle();
      // The session thrown away is the one that was refused.
      expect(purgeSessionMock).toHaveBeenCalledWith(IDENTITY_NUMBER);
      expect(purgeSessionMock).not.toHaveBeenCalledWith(OTHER_IDENTITY_NUMBER);

      button("Continue").click();

      expect(signInsStarted()).toBe(0);
    });

    it("signs in once when a refusal arrives after the options were switched off and on again", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      const answer = deferred<{ Err: { Unauthorized: string } }>();
      actor.get_accounts.mockReturnValue(answer.promise);
      actorForIdentityMock.mockResolvedValue(actor);
      render(identities[kind]);
      await settle();
      showAllOptions().click();
      flushSync();
      answer.resolve({ Err: { Unauthorized: "session" } });
      await settle();
      showAllOptions().click();
      flushSync();
      expect(signInsStarted()).toBe(1);

      button("Continue").click();

      expect(signInsStarted()).toBe(1);
    });

    it("does not show accounts that arrive for the identity selected before", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      localStorage.setItem(OTHER_TOGGLE_KEY, "1");
      const actor = sessionActor();
      const answer = deferred<{ Ok: unknown[] }>();
      actor.get_accounts.mockReturnValue(answer.promise);
      actorForIdentityMock.mockResolvedValueOnce(actor);
      render(identities[kind]);
      await settle();
      // The identity switched to has no session to show its own with.
      actorForIdentityMock.mockResolvedValue(undefined);
      select({ ...identities[kind], identityNumber: OTHER_IDENTITY_NUMBER });
      await settle();

      answer.resolve({ Ok: [primaryAccount, workAccount] });
      await settle();

      expect(document.body.querySelector('input[type="radio"]')).toBeNull();
      expect(
        lastUsedIdentitiesStoreValue.syncLastUsedAccounts,
      ).not.toHaveBeenCalled();
      // The identity switched to still waits for its own.
      button("Continue").click();
      expect(signInsStarted()).toBe(1);
    });

    it("leaves the options of the identity switched to alone when those of the one before fail to load", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      localStorage.setItem(OTHER_TOGGLE_KEY, "1");
      const actor = sessionActor();
      const answer = deferred<never>();
      actor.get_accounts.mockReturnValue(answer.promise);
      actorForIdentityMock.mockResolvedValueOnce(actor);
      render(identities[kind]);
      await settle();
      // The lookup for the identity switched to has not answered yet.
      actorForIdentityMock.mockReturnValue(new Promise(() => {}));
      select({ ...identities[kind], identityNumber: OTHER_IDENTITY_NUMBER });

      answer.reject(new Error("Failed to fetch"));
      await settle();

      expect(handleErrorMock).not.toHaveBeenCalled();
      expect(showAllOptions().checked).toBe(true);
      expect(localStorage.getItem(OTHER_TOGGLE_KEY)).toBe("1");
    });

    it("signs in once when the stored session is looked up after the options were switched off and on again", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const lookup = deferred<undefined>();
      actorForIdentityMock.mockReturnValue(lookup.promise);
      render(identities[kind]);
      showAllOptions().click();
      flushSync();
      showAllOptions().click();
      flushSync();
      expect(signInsStarted()).toBe(1);

      // No session: on its own, this would have the options wait for a press
      // of Continue. The switch has started the sign-in for them already.
      lookup.resolve(undefined);
      await settle();
      button("Continue").click();

      expect(signInsStarted()).toBe(1);
    });

    it("signs in once when the options were switched off and on again before Continue", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(undefined);
      render(identities[kind]);
      await settle();
      showAllOptions().click();
      flushSync();
      showAllOptions().click();
      flushSync();
      expect(signInsStarted()).toBe(1);

      // The accounts are on their way with the sign-in the switch started.
      button("Continue").click();

      expect(signInsStarted()).toBe(1);
    });

    it("leaves all options off when they were switched off before the stored session was found", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      const lookup = deferred<typeof actor>();
      actorForIdentityMock.mockReturnValue(lookup.promise);
      render(identities[kind]);
      expect(showAllOptions().checked).toBe(true);

      showAllOptions().click();
      flushSync();
      lookup.resolve(actor);
      await settle();

      expect(showAllOptions().checked).toBe(false);
      expect(actor.get_accounts).not.toHaveBeenCalled();
      expect(signInsStarted()).toBe(0);
    });
  });

  it("opens the pop-up of an OpenID identity on its provider", () => {
    render(identities.OpenID);

    button("Continue").click();

    expect(String(open.mock.calls[0][0])).toMatch(
      /^https:\/\/accounts\.google\.com\//,
    );
  });

  it("opens the pop-up of an SSO identity before its provider is known", () => {
    render(identities.SSO);

    button("Continue").click();

    expect(open.mock.calls[0][0]).toBe("/sso-connecting");
  });

  it("continues for the identity switched to once the sign-in for another's options has failed", async () => {
    localStorage.setItem(TOGGLE_KEY, "1");
    localStorage.setItem(OTHER_TOGGLE_KEY, "1");
    const discovery = deferred<never>();
    discoverSsoConfigMock.mockReturnValue(discovery.promise);
    actorForIdentityMock.mockResolvedValueOnce(undefined);
    render(identities.SSO);
    await settle();
    button("Continue").click();
    expect(signInsStarted()).toBe(1);
    const actor = sessionActor();
    actorForIdentityMock.mockResolvedValue(actor);
    select({ ...identities.SSO, identityNumber: OTHER_IDENTITY_NUMBER });
    await settle();
    expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();

    discovery.reject(new Error("SSO discovery failed"));
    await settle();
    expect(handleErrorMock).toHaveBeenCalledTimes(1);
    authenticationStoreValue.set({
      identityNumber: OTHER_IDENTITY_NUMBER,
      actor,
    });
    isAuthenticatedStoreValue.set(true);
    flushSync();
    button("Continue").click();
    await settle();

    expect(onAuthorize).toHaveBeenCalledTimes(1);
  });

  it("leaves the options of the identity switched to on when the sign-in the switch started for another fails", async () => {
    localStorage.setItem(OTHER_TOGGLE_KEY, "1");
    const discovery = deferred<never>();
    discoverSsoConfigMock.mockReturnValue(discovery.promise);
    actorForIdentityMock.mockResolvedValueOnce(undefined);
    render(identities.SSO);
    await settle();
    showAllOptions().click();
    flushSync();
    expect(signInsStarted()).toBe(1);
    actorForIdentityMock.mockResolvedValue(sessionActor());
    select({ ...identities.SSO, identityNumber: OTHER_IDENTITY_NUMBER });
    await settle();

    discovery.reject(new Error("SSO discovery failed"));
    await settle();

    expect(handleErrorMock).toHaveBeenCalledTimes(1);
    expect(showAllOptions().checked).toBe(true);
    expect(localStorage.getItem(OTHER_TOGGLE_KEY)).toBe("1");
  });

  it("loads the accounts with a stored session, without a sign-in", async () => {
    const actor = sessionActor();
    actorForIdentityMock.mockResolvedValue(actor);
    render(identities.OpenID);
    await settle();

    showAllOptions().click();
    await settle();

    expect(actor.get_accounts).toHaveBeenCalledTimes(1);
    expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
    expect(signInsStarted()).toBe(0);
  });
});
