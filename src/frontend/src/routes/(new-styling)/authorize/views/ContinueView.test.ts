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
//
// They also look at which identity the screen acts as. The page can be signed
// in as another identity than the selected one, and a sign-in can end after
// another identity was selected: neither is the selected identity's to act
// with.

vi.mock("$app/environment", () => ({ browser: true, building: false }));
vi.mock("$app/navigation", () => ({ goto: vi.fn(), onNavigate: vi.fn() }));

function createSubscribableStore<T>(initialValue: T) {
  let value = initialValue;
  const subscribers = new Set<(value: T) => void>();
  return {
    subscribe: (subscriber: (value: T) => void) => {
      subscribers.add(subscriber);
      subscriber(value);
      return () => {
        subscribers.delete(subscriber);
      };
    },
    set: (newValue: T) => {
      value = newValue;
      subscribers.forEach((subscriber) => subscriber(value));
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
// Nobody is signed in unless a test says so, and a sign-in is left pending
// unless a test has it finish.
const authenticationStoreValue = vi.hoisted(() => {
  const store = createSubscribableStore<unknown>(undefined);
  let pageActor: unknown;
  return {
    ...store,
    /**
     * Sets the actor of the page. As in the real store, a sign-in that
     * finishes brings an identity and no actor of its own: whichever identity
     * signs in is given this one.
     */
    setPageActor: (actor: unknown) => {
      pageActor = actor;
    },
    /** What a sign-in that finishes stores: `authenticated`, with the actor of the page. */
    finishSignIn: (authenticated: object) => {
      store.set({ ...authenticated, actor: pageActor });
      return Promise.resolve();
    },
  };
});
vi.mock("$lib/stores/authentication.store", () => ({
  authenticationStore: {
    subscribe: authenticationStoreValue.subscribe,
    set: authenticationStoreValue.finishSignIn,
  },
}));
/**
 * The session a sign-in runs on. `reset` replaces it with a new one, as the
 * real store does. The two differ in their nonce, `initialNonce` and
 * `nonceAfterReset`, so that a test can tell on which of them a sign-in was
 * started. `restore` puts the first one back.
 */
const sessionStoreValue = vi.hoisted(() => {
  const initialNonce = "nonce";
  const nonceAfterReset = "nonce-after-reset";
  const sessionWithNonce = (nonce: string) => ({
    nonce,
    salt: new Uint8Array(32),
    identity: { getPublicKey: () => ({ toDer: () => new Uint8Array(32) }) },
    agent: {},
    actor: {},
  });
  const store = createSubscribableStore(sessionWithNonce(initialNonce));
  return {
    initialNonce,
    nonceAfterReset,
    subscribe: store.subscribe,
    reset: vi.fn(() => store.set(sessionWithNonce(nonceAfterReset))),
    restore: () => store.set(sessionWithNonce(initialNonce)),
  };
});
vi.mock("$lib/stores/session.store", () => ({
  sessionStore: sessionStoreValue,
}));
// The canister's part of a passkey sign-in, which a test settles to have the
// sign-in finish. Those of the pop-up sign-ins are never reached: no pop-up
// below ever answers.
const authenticateWithPasskeyMock = vi.hoisted(() => vi.fn());
vi.mock("$lib/utils/authentication", () => ({
  authenticateWithPasskey: authenticateWithPasskeyMock,
  authenticateWithJWT: vi.fn(),
  authenticateWithSso: vi.fn(),
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
      subscribe: (subscriber: (value: T) => void) => {
        subscriber(value);
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

/** The number of the identity the Continue screen comes up with. */
const IDENTITY_NUMBER = BigInt(10000);
/** The number of a second identity, for the tests that select another one. */
const OTHER_IDENTITY_NUMBER = BigInt(10001);
/**
 * The `localStorage` key under which `ContinueView.svelte` keeps whether "Show
 * all options" is on for the identity numbered `IDENTITY_NUMBER`: its
 * `TOGGLE_STORAGE_PREFIX` followed by the identity number. The value is `"1"`
 * while the options are on, and the key is absent while they are off.
 */
const TOGGLE_KEY = `ii:multi-accounts:${IDENTITY_NUMBER}`;
/**
 * The `localStorage` key under which `ContinueView.svelte` keeps whether "Show
 * all options" is on for the identity numbered `OTHER_IDENTITY_NUMBER`, in the
 * same form as `TOGGLE_KEY`.
 */
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

/**
 * The actor of a sign-in, which the canister lets change the accounts as well
 * as read them.
 */
const signInActor = () => ({
  ...sessionActor(),
  create_account: vi.fn().mockResolvedValue({ Ok: workAccount }),
  update_account: vi.fn().mockResolvedValue({ Ok: workAccount }),
  set_default_account: vi.fn().mockResolvedValue({ Ok: workAccount }),
});

/**
 * An identity that signs in with a passkey. It opens no pop-up, and a test can
 * have its sign-in finish: see `authenticateWithPasskeyMock`.
 */
const passkeyIdentity = identity({
  passkey: { credentialId: new Uint8Array([1]) },
});

/**
 * What the canister's part of a passkey sign-in answers with when the passkey
 * belongs to the identity numbered `identityNumber`.
 */
const passkeySignInAs = (identityNumber: bigint) => ({
  identity: {},
  identityNumber,
  credentialId: new Uint8Array([1]),
});

/** A promise for a test to settle when it gets to that point. */
const deferred = <T>() => {
  let resolve!: (value: T) => void;
  let reject!: (reason: unknown) => void;
  const promise = new Promise<T>((resolvePromise, rejectPromise) => {
    resolve = resolvePromise;
    reject = rejectPromise;
  });
  return { promise, resolve, reject };
};

describe("ContinueView starts a sign-in inside the click that asks for it", () => {
  let component: ReturnType<typeof mount> | undefined;
  let open: ReturnType<typeof vi.spyOn>;
  let onAuthorize: ReturnType<typeof vi.fn<(...args: unknown[]) => void>>;

  const select = (selectedIdentity: LastUsedIdentity) => {
    lastUsedIdentitiesStoreValue.set({
      identities: { [`${selectedIdentity.identityNumber}`]: selectedIdentity },
      selected: selectedIdentity,
    });
    flushSync();
  };

  const render = (selectedIdentity: LastUsedIdentity) => {
    select(selectedIdentity);
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
    const matchingButton = Array.from(
      document.body.querySelectorAll("button"),
    ).find((candidate) => candidate.textContent?.trim() === name);
    if (matchingButton === undefined) {
      throw new Error(`No button named ${name}`);
    }
    return matchingButton;
  };

  const element = <T extends Element>(
    selector: string,
    type: new () => T,
  ): T => {
    const matchingElement = document.body.querySelector(selector);
    if (!(matchingElement instanceof type)) {
      throw new Error(`No ${type.name} matches ${selector}`);
    }
    return matchingElement;
  };

  const showAllOptions = (): HTMLInputElement =>
    element('input[role="switch"]', HTMLInputElement);

  const signInsStarted = (): number => open.mock.calls.length;

  /** Puts back what `stubProperty` replaced, most recent first. */
  let restoreStubbedProperties: (() => void)[] = [];

  /**
   * Defines `property` on `target` as `value` until `afterEach`, which puts
   * back the descriptor `property` had on `target`, or removes `property` when
   * it had none.
   */
  const stubProperty = (target: object, property: string, value: unknown) => {
    const originalDescriptor = Object.getOwnPropertyDescriptor(
      target,
      property,
    );
    Object.defineProperty(target, property, {
      configurable: true,
      writable: true,
      value,
    });
    restoreStubbedProperties.unshift(() => {
      if (originalDescriptor === undefined) {
        Reflect.deleteProperty(target, property);
      } else {
        Object.defineProperty(target, property, originalDescriptor);
      }
    });
  };

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

  /**
   * Signs the page in as the identity numbered `OTHER_IDENTITY_NUMBER` and
   * returns the actor of the page. With the identity numbered
   * `IDENTITY_NUMBER` selected, this is what "Manage identities" leaves behind
   * when the identity that signed in on the page is removed and the next one
   * is selected.
   */
  const signInAsOtherIdentity = () => {
    const actor = signInActor();
    authenticationStoreValue.setPageActor(actor);
    authenticationStoreValue.set({
      identityNumber: OTHER_IDENTITY_NUMBER,
      actor,
    });
    flushSync();
    return actor;
  };

  /**
   * Expects the one pop-up that opened to have opened after the session was
   * reset, once: the sign-in runs on the new session.
   */
  const expectSignInOnNewSession = () => {
    expect(sessionStoreValue.reset).toHaveBeenCalledTimes(1);
    expect(sessionStoreValue.reset.mock.invocationCallOrder[0]).toBeLessThan(
      open.mock.invocationCallOrder[0],
    );
  };

  /**
   * Has the passkey sign-ins that start from here on stay pending until the
   * test settles what this returns. The page gets `signInActor()` for its
   * actor, which is returned as `actor`.
   */
  const pendingPasskeySignIn = () => {
    const signIn = deferred<ReturnType<typeof passkeySignInAs>>();
    fetchIdentityCredentialsMock.mockResolvedValue(undefined);
    authenticateWithPasskeyMock.mockReturnValue(signIn.promise);
    const actor = signInActor();
    authenticationStoreValue.setPageActor(actor);
    return { signIn, actor };
  };

  beforeEach(() => {
    localStorage.clear();
    onAuthorize = vi.fn<(...args: unknown[]) => void>();
    authenticationStoreValue.set(undefined);
    authenticationStoreValue.setPageActor(undefined);
    sessionStoreValue.restore();
    // jsdom has no Popover API, which the tooltips drive, no modal dialogs,
    // no visual viewport for them to follow and no form submission for their
    // buttons, no Web Animations API, which transitions run on, and no
    // ResizeObserver, which the spinner of the button uses.
    stubProperty(HTMLElement.prototype, "showPopover", () => {});
    stubProperty(HTMLElement.prototype, "hidePopover", () => {});
    stubProperty(HTMLDialogElement.prototype, "showModal", () => {});
    stubProperty(HTMLDialogElement.prototype, "close", () => {});
    vi.stubGlobal("visualViewport", null);
    stubProperty(HTMLFormElement.prototype, "requestSubmit", () => {});
    // An animation that is over at once, so that what a transition takes off
    // the screen is gone from the document as well.
    stubProperty(Element.prototype, "animate", () => ({
      cancel: () => {},
      finished: Promise.resolve(),
      set onfinish(finish: () => void) {
        queueMicrotask(finish);
      },
    }));
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
    authenticateWithPasskeyMock.mockReturnValue(new Promise(() => {}));
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
    restoreStubbedProperties.forEach((restore) => restore());
    restoreStubbedProperties = [];
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
    vi.clearAllMocks();
  });

  describe.each(Object.keys(identities))("with a %s identity", (kind) => {
    it("signs in when Continue is pressed", () => {
      render(identities[kind]);
      expect(signInsStarted()).toBe(0);

      button("Continue").click();

      expect(signInsStarted()).toBe(1);
    });

    it("continues with the default account when Continue is pressed while signed in", async () => {
      const actor = sessionActor();
      actor.get_default_account.mockResolvedValue({ Ok: workAccount });
      render(identities[kind]);
      authenticationStoreValue.set({ identityNumber: IDENTITY_NUMBER, actor });
      flushSync();

      button("Continue").click();
      await settle();

      expect(signInsStarted()).toBe(0);
      expect(sessionStoreValue.reset).not.toHaveBeenCalled();
      expect(actor.get_default_account).toHaveBeenCalledWith(
        IDENTITY_NUMBER,
        "https://app.example",
      );
      expect(onAuthorize).toHaveBeenCalledTimes(1);
      await expect(onAuthorize.mock.calls[0][0]).resolves.toBe(BigInt(1));
    });

    it("signs in when all options are switched on with no session at hand", () => {
      render(identities[kind]);
      expect(signInsStarted()).toBe(0);

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
      const name = element(
        'input[aria-label="Account name"]',
        HTMLInputElement,
      );
      name.value = "Demo";
      name.dispatchEvent(new Event("input", { bubbles: true }));
      flushSync();
      expect(signInsStarted()).toBe(0);

      button("Create account").click();

      expect(signInsStarted()).toBe(1);
    });

    it("signs in when the default account is changed, and keeps the stored session", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      actorForIdentityMock.mockResolvedValue(actor);
      render(identities[kind]);
      await settle();
      element('button[aria-label="Edit Work"]', HTMLButtonElement).click();
      flushSync();
      element('dialog input[type="checkbox"]', HTMLInputElement).click();
      flushSync();
      expect(signInsStarted()).toBe(0);

      button("Save changes").click();

      expect(signInsStarted()).toBe(1);
      // Changing the default takes a sign-in. The stored session is not asked
      // to do it, and so not thrown away for being refused.
      await settle();
      expect(actor.set_default_account).not.toHaveBeenCalled();
      expect(purgeSessionMock).not.toHaveBeenCalled();
    });

    it("makes an account the default with the sign-in at hand, not with the stored session", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const storedSessionActor = sessionActor();
      actorForIdentityMock.mockResolvedValue(storedSessionActor);
      render(identities[kind]);
      await settle();
      // Signed in since, so that this press has no sign-in left to wait for.
      const signedInActor = {
        ...sessionActor(),
        set_default_account: vi.fn().mockResolvedValue({ Ok: workAccount }),
      };
      authenticationStoreValue.set({
        identityNumber: IDENTITY_NUMBER,
        actor: signedInActor,
      });
      flushSync();
      element('button[aria-label="Edit Work"]', HTMLButtonElement).click();
      flushSync();
      element('dialog input[type="checkbox"]', HTMLInputElement).click();
      flushSync();

      button("Save changes").click();
      await settle();

      expect(signInsStarted()).toBe(0);
      expect(signedInActor.set_default_account).toHaveBeenCalledWith(
        IDENTITY_NUMBER,
        "https://app.example",
        [BigInt(1)],
      );
      expect(storedSessionActor.set_default_account).not.toHaveBeenCalled();
      expect(handleErrorMock).not.toHaveBeenCalled();
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

    it("waits for Continue when the canister refuses the stored session on a press of the switch", async () => {
      const actor = sessionActor();
      actor.get_accounts.mockResolvedValue({
        Err: { Unauthorized: "session" },
      });
      actorForIdentityMock.mockResolvedValue(actor);
      render(identities[kind]);
      await settle();

      await pressShowAllOptions();

      // The press went into asking the canister: no sign-in is started after
      // it, and nothing has failed for the user.
      expect(purgeSessionMock).toHaveBeenCalledWith(IDENTITY_NUMBER);
      expect(signInsStarted()).toBe(0);
      expect(handleErrorMock).not.toHaveBeenCalled();
      expect(showAllOptions().checked).toBe(true);

      button("Continue").click();

      expect(signInsStarted()).toBe(1);
      expect(actor.get_accounts).toHaveBeenCalledTimes(1);
    });

    it("starts no sign-in when the stored session is refused after the options were switched off again", async () => {
      const actor = sessionActor();
      const answer = deferred<{ Err: { Unauthorized: string } }>();
      actor.get_accounts.mockReturnValue(answer.promise);
      actorForIdentityMock.mockResolvedValue(actor);
      render(identities[kind]);
      await settle();
      showAllOptions().click();
      flushSync();
      showAllOptions().click();
      flushSync();

      answer.resolve({ Err: { Unauthorized: "session" } });
      await settle();

      expect(signInsStarted()).toBe(0);
      expect(handleErrorMock).not.toHaveBeenCalled();
      expect(showAllOptions().checked).toBe(false);
      // Nor do the options wait for Continue once they are switched on
      // again: that press signs in for them, and Continue leaves it at that.
      showAllOptions().click();
      flushSync();
      expect(signInsStarted()).toBe(1);
      button("Continue").click();
      expect(signInsStarted()).toBe(1);
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
      expect(signInsStarted()).toBe(0);

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
      expect(signInsStarted()).toBe(0);

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
      expect(signInsStarted()).toBe(0);

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
      flushSync();

      button("Continue").click();
      await settle();

      expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
      expect(onAuthorize).not.toHaveBeenCalled();

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
      expect(purgeSessionMock).toHaveBeenCalledWith(IDENTITY_NUMBER);
      expect(purgeSessionMock).not.toHaveBeenCalledWith(OTHER_IDENTITY_NUMBER);

      button("Continue").click();

      expect(signInsStarted()).toBe(0);
    });

    it("signs in once when the options are switched on again after a refusal that arrived while they were off", async () => {
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
      expect(signInsStarted()).toBe(0);
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
      expect(signInsStarted()).toBe(0);
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

    it("reports nothing when the accounts fail to load after the options were switched off", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      const answer = deferred<never>();
      actor.get_accounts.mockReturnValue(answer.promise);
      actorForIdentityMock.mockResolvedValue(actor);
      render(identities[kind]);
      await settle();
      showAllOptions().click();
      flushSync();

      answer.reject(new Error("Failed to fetch"));
      await settle();

      expect(handleErrorMock).not.toHaveBeenCalled();
      expect(showAllOptions().checked).toBe(false);
    });

    it("reports nothing when the accounts fail to load after the screen was taken down", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      const answer = deferred<never>();
      actor.get_accounts.mockReturnValue(answer.promise);
      actorForIdentityMock.mockResolvedValue(actor);
      render(identities[kind]);
      await settle();
      expect(actor.get_accounts).toHaveBeenCalledTimes(1);
      if (component === undefined) {
        throw new Error("The screen is not up");
      }
      await unmount(component);
      component = undefined;

      answer.reject(new Error("Failed to fetch"));
      await settle();

      expect(handleErrorMock).not.toHaveBeenCalled();
    });

    it("asks the canister for nothing when the stored session is found after the screen was taken down", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      const lookup = deferred<typeof actor>();
      actorForIdentityMock.mockReturnValue(lookup.promise);
      render(identities[kind]);
      await settle();
      expect(showAllOptions().checked).toBe(true);
      if (component === undefined) {
        throw new Error("The screen is not up");
      }
      await unmount(component);
      component = undefined;

      lookup.resolve(actor);
      await settle();

      expect(actor.get_accounts).not.toHaveBeenCalled();
      expect(actor.get_default_account).not.toHaveBeenCalled();
      expect(handleErrorMock).not.toHaveBeenCalled();
    });

    it("reports it and switches all options off when the accounts fail to load as the screen comes up", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      const failure = new Error("Failed to fetch");
      actor.get_accounts.mockRejectedValue(failure);
      actorForIdentityMock.mockResolvedValue(actor);
      render(identities[kind]);
      await settle();

      // Nothing was pressed, so nothing opens, and the failure is reported.
      expect(signInsStarted()).toBe(0);
      expect(handleErrorMock).toHaveBeenCalledTimes(1);
      expect(handleErrorMock).toHaveBeenCalledWith(failure);
      // Not left on with no accounts and a Continue that does nothing.
      expect(showAllOptions().checked).toBe(false);
      expect(localStorage.getItem(TOGGLE_KEY)).toBeNull();
      expect(button("Continue").getAttribute("aria-disabled")).toBe("false");

      button("Continue").click();

      expect(signInsStarted()).toBe(1);
    });

    it("leaves Continue inert while the accounts load as the screen comes up", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      actor.get_accounts.mockReturnValue(new Promise(() => {}));
      actorForIdentityMock.mockResolvedValue(actor);
      render(identities[kind]);
      await settle();

      button("Continue").click();

      expect(signInsStarted()).toBe(0);
      expect(button("Continue").getAttribute("aria-disabled")).toBe("true");
    });

    it("continues with one press once the accounts are on the screen, however they got there", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      const answer = deferred<{ Ok: unknown[] }>();
      actor.get_accounts.mockReturnValue(answer.promise);
      actorForIdentityMock.mockResolvedValueOnce(actor);
      render(identities[kind]);
      await settle();
      // Another identity is selected and then this one again, by which time
      // its stored session is gone: the options wait for a sign-in.
      actorForIdentityMock.mockReturnValue(new Promise(() => {}));
      select({ ...identities[kind], identityNumber: OTHER_IDENTITY_NUMBER });
      actorForIdentityMock.mockResolvedValue(undefined);
      select(identities[kind]);
      await settle();

      // The accounts asked for at first arrive after all.
      answer.resolve({ Ok: [primaryAccount, workAccount] });
      await settle();
      expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
      authenticationStoreValue.set({ identityNumber: IDENTITY_NUMBER, actor });
      flushSync();
      button("Continue").click();
      await settle();

      expect(onAuthorize).toHaveBeenCalledTimes(1);
    });

    it("continues with one press when the accounts arrive before the session is found to be gone", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      const answer = deferred<{ Ok: unknown[] }>();
      actor.get_accounts.mockReturnValue(answer.promise);
      actorForIdentityMock.mockResolvedValueOnce(actor);
      render(identities[kind]);
      await settle();
      // Another identity is selected and then this one again, whose stored
      // session is gone by then. That has not been found out yet.
      actorForIdentityMock.mockReturnValue(new Promise(() => {}));
      select({ ...identities[kind], identityNumber: OTHER_IDENTITY_NUMBER });
      const lookup = deferred<undefined>();
      actorForIdentityMock.mockReturnValue(lookup.promise);
      select(identities[kind]);
      await settle();

      // The accounts asked for at first arrive, and only then the news that
      // there is no session to ask for them with.
      answer.resolve({ Ok: [primaryAccount, workAccount] });
      await settle();
      lookup.resolve(undefined);
      await settle();
      expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
      authenticationStoreValue.set({ identityNumber: IDENTITY_NUMBER, actor });
      flushSync();
      button("Continue").click();
      await settle();

      expect(onAuthorize).toHaveBeenCalledTimes(1);
      expect(actor.get_accounts).toHaveBeenCalledTimes(1);
    });

    it("continues with one press when a press of the switch is refused after the accounts arrived", async () => {
      const actor = sessionActor();
      const accountsAnswer = deferred<{ Ok: unknown[] }>();
      const refusalAnswer = deferred<{ Err: { Unauthorized: string } }>();
      actor.get_accounts
        .mockReturnValueOnce(accountsAnswer.promise)
        .mockReturnValueOnce(refusalAnswer.promise);
      actorForIdentityMock.mockResolvedValue(actor);
      render(identities[kind]);
      await settle();
      // On, which asks the canister, off, and on again, which asks once more.
      showAllOptions().click();
      flushSync();
      showAllOptions().click();
      flushSync();
      showAllOptions().click();
      flushSync();
      expect(actor.get_accounts).toHaveBeenCalledTimes(2);
      expect(signInsStarted()).toBe(0);

      accountsAnswer.resolve({ Ok: [primaryAccount, workAccount] });
      await settle();
      refusalAnswer.resolve({ Err: { Unauthorized: "session" } });
      await settle();
      expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
      expect(showAllOptions().checked).toBe(true);
      authenticationStoreValue.set({ identityNumber: IDENTITY_NUMBER, actor });
      flushSync();
      button("Continue").click();
      await settle();

      expect(onAuthorize).toHaveBeenCalledTimes(1);
      expect(actor.get_accounts).toHaveBeenCalledTimes(2);
    });

    it("does not read with a stored session found after the options were switched off and on again", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      const actor = sessionActor();
      const lookup = deferred<typeof actor>();
      actorForIdentityMock.mockReturnValue(lookup.promise);
      render(identities[kind]);
      showAllOptions().click();
      flushSync();
      showAllOptions().click();
      flushSync();
      // No session known at the press: it signs in for the options.
      expect(signInsStarted()).toBe(1);

      lookup.resolve(actor);
      await settle();

      expect(actor.get_accounts).not.toHaveBeenCalled();
      expect(document.body.querySelector('input[type="radio"]')).toBeNull();
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
      expect(signInsStarted()).toBe(0);
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

    it("does not make Continue sign in while the session of an identity selected after one that waited is looked up", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      localStorage.setItem(OTHER_TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValueOnce(undefined);
      render(identities[kind]);
      await settle();
      expect(button("Continue").getAttribute("aria-disabled")).toBe("false");
      // The lookup for the identity switched to has not answered yet.
      actorForIdentityMock.mockReturnValue(new Promise(() => {}));
      select({ ...identities[kind], identityNumber: OTHER_IDENTITY_NUMBER });
      await settle();

      button("Continue").click();

      expect(signInsStarted()).toBe(0);
    });

    it("shows Continue as busy and locks the switch while the sign-in for the options is on its way", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(undefined);
      render(identities[kind]);
      await settle();
      const continueButton = button("Continue");

      continueButton.click();
      flushSync();

      expect(signInsStarted()).toBe(1);
      expect(continueButton.disabled).toBe(true);
      expect(showAllOptions().disabled).toBe(true);
    });

    it("starts the sign-in on a new session when Continue is pressed with nobody signed in", () => {
      render(identities[kind]);
      expect(signInsStarted()).toBe(0);

      button("Continue").click();

      expect(signInsStarted()).toBe(1);
      expectSignInOnNewSession();
    });

    it("signs in as the selected identity when Continue is pressed while another one is signed in", async () => {
      actorForIdentityMock.mockResolvedValue(undefined);
      render(identities[kind]);
      await settle();
      const actor = signInAsOtherIdentity();
      expect(signInsStarted()).toBe(0);

      button("Continue").click();

      expect(signInsStarted()).toBe(1);
      expectSignInOnNewSession();
      // The app is not authorized as the identity that happens to be signed
      // in, and the canister is not asked for that identity's default account.
      await settle();
      expect(actor.get_default_account).not.toHaveBeenCalled();
      expect(onAuthorize).not.toHaveBeenCalled();
    });

    it("signs in as the selected identity when all options are switched on while another one is signed in", async () => {
      actorForIdentityMock.mockResolvedValue(undefined);
      render(identities[kind]);
      await settle();
      const actor = signInAsOtherIdentity();
      expect(signInsStarted()).toBe(0);

      showAllOptions().click();

      expect(signInsStarted()).toBe(1);
      expectSignInOnNewSession();
      // The accounts of the identity that happens to be signed in are neither
      // shown nor remembered for the selected one.
      await settle();
      expect(actor.get_accounts).not.toHaveBeenCalled();
      expect(document.body.querySelector('input[type="radio"]')).toBeNull();
      expect(
        lastUsedIdentitiesStoreValue.syncLastUsedAccounts,
      ).not.toHaveBeenCalled();
    });

    it("signs in as the selected identity when Continue is pressed for options that waited while another one is signed in", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(undefined);
      render(identities[kind]);
      await settle();
      const actor = signInAsOtherIdentity();
      expect(signInsStarted()).toBe(0);

      button("Continue").click();

      expect(signInsStarted()).toBe(1);
      expectSignInOnNewSession();
      await settle();
      expect(actor.get_accounts).not.toHaveBeenCalled();
      expect(document.body.querySelector('input[type="radio"]')).toBeNull();
      expect(
        lastUsedIdentitiesStoreValue.syncLastUsedAccounts,
      ).not.toHaveBeenCalled();
      expect(onAuthorize).not.toHaveBeenCalled();
    });

    it("signs in as the selected identity when Continue is pressed with an account while another one is signed in", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(sessionActor());
      render(identities[kind]);
      await settle();
      expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
      signInAsOtherIdentity();
      expect(signInsStarted()).toBe(0);

      button("Continue").click();

      expect(signInsStarted()).toBe(1);
      expectSignInOnNewSession();
      // The account of the selected identity is not used to authorize the app
      // as the identity that happens to be signed in.
      await settle();
      expect(
        lastUsedIdentitiesStoreValue.setAuthorizeOptions,
      ).not.toHaveBeenCalled();
      expect(onAuthorize).not.toHaveBeenCalled();
    });

    it("signs in as the selected identity when an account is created while another one is signed in", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(sessionActor());
      render(identities[kind]);
      await settle();
      const actor = signInAsOtherIdentity();
      button("Add another account").click();
      flushSync();
      const name = element(
        'input[aria-label="Account name"]',
        HTMLInputElement,
      );
      name.value = "Demo";
      name.dispatchEvent(new Event("input", { bubbles: true }));
      flushSync();
      expect(signInsStarted()).toBe(0);

      button("Create account").click();

      expect(signInsStarted()).toBe(1);
      expectSignInOnNewSession();
      // The account is not created for the identity that happens to be
      // signed in.
      await settle();
      expect(actor.create_account).not.toHaveBeenCalled();
    });

    it("signs in as the selected identity when the default account is changed while another one is signed in", async () => {
      localStorage.setItem(TOGGLE_KEY, "1");
      actorForIdentityMock.mockResolvedValue(sessionActor());
      render(identities[kind]);
      await settle();
      const actor = signInAsOtherIdentity();
      element('button[aria-label="Edit Work"]', HTMLButtonElement).click();
      flushSync();
      element('dialog input[type="checkbox"]', HTMLInputElement).click();
      flushSync();
      expect(signInsStarted()).toBe(0);

      button("Save changes").click();

      expect(signInsStarted()).toBe(1);
      expectSignInOnNewSession();
      // The default account of the identity that happens to be signed in is
      // left alone.
      await settle();
      expect(actor.set_default_account).not.toHaveBeenCalled();
      expect(actor.update_account).not.toHaveBeenCalled();
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
    expect(signInsStarted()).toBe(0);
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
    flushSync();
    button("Continue").click();
    await settle();

    expect(onAuthorize).toHaveBeenCalledTimes(1);
  });

  it("leaves Continue inert for an identity whose accounts load, selected before the sign-in for another's options failed", async () => {
    localStorage.setItem(TOGGLE_KEY, "1");
    localStorage.setItem(OTHER_TOGGLE_KEY, "1");
    const discovery = deferred<never>();
    discoverSsoConfigMock.mockReturnValue(discovery.promise);
    actorForIdentityMock.mockResolvedValueOnce(undefined);
    render(identities.SSO);
    await settle();
    button("Continue").click();
    expect(signInsStarted()).toBe(1);
    // The identity switched to has a stored session; its accounts are on
    // their way.
    const actor = sessionActor();
    actor.get_accounts.mockReturnValue(new Promise(() => {}));
    actorForIdentityMock.mockResolvedValue(actor);
    select({ ...identities.SSO, identityNumber: OTHER_IDENTITY_NUMBER });
    await settle();
    expect(actor.get_accounts).toHaveBeenCalledTimes(1);

    discovery.reject(new Error("SSO discovery failed"));
    await settle();

    // The failed sign-in was for the options of the identity before: those
    // of this one do not wait for a sign-in because of it.
    expect(handleErrorMock).toHaveBeenCalledTimes(1);
    expect(button("Continue").getAttribute("aria-disabled")).toBe("true");
    button("Continue").click();
    expect(signInsStarted()).toBe(1);
  });

  it("continues with one press when the sign-in for the options fails after the accounts arrived", async () => {
    localStorage.setItem(TOGGLE_KEY, "1");
    const discovery = deferred<never>();
    discoverSsoConfigMock.mockReturnValue(discovery.promise);
    const actor = sessionActor();
    const answer = deferred<{ Ok: unknown[] }>();
    actor.get_accounts.mockReturnValue(answer.promise);
    actorForIdentityMock.mockResolvedValueOnce(actor);
    render(identities.SSO);
    await settle();
    // Another identity is selected and then this one again, by which time
    // its stored session is gone: the options wait for a sign-in.
    actorForIdentityMock.mockReturnValue(new Promise(() => {}));
    select({ ...identities.SSO, identityNumber: OTHER_IDENTITY_NUMBER });
    actorForIdentityMock.mockResolvedValue(undefined);
    select(identities.SSO);
    await settle();
    button("Continue").click();
    expect(signInsStarted()).toBe(1);
    await settle();

    // The accounts asked for at first arrive behind the pop-up, and then the
    // sign-in fails.
    answer.resolve({ Ok: [primaryAccount, workAccount] });
    await settle();
    discovery.reject(new Error("SSO discovery failed"));
    await settle();
    expect(handleErrorMock).toHaveBeenCalledTimes(1);
    expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
    authenticationStoreValue.set({ identityNumber: IDENTITY_NUMBER, actor });
    flushSync();
    button("Continue").click();
    await settle();

    expect(onAuthorize).toHaveBeenCalledTimes(1);
    expect(actor.get_accounts).toHaveBeenCalledTimes(1);
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

  it("leaves the options on when a sign-in fails that the switch started before it was switched off and on again", async () => {
    const discovery = deferred<never>();
    discoverSsoConfigMock.mockReturnValueOnce(discovery.promise);
    actorForIdentityMock.mockResolvedValue(undefined);
    render(identities.SSO);
    await settle();
    showAllOptions().click();
    flushSync();
    showAllOptions().click();
    flushSync();
    showAllOptions().click();
    flushSync();
    expect(signInsStarted()).toBe(2);

    discovery.reject(new Error("SSO discovery failed"));
    await settle();

    // The second sign-in is still on its way, for options that are still on.
    expect(handleErrorMock).toHaveBeenCalledTimes(1);
    expect(showAllOptions().checked).toBe(true);
    expect(localStorage.getItem(TOGGLE_KEY)).toBe("1");
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

    authenticationStoreValue.set({ identityNumber: IDENTITY_NUMBER, actor });
    flushSync();
    button("Continue").click();
    await settle();

    expect(onAuthorize).toHaveBeenCalledTimes(1);
    expect(actor.get_accounts).toHaveBeenCalledTimes(1);
  });

  it("shows the options as the screen comes up signed in, and continues with one press", async () => {
    localStorage.setItem(TOGGLE_KEY, "1");
    const actor = sessionActor();
    authenticationStoreValue.set({ identityNumber: IDENTITY_NUMBER, actor });
    // What the lookup answers with while signed in as this identity.
    actorForIdentityMock.mockResolvedValue(actor);
    render(identities.OpenID);
    await settle();

    expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
    button("Continue").click();
    await settle();

    expect(signInsStarted()).toBe(0);
    expect(onAuthorize).toHaveBeenCalledTimes(1);
  });

  it("does not keep the actor of a sign-in as a stored session once that sign-in is gone", async () => {
    const actor = sessionActor();
    authenticationStoreValue.set({ identityNumber: IDENTITY_NUMBER, actor });
    actorForIdentityMock.mockResolvedValue(actor);
    render(identities.OpenID);
    await settle();
    authenticationStoreValue.set(undefined);
    flushSync();
    expect(signInsStarted()).toBe(0);

    showAllOptions().click();

    expect(signInsStarted()).toBe(1);
    expect(actor.get_accounts).not.toHaveBeenCalled();
  });

  it("opens the pop-up of an OpenID identity with the nonce of the session it has reset to", () => {
    render(identities.OpenID);

    button("Continue").click();

    expect(
      new URL(String(open.mock.calls[0][0])).searchParams.get("nonce"),
    ).toBe(sessionStoreValue.nonceAfterReset);
  });

  it("shows the accounts of the selected identity from its stored session while another one is signed in", async () => {
    const storedSessionActor = sessionActor();
    actorForIdentityMock.mockResolvedValue(storedSessionActor);
    render(identities.OpenID);
    await settle();
    const actor = signInAsOtherIdentity();

    showAllOptions().click();
    await settle();

    expect(signInsStarted()).toBe(0);
    expect(storedSessionActor.get_accounts).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      "https://app.example",
    );
    expect(actor.get_accounts).not.toHaveBeenCalled();
    expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
  });

  it("continues as the selected identity once it has signed in, with another one signed in before", async () => {
    const { signIn } = pendingPasskeySignIn();
    actorForIdentityMock.mockResolvedValue(undefined);
    render(passkeyIdentity);
    await settle();
    const actor = signInAsOtherIdentity();
    actor.get_default_account.mockResolvedValue({ Ok: workAccount });

    button("Continue").click();
    await settle();
    expect(authenticateWithPasskeyMock).toHaveBeenCalledTimes(1);
    expect(authenticateWithPasskeyMock.mock.calls[0][0].session.nonce).toBe(
      sessionStoreValue.nonceAfterReset,
    );
    expect(actor.get_default_account).not.toHaveBeenCalled();
    expect(onAuthorize).not.toHaveBeenCalled();

    signIn.resolve(passkeySignInAs(IDENTITY_NUMBER));
    await settle();

    expect(actor.get_default_account).toHaveBeenCalledTimes(1);
    expect(actor.get_default_account).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      "https://app.example",
    );
    expect(onAuthorize).toHaveBeenCalledTimes(1);
    await expect(onAuthorize.mock.calls[0][0]).resolves.toBe(BigInt(1));
  });

  it("does not continue when the sign-in ends after another identity was selected, and signs that one in on the next press", async () => {
    const { signIn, actor } = pendingPasskeySignIn();
    actorForIdentityMock.mockResolvedValue(undefined);
    render(passkeyIdentity);
    await settle();
    button("Continue").click();
    await settle();
    expect(authenticateWithPasskeyMock).toHaveBeenCalledTimes(1);

    select({ ...passkeyIdentity, identityNumber: OTHER_IDENTITY_NUMBER });
    signIn.resolve(passkeySignInAs(IDENTITY_NUMBER));
    await settle();

    expect(onAuthorize).not.toHaveBeenCalled();
    expect(actor.get_default_account).not.toHaveBeenCalled();
    expect(handleErrorMock).not.toHaveBeenCalled();
    // The screen of the identity selected since is not left busy, and the
    // sign-in that ended is not that identity's to continue with.
    expect(button("Continue").disabled).toBe(false);
    authenticateWithPasskeyMock.mockReturnValue(new Promise(() => {}));
    button("Continue").click();
    await settle();
    expect(authenticateWithPasskeyMock).toHaveBeenCalledTimes(2);
    expect(onAuthorize).not.toHaveBeenCalled();
  });

  it("shows no accounts when the sign-in for the options ends after another identity was selected", async () => {
    localStorage.setItem(OTHER_TOGGLE_KEY, "1");
    const { signIn, actor } = pendingPasskeySignIn();
    actorForIdentityMock.mockResolvedValue(undefined);
    render(passkeyIdentity);
    await settle();
    showAllOptions().click();
    await settle();
    expect(authenticateWithPasskeyMock).toHaveBeenCalledTimes(1);

    select({ ...passkeyIdentity, identityNumber: OTHER_IDENTITY_NUMBER });
    await settle();
    signIn.resolve(passkeySignInAs(IDENTITY_NUMBER));
    await settle();

    expect(actor.get_accounts).not.toHaveBeenCalled();
    expect(document.body.querySelector('input[type="radio"]')).toBeNull();
    expect(
      lastUsedIdentitiesStoreValue.syncLastUsedAccounts,
    ).not.toHaveBeenCalled();
    expect(handleErrorMock).not.toHaveBeenCalled();
    expect(showAllOptions().checked).toBe(true);
  });

  it("does not continue with an account when the sign-in ends after another identity was selected", async () => {
    localStorage.setItem(TOGGLE_KEY, "1");
    const { signIn } = pendingPasskeySignIn();
    actorForIdentityMock.mockResolvedValueOnce(sessionActor());
    render(passkeyIdentity);
    await settle();
    expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
    button("Continue").click();
    await settle();
    expect(authenticateWithPasskeyMock).toHaveBeenCalledTimes(1);

    // The identity switched to has no session to show its accounts with.
    actorForIdentityMock.mockResolvedValue(undefined);
    select({ ...passkeyIdentity, identityNumber: OTHER_IDENTITY_NUMBER });
    signIn.resolve(passkeySignInAs(IDENTITY_NUMBER));
    await settle();

    expect(
      lastUsedIdentitiesStoreValue.setAuthorizeOptions,
    ).not.toHaveBeenCalled();
    expect(onAuthorize).not.toHaveBeenCalled();
    expect(handleErrorMock).not.toHaveBeenCalled();
    expect(button("Continue").disabled).toBe(false);
  });

  // The canister answers a sign-in with the identity the credential belongs
  // to. For an OpenID or SSO identity that is another one than the sign-in was
  // started for when another account is picked at the provider.
  it("continues with the identity a sign-in returned when that is not the selected one", async () => {
    const { signIn, actor } = pendingPasskeySignIn();
    actorForIdentityMock.mockResolvedValue(undefined);
    render(passkeyIdentity);
    await settle();
    button("Continue").click();
    await settle();

    signIn.resolve(passkeySignInAs(OTHER_IDENTITY_NUMBER));
    await settle();

    expect(actor.get_default_account).toHaveBeenCalledWith(
      OTHER_IDENTITY_NUMBER,
      "https://app.example",
    );
    expect(onAuthorize).toHaveBeenCalledTimes(1);
  });

  it("signs in again on the press after a sign-in that returned another identity than the selected one", async () => {
    const { signIn, actor } = pendingPasskeySignIn();
    actorForIdentityMock.mockResolvedValue(undefined);
    render(passkeyIdentity);
    await settle();
    showAllOptions().click();
    await settle();
    signIn.resolve(passkeySignInAs(OTHER_IDENTITY_NUMBER));
    await settle();
    expect(actor.get_accounts).toHaveBeenCalledWith(
      OTHER_IDENTITY_NUMBER,
      "https://app.example",
    );
    expect(document.body.querySelector('input[type="radio"]')).not.toBeNull();
    expect(authenticateWithPasskeyMock).toHaveBeenCalledTimes(1);
    authenticateWithPasskeyMock.mockReturnValue(new Promise(() => {}));

    button("Continue").click();
    await settle();

    expect(authenticateWithPasskeyMock).toHaveBeenCalledTimes(2);
    expect(onAuthorize).not.toHaveBeenCalled();
  });
});
