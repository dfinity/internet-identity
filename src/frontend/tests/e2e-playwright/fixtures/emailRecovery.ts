/**
 * Test fixture for the email-recovery feature.
 *
 * Covers two layers of testing in one fixture:
 *
 * - **Wizard surface** — flag toggling, card visibility, opening the
 *   setup / recovery wizards. These tests just exercise the FE
 *   wiring and don't touch the canister.
 *
 * - **Real end-to-end flow** — synthesizes a DNSSEC chain
 *   (`dnssecTestSigner`) plus a DKIM keypair (`dkimTestSigner`),
 *   intercepts the FE's DoH lookups (`dohRouteInterceptor`), and
 *   submits the canister-issued nonce email through `smtp_request`.
 *   The local II canister is deployed with a trust anchor matching
 *   the signer's deterministic seed (see `local_test_arg.did` and
 *   the workflow at `.github/workflows/canister-tests.yml`).
 *
 * The DNSSEC + DKIM material is built lazily on first use, so tests
 * that only exercise the wizard surface don't pay the keygen cost.
 */

import { test as base, expect, Locator, Page } from "@playwright/test";
import { Actor, AnonymousIdentity, HttpAgent } from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import { Agent as UndiciAgent } from "undici";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import type {
  _SERVICE,
  DnsProofBundle,
} from "$lib/generated/internet_identity_types";

import { II_URL } from "../utils";
import { buildChain } from "../utils/dnssecTestSigner";
import { TestSigner } from "../utils/dkimTestSigner";
import { installDohInterceptor } from "../utils/dohRouteInterceptor";
import { DEFAULT_HOST } from "./identity";

/** Domain the synthesized DNSSEC chain authenticates. */
const TEST_DOMAIN = "test.example.com";
/** DKIM selector the synthesized email is signed under. With the
 *  two-phase flow the canister parses `s=` from the email and tells
 *  the FE which leaf to walk — we no longer probe a candidate list,
 *  so any selector value works as long as the test fixture signs
 *  with it. */
const TEST_SELECTOR = "mail";

/**
 * Per-test "From" address. The canister keeps a global
 * `address → anchor` reverse index, so two tests reusing the same
 * address in the same canister deployment will collide on the
 * second `bind_credential_to_anchor` call. We mint a fresh
 * local-part per fixture instance so tests stay isolated.
 */
function makeFromAddress(): string {
  const rand = Math.random().toString(36).slice(2, 10);
  return `alice-${rand}@${TEST_DOMAIN}`;
}

/**
 * Inject `localStorage` overrides for the email-recovery feature
 * flag before the page's JS runs. The runtime flag store at
 * `lib/state/featureFlags.ts` reads from this key on init, so this
 * is enough to flip the flag without faking the page hostname.
 *
 * The corresponding key shape is the runtime constant
 * `LOCALSTORAGE_FEATURE_FLAGS_PREFIX + name` from `featureFlags.ts`.
 * We hardcode the literal here on purpose — if the runtime prefix
 * changes we want the test to fail loudly.
 */
async function setEmailRecoveryFlag(page: Page, on: boolean) {
  await page.addInitScript(
    ({ value }) => {
      try {
        window.localStorage.setItem(
          "ii-localstorage-feature-flags__EMAIL_RECOVERY",
          JSON.stringify(value),
        );
      } catch {
        // localStorage may be locked in some test contexts.
      }
    },
    { value: on },
  );
}

class EmailRecoveryWizard {
  readonly #view: Locator;

  constructor(view: Locator) {
    this.#view = view;
  }

  /** Step 1: type address and continue. */
  async enterAddress(address: string): Promise<void> {
    await expect(
      this.#view.getByRole("heading", { name: "Add a recovery email" }),
    ).toBeVisible();
    await this.#view
      .getByRole("textbox", { name: "Email address" })
      .fill(address);
    await this.#view.getByRole("button", { name: "Continue" }).click();
  }

  /** Step 2: assert the verify-email view rendered with the right
   *  recipient and a valid-looking nonce in the Subject. */
  async expectVerifyEmailView(opts: {
    recipient: "register@id.ai" | "recover@id.ai";
  }): Promise<void> {
    await expect(
      this.#view.getByRole("heading", { name: "Verify your email" }),
    ).toBeVisible();
    await expect(this.#view.getByText(opts.recipient)).toBeVisible();
    // The Subject token is rendered in a monospaced span; assert
    // that the canister-issued shape matches the prefix.
    await expect(
      this.#view.getByText(/II-Recovery-[0-9a-f]{16}/),
    ).toBeVisible();
  }

  async close(): Promise<void> {
    await this.#view.getByRole("button", { name: "Close" }).click();
  }
}

/** Self-signed-cert tolerant fetch — same setup as `utils.ts`. */
const insecureUndici = new UndiciAgent({
  connect: { rejectUnauthorized: false },
});
const insecureFetch: typeof fetch = (url, options = {}) =>
  fetch(url, { dispatcher: insecureUndici, ...options } as RequestInit);

async function readCanisterId(page: Page): Promise<Principal> {
  const el = await page.$("[data-canister-id]");
  if (el === null) {
    throw new Error("page does not have [data-canister-id]");
  }
  const text = await el.getAttribute("data-canister-id");
  if (text === null) {
    throw new Error("data-canister-id attribute is empty");
  }
  return Principal.fromText(text);
}

async function makeAnonymousActor(
  canisterId: Principal,
): Promise<ReturnType<typeof Actor.createActor<_SERVICE>>> {
  const agent = await HttpAgent.create({
    host: DEFAULT_HOST,
    shouldFetchRootKey: true,
    verifyQuerySignatures: false,
    fetch: insecureFetch,
    identity: new AnonymousIdentity(),
  });
  return Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent,
    canisterId,
  });
}

interface EmailRecoveryCtx {
  /** Skeleton bundle (chain only, `leaf = []`) for prepare-time. */
  skeleton: DnsProofBundle;
  /** Signed DKIM TXT leaf for `<selector>._domainkey.<domain>` —
   *  the DoH layer serves it; the FE walks it post-email. */
  dkimLeaf: import("$lib/generated/internet_identity_types").SignedRRset;
  /** DKIM signer used to produce the test email. */
  dkim: TestSigner;
}

class EmailRecoveryFixtures {
  readonly #page: Page;
  /** Lazy: built on first call to `installDohInterceptor` / `submitEmail`. */
  #ctx: Promise<EmailRecoveryCtx> | undefined;

  /** Domain the synthesized DNSSEC chain authenticates. */
  readonly domain = TEST_DOMAIN;
  /** Selector the test fixture signs the email with. The canister
   *  reads it back from the DKIM-Signature header and tells the FE
   *  which leaf to walk; no probing involved. */
  readonly selector = TEST_SELECTOR;
  /** "From" address used in both setup and recovery emails. Unique
   *  per test so reruns against a long-lived canister don't collide
   *  on the global address → anchor reverse index. */
  readonly fromAddress = makeFromAddress();

  constructor(page: Page) {
    this.#page = page;
  }

  // ---------------------------------------------------------------
  // Flag + card surface — used by every email-recovery test
  // ---------------------------------------------------------------

  async enableFlag(): Promise<void> {
    await setEmailRecoveryFlag(this.#page, true);
  }

  async disableFlag(): Promise<void> {
    await setEmailRecoveryFlag(this.#page, false);
  }

  async assertSetupCardVisible(): Promise<void> {
    await expect(
      this.#page.getByRole("heading", { name: "Recovery email" }),
    ).toBeVisible();
    await expect(
      this.#page.getByRole("button", { name: "Add email" }),
    ).toBeVisible();
  }

  async assertSetupCardHidden(): Promise<void> {
    // The phrase card always shows; assert specifically that the
    // *email* sub-card isn't rendered.
    await expect(
      this.#page.getByRole("heading", { name: "Recovery email" }),
    ).toBeHidden();
  }

  async assertRecoverWithEmailHidden(): Promise<void> {
    await expect(
      this.#page.getByRole("button", { name: "Recover with email" }),
    ).toBeHidden();
  }

  /**
   * Open the setup wizard from the manage-page card and run `fn`
   * against it. The wizard is closed (Cancel or Done) by `fn`.
   */
  async openSetupWizard<T>(
    fn: (wizard: EmailRecoveryWizard) => Promise<T>,
  ): Promise<T> {
    await this.#page
      .getByRole("main")
      .getByRole("button", { name: "Add email" })
      .click();
    const dialog = this.#page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    return fn(new EmailRecoveryWizard(dialog));
  }

  /**
   * Open the recover-with-email wizard from the recovery sign-in
   * page and run `fn` against it. The recovery wizard reuses the
   * same SendMagicEmail view; the only differences for our purposes
   * are the heading on step 1 and the recipient on step 2.
   */
  async openRecoveryWizard<T>(
    fn: (wizard: EmailRecoveryWizard) => Promise<T>,
  ): Promise<T> {
    await this.#page
      .getByRole("button", { name: "Recover with email" })
      .click();
    const dialog = this.#page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    return fn(new EmailRecoveryWizard(dialog));
  }

  // ---------------------------------------------------------------
  // Real end-to-end machinery — DNSSEC chain + DKIM keypair + DoH
  // interception + smtp_request. Lazy.
  // ---------------------------------------------------------------

  /** Wire up `page.route()` handlers for both DoH endpoints to serve
   *  bytes from the freshly-built DNSSEC chain. Idempotent. The
   *  skeleton chain is exposed via the bundle; the DKIM leaf is
   *  exposed as an extra so the FE's post-email walk finds it. */
  async installDohInterceptor(): Promise<void> {
    const { skeleton, dkimLeaf } = await this.#ensureCtx();
    await installDohInterceptor(this.#page, skeleton, [dkimLeaf]);
  }

  /**
   * Sign and submit a DKIM-signed email through the canister's
   * `smtp_request` method. Triggers the setup-completion
   * (`register@id.ai`) and recovery (`recover@id.ai`) paths.
   *
   * Reads the canister id from the currently-loaded page so the
   * actor talks to the same canister the wizard is polling.
   */
  async submitEmail(params: {
    to: "register@id.ai" | "recover@id.ai";
    subject: string;
    body?: string;
  }): Promise<void> {
    const { dkim } = await this.#ensureCtx();
    const signed = await dkim.signEmail({
      from: this.fromAddress,
      to: params.to,
      subject: params.subject,
      body: new TextEncoder().encode(params.body ?? "Hello.\r\n"),
      timestamp: Math.floor(Date.now() / 1000),
    });
    const canisterId = await readCanisterId(this.#page);
    const actor = await makeAnonymousActor(canisterId);
    const resp = await actor.smtp_request(signed.request);
    if ("Err" in resp) {
      throw new Error(
        `smtp_request returned Err: ${resp.Err.message} (code ${resp.Err.code})`,
      );
    }
  }

  /** Build the DNSSEC chain + DKIM keypair the first time the test
   *  reaches for the network-side machinery. ~200ms — RSA keygen. */
  #ensureCtx(): Promise<EmailRecoveryCtx> {
    if (this.#ctx === undefined) {
      this.#ctx = (async () => {
        const dkim = await TestSigner.create(TEST_DOMAIN, TEST_SELECTOR);
        const nowSecs = Math.floor(Date.now() / 1000);
        const { skeleton, dkimLeaf } = await buildChain({
          domain: TEST_DOMAIN,
          selector: TEST_SELECTOR,
          dkimTxt: dkim.publicTxtRecord(),
          // No DMARC — the canister-side flow accepts strict DMARC
          // alignment when no DMARC record is published.
          nowSecs,
        });
        return { skeleton, dkimLeaf, dkim };
      })();
    }
    return this.#ctx;
  }
}

export const test = base.extend<{
  emailRecovery: EmailRecoveryFixtures;
}>({
  emailRecovery: async ({ page }, use) => {
    await use(new EmailRecoveryFixtures(page));
  },
});

export { II_URL };
