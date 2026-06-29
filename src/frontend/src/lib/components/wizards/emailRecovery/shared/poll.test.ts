import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type {
  EmailChallengeDiagnostics,
  EmailChallengeError,
  EmailChallengeStatus,
} from "$lib/generated/internet_identity_types";
import { Funnel } from "$lib/utils/analytics/Funnel";
import { CanisterError } from "$lib/utils/utils";
import { runEmailRecoveryPoll, type EmailRecoveryPollDeps } from "./poll";
import { EXPIRED_MESSAGE } from "./errors";
import { assembleDkimResolution } from "$lib/utils/dnssec";

// The funnel hits analytics on every trigger/close; stub it out.
vi.mock("$lib/utils/analytics/analytics", () => ({
  analytics: { event: vi.fn() },
}));

// The DNSSEC leaf walk is exercised by the NeedDkimLeaf branch. Each test
// pins its outcome (a signed resolution, `undefined` for the unsigned-zone
// fallback, or a throw).
vi.mock("$lib/utils/dnssec", () => ({
  assembleDkimResolution: vi.fn(),
}));
const walkMock = vi.mocked(assembleDkimResolution);

const TestEvents = {
  needDkimLeaf: "need-dkim-leaf",
  dkimLeafSubmitted: "dkim-leaf-submitted",
  failed: "failed",
  unsupportedDomain: "unsupported-domain",
} as const;

const NONCE = "nonce-123";
const DOMAIN = "example.com";

// Status shorthands.
const PENDING: EmailChallengeStatus = { Pending: null };
const RESOLVING: EmailChallengeStatus = { ResolvingDoh: null };
const NEED_LEAF: EmailChallengeStatus = {
  NeedDkimLeaf: { selector: "sel" },
};
const REG_OK: EmailChallengeStatus = { RegistrationSucceeded: null };
const failed = (reason: EmailChallengeError): EmailChallengeStatus => ({
  Failed: reason,
});

const canisterErr = (reason: EmailChallengeError) => new CanisterError(reason);
const transportErr = () => new Error("network down");

interface Harness {
  deps: EmailRecoveryPollDeps<typeof TestEvents>;
  funnel: Funnel<typeof TestEvents>;
  statusFn: ReturnType<typeof vi.fn>;
  submitDkimLeaf: ReturnType<typeof vi.fn>;
  resolveViaDoh: ReturnType<typeof vi.fn>;
  toUnsupported: ReturnType<typeof vi.fn>;
  toFailed: ReturnType<typeof vi.fn>;
  setPolling: ReturnType<typeof vi.fn>;
}

/**
 * Build a harness. `statuses` is the queue `email_challenge_status` returns,
 * one per poll tick; once drained it keeps yielding the last entry so a loop
 * that should have terminated on its own can't run dry. `isActive` stays true
 * (every happy-path test terminates by returning out of the loop), unless a
 * test overrides it to exercise the cooperative-stop path.
 */
const harness = (
  statuses: EmailChallengeStatus[],
  overrides: Partial<EmailRecoveryPollDeps<typeof TestEvents>> = {},
): Harness => {
  const funnel = new Funnel<typeof TestEvents>("test");
  vi.spyOn(funnel, "trigger");
  vi.spyOn(funnel, "close");

  const queue = [...statuses];
  const statusFn = vi.fn((): Promise<EmailChallengeStatus> => {
    const next = queue.length > 1 ? queue.shift() : queue[0];
    return Promise.resolve(next ?? PENDING);
  });
  const submitDkimLeaf = vi.fn((): Promise<void> => Promise.resolve());
  const resolveViaDoh = vi.fn((): Promise<void> => Promise.resolve());
  const toUnsupported = vi.fn();
  const toFailed = vi.fn();
  const setPolling = vi.fn();

  const deps: EmailRecoveryPollDeps<typeof TestEvents> = {
    nonce: NONCE,
    domain: DOMAIN,
    status: statusFn,
    submitDkimLeaf,
    resolveViaDoh,
    diagnostics: vi.fn(
      (): Promise<[] | [EmailChallengeDiagnostics]> => Promise.resolve([]),
    ),
    funnel,
    events: {
      needDkimLeaf: TestEvents.needDkimLeaf,
      dkimLeafSubmitted: TestEvents.dkimLeafSubmitted,
      failed: TestEvents.failed,
      unsupportedDomain: TestEvents.unsupportedDomain,
    },
    handleSuccess: (s: EmailChallengeStatus) =>
      "RegistrationSucceeded" in s || "RecoveryReady" in s,
    isActive: () => true,
    setPolling,
    toUnsupported,
    toFailed,
    ...overrides,
  };

  return {
    deps,
    funnel,
    statusFn,
    submitDkimLeaf,
    resolveViaDoh,
    toUnsupported,
    toFailed,
    setPolling,
  };
};

/** Drive the poll to completion, flushing the `setTimeout` backoffs. */
const run = async (deps: EmailRecoveryPollDeps<typeof TestEvents>) => {
  const p = runEmailRecoveryPoll(deps);
  await vi.runAllTimersAsync();
  await p;
};

describe("runEmailRecoveryPoll", () => {
  beforeEach(() => {
    vi.useFakeTimers();
    walkMock.mockReset();
  });
  afterEach(() => {
    vi.runOnlyPendingTimers();
    vi.useRealTimers();
    vi.clearAllMocks();
  });

  it("polls through Pending until a terminal success, then stops", async () => {
    const h = harness([PENDING, PENDING, REG_OK]);
    await run(h.deps);

    expect(h.statusFn.mock.calls.length).toBeGreaterThanOrEqual(3);
    expect(h.toFailed).not.toHaveBeenCalled();
    expect(h.toUnsupported).not.toHaveBeenCalled();
    // The flag is always cleared on the way out (the `finally`).
    expect(h.setPolling).toHaveBeenLastCalledWith(false);
  });

  it("stops as soon as handleSuccess accepts the status", async () => {
    const recoveryReady: EmailChallengeStatus = {
      RecoveryReady: {
        user_key: new Uint8Array(),
        expiration: BigInt(0),
        anchor_number: BigInt(0),
      },
    };
    const h = harness([recoveryReady, REG_OK]);
    await run(h.deps);

    // Stopped on the first status — the second was never polled.
    expect(h.statusFn).toHaveBeenCalledTimes(1);
    expect(h.toFailed).not.toHaveBeenCalled();
  });

  it("drives resolveViaDoh once per ResolvingDoh tick, idempotently", async () => {
    const h = harness([RESOLVING, RESOLVING, REG_OK]);
    await run(h.deps);

    expect(h.resolveViaDoh).toHaveBeenCalledTimes(2);
    expect(h.resolveViaDoh).toHaveBeenCalledWith(NONCE);
    expect(h.toFailed).not.toHaveBeenCalled();
  });

  it("routes a canister Err thrown by resolveViaDoh as a terminal failure", async () => {
    const h = harness([RESOLVING, REG_OK], {
      resolveViaDoh: vi.fn(
        (): Promise<void> =>
          Promise.reject(canisterErr({ EmailVerificationFailed: "bad sig" })),
      ),
    });
    await run(h.deps);

    expect(h.toFailed).toHaveBeenCalledTimes(1);
    expect(h.toUnsupported).not.toHaveBeenCalled();
    // It stopped — the success status after it was never reached.
    expect(h.statusFn).toHaveBeenCalledTimes(1);
  });

  it("routes a DomainNotAllowlisted canister Err from resolveViaDoh to the unsupported view", async () => {
    const h = harness([RESOLVING], {
      resolveViaDoh: vi.fn(
        (): Promise<void> =>
          Promise.reject(canisterErr({ DomainNotAllowlisted: DOMAIN })),
      ),
    });
    await run(h.deps);

    expect(h.toUnsupported).toHaveBeenCalledWith(DOMAIN);
    expect(h.toFailed).not.toHaveBeenCalled();
  });

  it("surfaces a retryable failure when resolveViaDoh throws a transport error", async () => {
    const h = harness([RESOLVING], {
      resolveViaDoh: vi.fn((): Promise<void> => Promise.reject(transportErr())),
    });
    await run(h.deps);

    expect(h.toFailed).toHaveBeenCalledTimes(1);
    expect(h.toFailed.mock.calls[0][0]).toContain(
      "couldn't reach Internet Identity",
    );
  });

  it("walks the DKIM leaf and submits it on the DNSSEC path", async () => {
    walkMock.mockResolvedValue({ hops: [], extraChains: [] });
    const h = harness([NEED_LEAF, REG_OK]);
    await run(h.deps);

    expect(walkMock).toHaveBeenCalledWith(DOMAIN, "sel");
    expect(h.submitDkimLeaf).toHaveBeenCalledTimes(1);
    expect(h.submitDkimLeaf).toHaveBeenCalledWith({
      nonce: NONCE,
      hops: [],
      extra_chains: [],
    });
    expect(h.resolveViaDoh).not.toHaveBeenCalled();
  });

  it("falls back to resolveViaDoh when the leaf walk yields undefined (unsigned zone)", async () => {
    walkMock.mockResolvedValue(undefined);
    const h = harness([NEED_LEAF, REG_OK]);
    await run(h.deps);

    expect(h.submitDkimLeaf).not.toHaveBeenCalled();
    expect(h.resolveViaDoh).toHaveBeenCalledWith(NONCE);
  });

  it("submits the DKIM leaf at most once across repeated NeedDkimLeaf ticks", async () => {
    walkMock.mockResolvedValue({ hops: [], extraChains: [] });
    const h = harness([NEED_LEAF, NEED_LEAF, REG_OK]);
    await run(h.deps);

    expect(h.submitDkimLeaf).toHaveBeenCalledTimes(1);
  });

  it("routes a canister Err thrown by submitDkimLeaf as a terminal failure", async () => {
    walkMock.mockResolvedValue({ hops: [], extraChains: [] });
    const h = harness([NEED_LEAF, REG_OK], {
      submitDkimLeaf: vi.fn(
        (): Promise<void> =>
          Promise.reject(canisterErr({ DkimLeafMismatch: null })),
      ),
    });
    await run(h.deps);

    expect(h.toFailed).toHaveBeenCalledTimes(1);
    expect(h.statusFn).toHaveBeenCalledTimes(1);
  });

  it("surfaces a retryable failure when submitDkimLeaf throws a transport error", async () => {
    walkMock.mockResolvedValue({ hops: [], extraChains: [] });
    const h = harness([NEED_LEAF, REG_OK], {
      submitDkimLeaf: vi.fn(
        (): Promise<void> => Promise.reject(transportErr()),
      ),
    });
    await run(h.deps);

    expect(h.toFailed).toHaveBeenCalledTimes(1);
    expect(h.toFailed.mock.calls[0][0]).toContain(
      "couldn't reach Internet Identity",
    );
  });

  it("routes a generic Failed status to the failed view", async () => {
    const h = harness([failed({ AddressMismatch: null })]);
    await run(h.deps);

    expect(h.toFailed).toHaveBeenCalledTimes(1);
    expect(h.toUnsupported).not.toHaveBeenCalled();
  });

  it("routes a DomainNotSupported Failed status to the unsupported view", async () => {
    const h = harness([failed({ DomainNotSupported: DOMAIN })]);
    await run(h.deps);

    expect(h.toUnsupported).toHaveBeenCalledWith(DOMAIN);
    expect(h.toFailed).not.toHaveBeenCalled();
  });

  it("routes an Expired status to the failed view with the expiry message", async () => {
    const h = harness([{ Expired: null }]);
    await run(h.deps);

    expect(h.toFailed).toHaveBeenCalledTimes(1);
    expect(h.toFailed.mock.calls[0][0]).toBe(EXPIRED_MESSAGE);
  });

  it("rides out transient status-poll failures below the threshold", async () => {
    let calls = 0;
    const statusFn = vi.fn((): Promise<EmailChallengeStatus> => {
      calls += 1;
      return calls <= 3
        ? Promise.reject(transportErr())
        : Promise.resolve(REG_OK);
    });
    const h = harness([REG_OK], { status: statusFn });
    await run(h.deps);

    // Three blips then a clean success — the loop recovered, no failure view.
    expect(statusFn).toHaveBeenCalledTimes(4);
    expect(h.toFailed).not.toHaveBeenCalled();
  });

  it("gives up with a retryable failure after MAX consecutive poll errors", async () => {
    const statusFn = vi.fn(
      (): Promise<EmailChallengeStatus> => Promise.reject(transportErr()),
    );
    const h = harness([REG_OK], { status: statusFn });
    await run(h.deps);

    // 5 consecutive failures (MAX_CONSECUTIVE_POLL_ERRORS) then it surfaces.
    expect(statusFn).toHaveBeenCalledTimes(5);
    expect(h.toFailed).toHaveBeenCalledTimes(1);
    expect(h.toFailed.mock.calls[0][0]).toContain(
      "couldn't reach Internet Identity",
    );
  });

  it("clears the polling flag even when the wizard deactivates without a terminal status", async () => {
    let ticks = 0;
    const h = harness([PENDING], {
      isActive: () => {
        ticks += 1;
        return ticks <= 2;
      },
    });
    await run(h.deps);

    expect(h.setPolling).toHaveBeenLastCalledWith(false);
    expect(h.toFailed).not.toHaveBeenCalled();
  });
});
