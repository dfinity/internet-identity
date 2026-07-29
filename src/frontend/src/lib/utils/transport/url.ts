/**
 * ICRC-167 browser URL transport (signer side).
 *
 * A third {@link Transport} alongside {@link PostMessageTransport} and
 * {@link LegacyTransport}. Where those keep a live `postMessage` channel to a
 * relying party (RP) that opened II in a separate window, this one serves an RP
 * that navigated the *current* tab to II (a top-level redirect): the request
 * arrives in this page's URL hash fragment, and the response is delivered by
 * navigating the tab to the RP's callback with the response in *its* fragment.
 *
 * See https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_167_browser_url_transport.md
 *
 * The transport is self-contained: the ICRC method handlers
 * (`stores/channelHandlers/*`) drive it purely through the {@link Channel}
 * interface and are unaware of it. The only integration point is
 * `channelStore.getTransports()`, which enters it into the establish race.
 *
 * Two things are specific to a redirect (and handled entirely here, so the rest
 * of II stays transport-agnostic):
 *
 *  - **Callback trust.** postMessage gets a browser-verified `event.origin`; a
 *    redirect has none, so trust is anchored on the `callback`: the response is
 *    delivered only to a URL the callback's own origin declares in its
 *    `/.well-known/ii-auth-callbacks` allow-list (see {@link matchDeclaredCallback}),
 *    and `channel.origin` is that callback origin — so the delegation is scoped
 *    to whoever owns the callback, and the existing derivation-origin check in
 *    the delegation handler still applies unchanged.
 *
 *  - **Intermediate-key middleware for delegations.** A delegation returned
 *    over a redirect must not be certified directly against the RP-supplied
 *    session key: what the canister signs transits the IC (readable from the
 *    replica/boundary), so it must be inert on its own. {@link interceptDelegation}
 *    swaps the RP session key for a fresh ephemeral key on the way in — the
 *    canister only ever delegates to that key — and extends the chain with a
 *    second hop to the RP session key on the way out. The only redeemable chain
 *    is assembled here and leaves exclusively via the callback fragment. This
 *    mirrors the redirect path of {@link LegacyChannel}, but as a pure
 *    request/response transform decoupled from the redirect itself.
 */
import { z } from "zod";
import {
  type Channel,
  type ChannelOptions,
  type JsonRequest,
  JsonRequestSchema,
  type JsonResponse,
  JsonResponseSchema,
  type Transport,
  DelegationParamsCodec,
  DelegationResultSchema,
  GENERIC_ERROR_CODE,
} from "$lib/utils/transport/utils";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import type { PublicKey } from "@icp-sdk/core/agent";
import { matchDeclaredCallback } from "$lib/utils/authCallbacks";
import { frontendCanisterConfig } from "$lib/globals";
import {
  createStore,
  get as idbGet,
  set as idbSet,
  delMany as idbDelMany,
  entries as idbEntries,
} from "idb-keyval";

/** Hash-fragment parameter carrying the JSON-RPC request (in) / response (out). */
const MESSAGE_PARAM = "message";
/** Hash-fragment parameter carrying the RP callback URL (request only). */
const CALLBACK_PARAM = "callback";
/** Hash-fragment parameter the RP uses to correlate the response with its request. */
const STATE_PARAM = "state";

/** Upper bound on the raw `message` param. A delegation params blob plus
 *  batched attributes is well under 1 KB, so this is generous headroom while
 *  bounding the work a crafted link can impose before any user interaction
 *  (parsing, per-request keygen, IndexedDB writes). */
const MAX_MESSAGE_LENGTH = 16 * 1024;
/** Upper bound on batch size. The real 1-click batch is 2 (`icrc34_delegation`
 *  + `ii-icrc3-attributes`); anything approaching this is abuse. */
const MAX_BATCH_SIZE = 10;

// The outer (intermediate -> RP session key) delegation lasts 30 days; the
// chain still expires earlier if the inner canister-signed hop does. Matches
// the legacy redirect flow (transport/legacy.ts).
const OUTER_DELEGATION_EXPIRATION_MS = 30 * 24 * 60 * 60 * 1000;

/** The response for a batch request left unanswered because an earlier request
 *  in the same batch errored and aborted the flow. */
const abortedBatchError = (id: string | number): JsonResponse => ({
  jsonrpc: "2.0",
  id,
  error: {
    code: GENERIC_ERROR_CODE,
    message:
      "Request not processed: the batch was aborted by an earlier error.",
  },
});

/** Thrown when the current load is not an ICRC-167 request, so the establish
 *  race falls through to the other transports. */
export class UrlTransportUnsupportedError extends Error {}

/** Whether `host` is a loopback host, per the secure-context definition:
 *  `localhost`, `*.localhost`, the `127.0.0.0/8` range, or `[::1]`. */
const isLoopbackHost = (host: string): boolean =>
  host === "localhost" ||
  host.endsWith(".localhost") ||
  host === "[::1]" ||
  /^127(?:\.\d{1,3}){3}$/.test(host);

/**
 * Whether the callback URL is a *secure context*, mirroring how browsers
 * decide it: `https`, or `http` only on a loopback host. Everything else — a
 * `javascript:`/`data:` scheme, or plain `http` on a remote host — is not a
 * secure context and is rejected as a redirect target.
 *
 * The loopback (`http`) allowance is further gated on the `dev_csp` canister
 * config. Delivering the response is a same-tab navigation (not CSP-bound), but
 * establishing the flow first fetches the callback origin's allow-list, and
 * that fetch is a `connect-src` request: II's production CSP allows only
 * `https:` there, so an `http` loopback allow-list can only be fetched when the
 * canister runs with the dev CSP (`connect-src … http:`). Gating on the same
 * flag keeps a loopback callback from passing this check yet failing the fetch
 * in production.
 * @see https://w3c.github.io/webappsec-secure-contexts/
 */
const isSecureContextUrl = (url: URL): boolean =>
  url.protocol === "https:" ||
  (url.protocol === "http:" &&
    isLoopbackHost(url.hostname) &&
    frontendCanisterConfig.dev_csp[0] === true);

interface UrlRequest {
  /** The JSON-RPC request(s). An array is an ICRC-25 batch, answered in one redirect. */
  requests: JsonRequest[];
  /** Whether the RP sent a batch (array) — the response mirrors this shape. */
  batch: boolean;
  /** Validated RP callback URL the response is delivered to. */
  callback: string;
  /** Opaque correlation value echoed back to the RP. */
  state: string;
}

/**
 * Reads and validates the ICRC-167 request from the current page's URL hash,
 * then strips it from the address bar. Returns `undefined` when there is no
 * `message` in the hash (not an ICRC-167 load) so the caller can defer to the
 * other transports.
 */
const readUrlRequest = (): UrlRequest | undefined => {
  const params = new URLSearchParams(window.location.hash.replace(/^#/, ""));
  const message = params.get(MESSAGE_PARAM);
  const callback = params.get(CALLBACK_PARAM);
  const state = params.get(STATE_PARAM);

  // No message → this is not an ICRC-167 request; let another transport win.
  if (message === null) {
    return undefined;
  }

  // Bound the raw request before parsing: the message is attacker-craftable
  // (anyone can put it in a link), so cap it so an oversized payload can't
  // impose parse/keygen/storage cost before any user interaction.
  if (message.length > MAX_MESSAGE_LENGTH) {
    throw new Error("ICRC-167 request message is too large");
  }

  // Strip the request from the URL so it does not linger in history or leak via
  // the address bar while the flow runs (mirrors the legacy/MCP redirect flows).
  const stripped = new URL(window.location.href);
  stripped.hash = "";
  window.history.replaceState(null, "", stripped);

  if (callback === null || state === null) {
    throw new Error("ICRC-167 request is missing a callback or state");
  }

  // The callback becomes a top-level navigation target, so constrain it up
  // front: it must be a secure-context URL (https, or http on loopback) — never
  // a `javascript:`/`data:` scheme (which would execute in this origin) and
  // never plain http on a remote host — and it must be protocol + host + path
  // only: no query and no fragment (the transport appends its own response
  // fragment). It is further constrained to the relying party's declared
  // allow-list before any delivery (see UrlTransport.establishChannel).
  let callbackUrl: URL;
  try {
    callbackUrl = new URL(callback);
  } catch {
    throw new Error("ICRC-167 callback is not a valid URL");
  }
  if (!isSecureContextUrl(callbackUrl)) {
    throw new Error(
      "ICRC-167 callback must be a secure context (https, or http on loopback)",
    );
  }
  // Protocol + host + path only (like an OpenID redirect_uri): reconstruct the
  // URL from just its origin and path and require it to match. Anything extra —
  // a query, a fragment, or embedded credentials — makes it differ and is
  // rejected, while benign normalization (default port, host case) does not.
  const canonical = new URL(callbackUrl.pathname, callbackUrl.origin);
  if (canonical.href !== callbackUrl.href) {
    throw new Error(
      "ICRC-167 callback must be protocol + host + path only (no query, fragment, or credentials)",
    );
  }

  let parsed: unknown;
  try {
    parsed = JSON.parse(message);
  } catch {
    throw new Error("ICRC-167 request message is not valid JSON");
  }

  const batch = Array.isArray(parsed);
  const values: unknown[] = batch ? (parsed as unknown[]) : [parsed];
  if (values.length > MAX_BATCH_SIZE) {
    throw new Error("ICRC-167 batch has too many requests");
  }
  const requests = values.map((value) => {
    const result = JsonRequestSchema.safeParse(value);
    if (!result.success) {
      throw new Error(
        "ICRC-167 request message is not a valid JSON-RPC request",
      );
    }
    // The response is correlated and delivered by id (the JSON-RPC response
    // schema requires one), and a single redirect carries the whole batch — an
    // id-less request (a JSON-RPC notification) can never be answered, so
    // reject it rather than silently drop it from the delivered response.
    if (result.data.id === undefined) {
      throw new Error(
        "ICRC-167 request must have an id (notifications are not supported)",
      );
    }
    return result.data;
  });

  // Reject duplicate ids (type-tagged, so `1` and `"1"` don't collide). Responses
  // are collected in an id-keyed map and delivered by id, so two requests sharing
  // an id would overwrite each other's finalizer, coalesce early, and duplicate
  // the response in the delivered batch.
  const idKeys = requests.map((request) => JSON.stringify(request.id));
  if (new Set(idKeys).size !== idKeys.length) {
    throw new Error("ICRC-167 request ids must be unique");
  }

  return { requests, batch, callback: callbackUrl.href, state };
};

export interface DelegationInterceptor {
  /** The request as it should be emitted to the handlers. */
  request: JsonRequest;
  /** Transforms the handler's response before it is delivered to the RP. */
  finalize: (response: JsonResponse) => Promise<JsonResponse>;
}

/** An interceptor that leaves the request and response untouched. */
const passthrough = (request: JsonRequest): DelegationInterceptor => ({
  request,
  finalize: (response) => Promise.resolve(response),
});

/** A delegation request with valid params — the only kind that gets the
 *  intermediate-key treatment. */
const isDelegationRequest = (request: JsonRequest): boolean =>
  request.method === "icrc34_delegation" &&
  request.id !== undefined &&
  DelegationParamsCodec.safeParse(request.params).success;

// The ephemeral key is generated NON-extractable: it must survive an II-internal
// redirect (e.g. an OpenID/SSO hop), but its private key must never be readable
// back out of storage. So the key pair itself is kept as a non-extractable
// `CryptoKeyPair` in IndexedDB (which stores it by structured clone, without
// exposing the private bits), and only its id is journaled in sessionStorage —
// mirroring how II persists its session keys (stores/session-delegation.store).
const createEphemeralIdentity = (): Promise<ECDSAKeyIdentity> =>
  ECDSAKeyIdentity.generate({ extractable: false });

/**
 * Builds the interceptor for one `icrc34_delegation` request against a given
 * ephemeral key: rewrites the request's `publicKey` to the ephemeral key (so
 * the canister only ever delegates to it) and returns a `finalize` that extends
 * the returned chain with a second hop to the RP's original session key, signed
 * with the ephemeral key. Pure given `(request, ephemeral)`, so the same builder
 * serves a fresh flow (a freshly generated key) and a rehydrated one (the key
 * reconstructed from storage). The request must have valid delegation params
 * (see {@link isDelegationRequest}).
 */
const buildDelegationInterceptor = (
  request: JsonRequest,
  ephemeral: ECDSAKeyIdentity,
): DelegationInterceptor => {
  const params = DelegationParamsCodec.parse(request.params);
  const rpSessionKey = new Uint8Array(params.publicKey.toDer());
  return {
    request: {
      ...request,
      params: DelegationParamsCodec.encode({
        ...params,
        publicKey: ephemeral.getPublicKey(),
      }),
    },
    finalize: async (response) => {
      // Errors carry no chain to extend.
      if (!("result" in response)) {
        return response;
      }
      const canisterChain = DelegationResultSchema.parse(response.result);
      const chain = await DelegationChain.create(
        ephemeral,
        { toDer: () => rpSessionKey } as unknown as PublicKey,
        new Date(Date.now() + OUTER_DELEGATION_EXPIRATION_MS),
        { previous: canisterChain },
      );
      return { ...response, result: DelegationResultSchema.encode(chain) };
    },
  };
};

/**
 * Intermediate-key middleware for `icrc34_delegation`, as a pure
 * request/response transform (no navigation, no storage). For a delegation
 * request it generates a fresh ephemeral {@link ECDSAKeyIdentity}, rewrites the
 * request's `publicKey` to that key (so II asks the canister for a delegation
 * to the ephemeral key — the only delegation that reaches certified subnet
 * state), and returns a `finalize` that extends the resulting chain with a
 * second hop to the RP's original session key, signed here with the ephemeral
 * key. Any other method — and any error response — passes through untouched.
 */
export const interceptDelegation = async (
  request: JsonRequest,
): Promise<DelegationInterceptor> =>
  isDelegationRequest(request)
    ? buildDelegationInterceptor(request, await createEphemeralIdentity())
    : passthrough(request);

export class UrlChannel implements Channel {
  #origin: string;
  // Stable per-flow token (generated once when the flow is first established,
  // journaled in the PersistedFlow, and handed back to the resumed channel), so
  // the transport can prove a resume belongs to this flow.
  #resumeToken: string;
  #callback: string;
  #state: string;
  #batch: boolean;
  #closed = false;
  #delivered = false;
  // Requests as emitted to handlers (delegation requests already rewritten to
  // the intermediate key), in the order the RP sent them.
  #requests: JsonRequest[];
  // Per-request response transforms, keyed by request id.
  #finalizers = new Map<string | number, DelegationInterceptor["finalize"]>();
  // Collected responses awaiting a complete batch, keyed by request id.
  #responses = new Map<string | number, JsonResponse>();
  #requestListeners = new Set<(request: JsonRequest) => void>();
  #closeListeners = new Set<() => void>();
  // Persists collected responses so a mid-flow redirect resumes with progress.
  #onResponsesChanged: (responses: Record<string, JsonResponse>) => void;
  // Invoked just before the response redirect, to drop any persisted flow.
  #onDelivered: () => void;
  // Invoked when the channel is torn down without ever delivering, to drop the
  // persisted flow that delivery would otherwise have cleared.
  #onClosed: () => void;

  constructor(params: {
    origin: string;
    callback: string;
    state: string;
    batch: boolean;
    interceptors: DelegationInterceptor[];
    // The flow's stable resume token; a fresh one is minted when omitted.
    resumeToken?: string;
    // Responses already collected on an earlier load (resume), keyed by id.
    responses?: Record<string, JsonResponse>;
    onResponsesChanged?: (responses: Record<string, JsonResponse>) => void;
    onDelivered?: () => void;
    onClosed?: () => void;
  }) {
    this.#origin = params.origin;
    this.#resumeToken = params.resumeToken ?? crypto.randomUUID();
    this.#callback = params.callback;
    this.#state = params.state;
    this.#batch = params.batch;
    this.#onResponsesChanged = params.onResponsesChanged ?? (() => {});
    this.#onDelivered = params.onDelivered ?? (() => {});
    this.#onClosed = params.onClosed ?? (() => {});
    this.#requests = params.interceptors.map(({ request }) => request);
    for (const { request, finalize } of params.interceptors) {
      if (request.id !== undefined) {
        this.#finalizers.set(request.id, finalize);
      }
    }
    // Restore responses from an earlier load; key by the response's own id so
    // the id type (string vs number) is preserved across JSON round-tripping.
    for (const response of Object.values(params.responses ?? {})) {
      this.#responses.set(response.id, response);
    }
  }

  get origin() {
    return this.#origin;
  }

  get resumeToken() {
    return this.#resumeToken;
  }

  get closed() {
    return this.#closed;
  }

  addEventListener(
    ...[event, listener]:
      | [event: "close", listener: () => void]
      | [event: "request", listener: (request: JsonRequest) => void]
  ): () => void {
    switch (event) {
      case "close":
        this.#closeListeners.add(listener);
        return () => {
          this.#closeListeners.delete(listener);
        };
      case "request": {
        // The request(s) arrived with the page load, so replay them to each
        // handler as it subscribes (mirrors PostMessageChannel). Skip any that
        // already have a response (restored from an earlier load), so a handler
        // that already answered before a redirect is not re-run.
        this.#requests
          .filter(
            (request) =>
              request.id === undefined || !this.#responses.has(request.id),
          )
          .forEach((request) => listener(request));
        this.#requestListeners.add(listener);
        return () => {
          this.#requestListeners.delete(listener);
        };
      }
    }
  }

  async send(response: JsonResponse): Promise<void> {
    if (this.#closed) {
      throw new Error("URL channel is closed");
    }
    if (this.#delivered) {
      // A redirect fires once; ignore stray responses after delivery.
      return;
    }

    const finalize = this.#finalizers.get(response.id);
    const finalized = finalize ? await finalize(response) : response;
    // Re-check after the `await`: a concurrent `send` (e.g. the error-abort
    // path) may have delivered or closed the channel while this one was
    // finalizing, and the guards above ran before that yield. Without this, an
    // aborted batch could still deliver a real delegation via a second assign.
    if (this.#delivered || this.#closed) {
      return;
    }
    this.#responses.set(response.id, finalized);

    // A redirect can only carry the whole batch once, so normally wait until
    // every request has a response. The exception is an error: a JSON-RPC error
    // aborts the batch, so redirect on the first one rather than wait for
    // responses that may never come, filling any still-unanswered request with
    // a generic error — e.g. [result, result, error, generic, generic].
    const expectedIds = this.#requests
      .map((request) => request.id)
      .filter((id): id is string | number => id !== undefined);
    const isError = "error" in finalized;
    const allAnswered = expectedIds.every((id) => this.#responses.has(id));
    if (!isError && !allAnswered) {
      // Persist progress so a mid-flow redirect resumes with this response
      // already collected, rather than re-running its handler.
      this.#onResponsesChanged(Object.fromEntries(this.#responses));
      return;
    }

    const responses = expectedIds.map(
      (id) => this.#responses.get(id) ?? abortedBatchError(id),
    );
    const message = this.#batch ? responses : responses[0];

    const fragment = new URLSearchParams();
    fragment.set(MESSAGE_PARAM, JSON.stringify(message));
    fragment.set(STATE_PARAM, this.#state);

    // Build the delivery URL with the URL constructor rather than string
    // concatenation, and re-assert the scheme at the navigation sink: the
    // callback was validated when read (and against the allow-list), so this
    // only ever navigates to a secure-context URL — https, or http on loopback.
    const deliveryUrl = new URL(this.#callback);
    if (!isSecureContextUrl(deliveryUrl)) {
      throw new Error("Refusing to deliver to a non-secure-context callback");
    }
    deliveryUrl.hash = fragment.toString();

    this.#delivered = true;
    this.#onDelivered();
    window.location.assign(deliveryUrl.href);
  }

  close(): Promise<void> {
    // A channel torn down without ever delivering leaves its persisted flow
    // behind (delivery is what clears it). Drop it here too. Guarded on
    // `#delivered` so a post-delivery close never wipes a subsequent flow, and
    // on `#closed` so a double close is a no-op.
    if (!this.#delivered && !this.#closed) {
      this.#onClosed();
    }
    this.#closed = true;
    this.#closeListeners.forEach((listener) => listener());
    return Promise.resolve();
  }
}

/** sessionStorage key for an in-progress URL-transport flow, so it survives an
 *  II-internal redirect (e.g. an OpenID/SSO hop) and can be resumed on the
 *  return load. Removed on delivery. */
const FLOW_STORAGE_KEY = "ii-icrc167-url-flow";
/** A stored flow older than this is treated as abandoned and ignored, so a
 *  single-use ephemeral key is never resurrected long after the fact. */
const FLOW_TIMEOUT_MS = 10 * 60 * 1000;

interface PersistedFlow {
  origin: string;
  callback: string;
  state: string;
  batch: boolean;
  timestamp: number;
  // Stable per-flow token minted at establish. The resume must present it (via
  // ChannelOptions.resumeToken) to prove ownership, so a lingering flow can't
  // be resumed by an unrelated later establish — even one for the same origin.
  resumeToken: string;
  // Original requests as received from the RP (`publicKey` still the RP session
  // key); the ephemeral rewrite is re-derived on resume.
  requests: JsonRequest[];
  // IndexedDB id of the ephemeral key per delegation request id, so the second
  // hop can still be signed after the redirect. Only the id lives here (in
  // sessionStorage); the key pair itself is a non-extractable CryptoKeyPair in
  // the IndexedDB store below.
  ephemeralKeyIds: Record<string, string>;
  // Responses already collected (finalized), keyed by request id, so a batch
  // that redirects again mid-flow resumes with its progress intact and does not
  // re-run already-answered handlers. Today's 1-click flows redirect only once,
  // before any response, so this is usually empty — it is defence for a batch
  // whose handlers redirect after answering.
  responses: Record<string, JsonResponse>;
}

// Key into `ephemeralKeyIds` (a plain object, so string keys only) by a
// type-tagged id: a JSON-RPC id is a string or a number, and
// `String(1) === String("1")`, so a bare `String(id)` would collide a numeric
// id with the string of the same digits. `JSON.stringify` distinguishes them
// (`1` vs `"1"`), and store and lookup must agree on it.
const ephemeralKeyMapKey = (id: string | number): string => JSON.stringify(id);

const storeFlow = (flow: PersistedFlow): void => {
  sessionStorage.setItem(FLOW_STORAGE_KEY, JSON.stringify(flow));
};

// Dedicated IndexedDB store for the transport's single-use ephemeral
// intermediate keys. The private key is kept here as a non-extractable
// CryptoKeyPair (never in the sessionStorage flow journal, which holds only the
// key id), so it cannot be read back out even by script with storage access.
const EPHEMERAL_KEY_STORE = createStore("ii-icrc167-ephemeral-keys", "keys");

interface StoredEphemeralKey {
  keyPair: CryptoKeyPair;
  // Epoch ms after which an abandoned-flow orphan is swept. Matches the flow
  // timeout, so a key never outlives the flow it belongs to.
  expiresAt: number;
}

const storeEphemeralKey = (
  keyId: string,
  keyPair: CryptoKeyPair,
): Promise<void> =>
  idbSet(
    keyId,
    { keyPair, expiresAt: Date.now() + FLOW_TIMEOUT_MS },
    EPHEMERAL_KEY_STORE,
  );

const loadEphemeralIdentity = async (
  keyId: string,
): Promise<ECDSAKeyIdentity | undefined> => {
  const stored = await idbGet<StoredEphemeralKey>(keyId, EPHEMERAL_KEY_STORE);
  return stored === undefined
    ? undefined
    : ECDSAKeyIdentity.fromKeyPair(stored.keyPair);
};

// Best-effort removal of a completed/abandoned flow's ephemeral keys. Fire and
// forget: on delivery the page navigates away, so the deletes may not land —
// `sweepExpiredEphemeralKeys` reclaims any that don't on the next fresh flow.
const deleteEphemeralKeys = (keyIds: string[]): void => {
  if (keyIds.length > 0) {
    void idbDelMany(keyIds, EPHEMERAL_KEY_STORE);
  }
};

// Delete a flow's ephemeral keys and its sessionStorage entry together. Reads
// the flow first to learn its key ids, then removes both.
const deleteFlow = (): void => {
  const flow = readStoredFlow();
  sessionStorage.removeItem(FLOW_STORAGE_KEY);
  if (flow !== undefined) {
    deleteEphemeralKeys(Object.values(flow.ephemeralKeyIds));
  }
};

// Sweep orphaned ephemeral keys — those whose flow navigated away before its
// own cleanup landed, or expired. Bounded by `expiresAt` so a concurrent tab's
// still-live keys are left untouched.
const sweepExpiredEphemeralKeys = async (): Promise<void> => {
  const now = Date.now();
  const stale = (
    await idbEntries<string, StoredEphemeralKey>(EPHEMERAL_KEY_STORE)
  )
    .filter(([, value]) => value.expiresAt <= now)
    .map(([keyId]) => keyId);
  deleteEphemeralKeys(stale);
};

/** Merges the latest collected responses into the stored flow, so a redirect
 *  mid-flow resumes with them. No-op if the flow is already gone (delivered or
 *  expired). */
const persistResponses = (responses: Record<string, JsonResponse>): void => {
  const current = readStoredFlow();
  if (current !== undefined) {
    storeFlow({ ...current, responses });
  }
};

// Validates a stored flow on read: a truncated write (storage quota, a
// `persistResponses` interleaving), a tampered entry, or a future format change
// must be rejected here rather than reach #rehydrate with a half-built shape —
// the resume path anchors the whole transport's security invariant.
const PersistedFlowSchema = z.object({
  origin: z.string(),
  callback: z.string(),
  state: z.string(),
  batch: z.boolean(),
  timestamp: z.number(),
  resumeToken: z.string(),
  requests: z.array(JsonRequestSchema),
  ephemeralKeyIds: z.record(z.string(), z.string()),
  responses: z.record(z.string(), JsonResponseSchema),
});

const readStoredFlow = (): PersistedFlow | undefined => {
  const json = sessionStorage.getItem(FLOW_STORAGE_KEY);
  if (json === null) {
    return undefined;
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(json);
  } catch {
    sessionStorage.removeItem(FLOW_STORAGE_KEY);
    return undefined;
  }
  const result = PersistedFlowSchema.safeParse(parsed);
  if (!result.success) {
    sessionStorage.removeItem(FLOW_STORAGE_KEY);
    return undefined;
  }
  const flow: PersistedFlow = result.data;
  if (Date.now() > flow.timestamp + FLOW_TIMEOUT_MS) {
    // The flow is abandoned; its ephemeral keys are reclaimed by
    // sweepExpiredEphemeralKeys (they carry the same timeout).
    sessionStorage.removeItem(FLOW_STORAGE_KEY);
    return undefined;
  }
  return flow;
};

export class UrlTransport implements Transport {
  async establishChannel(options?: ChannelOptions): Promise<UrlChannel> {
    const request = readUrlRequest();
    if (request !== undefined) {
      return await this.#establishFresh(request);
    }
    // No request in the hash: this may be the return load of an II-internal
    // redirect (e.g. an OpenID/SSO hop) that unloaded the page mid-flow, so
    // resume the persisted flow — but only when the caller PROVES it owns this
    // flow. `channelStore.establish` forwards, on the `openid-resume` return,
    // the redirecting channel's `resumeToken` (stashed before the redirect) and
    // its `allowedOrigin`; a normal load carries neither. Requiring the token to
    // match the journaled one — not merely inferring ownership from the origin —
    // means an unrelated later establish, even one for the same origin, can't
    // resume this flow: its channel carries a different token. The origin check
    // is kept as a cheap corroborating guard.
    const stored = readStoredFlow();
    if (
      stored !== undefined &&
      options?.allowedOrigin === stored.origin &&
      options?.resumeToken === stored.resumeToken
    ) {
      return await this.#rehydrate(stored);
    }
    // This establish is not resuming the stored flow, so any flow still sitting
    // in storage is abandoned (its own load never delivered and never came back
    // in resume mode). Drop it now rather than let it linger to its timeout.
    deleteFlow();
    throw new UrlTransportUnsupportedError(
      "No ICRC-167 request in the URL hash and no resumable flow for this load",
    );
  }

  async #establishFresh(request: UrlRequest): Promise<UrlChannel> {
    // Trust is anchored on the callback: deliver only to a URL the callback's
    // own origin declares, and scope the channel (and therefore the delegation)
    // to that origin. Fails the flow before any handler runs otherwise.
    const origin = new URL(request.callback).origin;
    await matchDeclaredCallback(origin, request.callback);

    // Reclaim any ephemeral keys orphaned by an earlier abandoned flow before
    // minting new ones, so the store does not grow without bound.
    await sweepExpiredEphemeralKeys();

    // Build the interceptors, generating a non-extractable ephemeral key per
    // delegation request. The key pair is kept in IndexedDB; only its id is
    // journaled, so the flow can resume after an II-internal redirect.
    const ephemeralKeyIds: Record<string, string> = {};
    const interceptors: DelegationInterceptor[] = [];
    for (const jsonRequest of request.requests) {
      if (!isDelegationRequest(jsonRequest) || jsonRequest.id === undefined) {
        interceptors.push(passthrough(jsonRequest));
        continue;
      }
      const ephemeral = await createEphemeralIdentity();
      const keyId = crypto.randomUUID();
      await storeEphemeralKey(keyId, ephemeral.getKeyPair());
      ephemeralKeyIds[ephemeralKeyMapKey(jsonRequest.id)] = keyId;
      interceptors.push(buildDelegationInterceptor(jsonRequest, ephemeral));
    }

    // Mint the flow's stable resume token once, journal it, and hand it to the
    // channel so channel.resumeToken === flow.resumeToken. The authorize page
    // stashes it before the IdP redirect and passes it back on resume.
    const resumeToken = crypto.randomUUID();
    storeFlow({
      origin,
      callback: request.callback,
      state: request.state,
      batch: request.batch,
      timestamp: Date.now(),
      resumeToken,
      requests: request.requests,
      ephemeralKeyIds,
      responses: {},
    });

    return new UrlChannel({
      origin,
      callback: request.callback,
      state: request.state,
      batch: request.batch,
      resumeToken,
      interceptors,
      onResponsesChanged: persistResponses,
      onDelivered: deleteFlow,
      onClosed: deleteFlow,
    });
  }

  async #rehydrate(flow: PersistedFlow): Promise<UrlChannel> {
    // The flow (and its callback) was validated when first stored — this is
    // II's own origin-scoped sessionStorage — so it is not re-fetched here.
    const interceptors = await Promise.all(
      flow.requests.map(async (jsonRequest) => {
        const keyId =
          jsonRequest.id !== undefined
            ? flow.ephemeralKeyIds[ephemeralKeyMapKey(jsonRequest.id)]
            : undefined;
        if (keyId === undefined) {
          // A non-delegation request legitimately has no journaled key. But a
          // delegation request MUST have one (every delegation request is keyed
          // in #establishFresh); reaching here means the stored flow is missing
          // it (corruption / tampering / format drift), so fail rather than let
          // the canister certify a delegation directly to the RP-supplied key —
          // the same fail-closed stance as the ephemeral-key-gone branch below.
          if (isDelegationRequest(jsonRequest)) {
            throw new Error(
              "ICRC-167 flow cannot resume: no ephemeral key journaled for a delegation request",
            );
          }
          return passthrough(jsonRequest);
        }
        const ephemeral = await loadEphemeralIdentity(keyId);
        if (ephemeral === undefined) {
          // The key was swept or cleared: the intermediate hop can no longer be
          // signed, so the flow cannot be completed safely. Fail rather than
          // fall back to a non-intercepted delegation.
          throw new Error(
            "ICRC-167 flow cannot resume: ephemeral key is no longer available",
          );
        }
        return buildDelegationInterceptor(jsonRequest, ephemeral);
      }),
    );

    return new UrlChannel({
      origin: flow.origin,
      callback: flow.callback,
      state: flow.state,
      batch: flow.batch,
      resumeToken: flow.resumeToken,
      interceptors,
      responses: flow.responses,
      onResponsesChanged: persistResponses,
      onDelivered: deleteFlow,
      onClosed: deleteFlow,
    });
  }
}
