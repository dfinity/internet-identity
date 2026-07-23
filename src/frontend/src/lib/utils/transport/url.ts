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
import {
  type Channel,
  type ChannelOptions,
  type JsonRequest,
  JsonRequestSchema,
  type JsonResponse,
  type Transport,
  DelegationParamsCodec,
  DelegationResultSchema,
  GENERIC_ERROR_CODE,
} from "$lib/utils/transport/utils";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import type { PublicKey } from "@icp-sdk/core/agent";
import { matchDeclaredCallback } from "$lib/utils/authCallbacks";

/** Hash-fragment parameter carrying the JSON-RPC request (in) / response (out). */
const MESSAGE_PARAM = "message";
/** Hash-fragment parameter carrying the RP callback URL (request only). */
const CALLBACK_PARAM = "callback";
/** Hash-fragment parameter the RP uses to correlate the response with its request. */
const STATE_PARAM = "state";

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

  // Strip the request from the URL so it does not linger in history or leak via
  // the address bar while the flow runs (mirrors the legacy/MCP redirect flows).
  const stripped = new URL(window.location.href);
  stripped.hash = "";
  window.history.replaceState(null, "", stripped);

  if (callback === null || state === null) {
    throw new Error("ICRC-167 request is missing a callback or state");
  }

  let parsed: unknown;
  try {
    parsed = JSON.parse(message);
  } catch {
    throw new Error("ICRC-167 request message is not valid JSON");
  }

  const batch = Array.isArray(parsed);
  const values: unknown[] = batch ? (parsed as unknown[]) : [parsed];
  const requests = values.map((value) => {
    const result = JsonRequestSchema.safeParse(value);
    if (!result.success) {
      throw new Error(
        "ICRC-167 request message is not a valid JSON-RPC request",
      );
    }
    return result.data;
  });

  return { requests, batch, callback, state };
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

// The ephemeral key is generated extractable so it can be persisted (as JWK)
// across an II-internal redirect and reconstructed on the return load.
type EphemeralJwk = { privateJwk: JsonWebKey; publicJwk: JsonWebKey };

const createEphemeralIdentity = (): Promise<ECDSAKeyIdentity> =>
  ECDSAKeyIdentity.generate({ extractable: true });

const exportEphemeral = async (
  identity: ECDSAKeyIdentity,
): Promise<EphemeralJwk> => {
  const keyPair = identity.getKeyPair();
  return {
    privateJwk: await crypto.subtle.exportKey("jwk", keyPair.privateKey),
    publicJwk: await crypto.subtle.exportKey("jwk", keyPair.publicKey),
  };
};

const importEphemeral = async ({
  privateJwk,
  publicJwk,
}: EphemeralJwk): Promise<ECDSAKeyIdentity> => {
  const algorithm = { name: "ECDSA", namedCurve: "P-256" } as const;
  const [privateKey, publicKey] = await Promise.all([
    crypto.subtle.importKey("jwk", privateJwk, algorithm, true, ["sign"]),
    crypto.subtle.importKey("jwk", publicJwk, algorithm, true, ["verify"]),
  ]);
  return ECDSAKeyIdentity.fromKeyPair({ privateKey, publicKey });
};

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

  constructor(params: {
    origin: string;
    callback: string;
    state: string;
    batch: boolean;
    interceptors: DelegationInterceptor[];
    // Responses already collected on an earlier load (resume), keyed by id.
    responses?: Record<string, JsonResponse>;
    onResponsesChanged?: (responses: Record<string, JsonResponse>) => void;
    onDelivered?: () => void;
  }) {
    this.#origin = params.origin;
    this.#callback = params.callback;
    this.#state = params.state;
    this.#batch = params.batch;
    this.#onResponsesChanged = params.onResponsesChanged ?? (() => {});
    this.#onDelivered = params.onDelivered ?? (() => {});
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

    this.#delivered = true;
    this.#onDelivered();
    window.location.assign(`${this.#callback}#${fragment.toString()}`);
  }

  close(): Promise<void> {
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
  // Original requests as received from the RP (`publicKey` still the RP session
  // key); the ephemeral rewrite is re-derived on resume.
  requests: JsonRequest[];
  // Ephemeral key material per delegation request id, so the second hop can
  // still be signed after the redirect.
  ephemeralKeys: Record<string, EphemeralJwk>;
  // Responses already collected (finalized), keyed by request id, so a batch
  // that redirects again mid-flow resumes with its progress intact and does not
  // re-run already-answered handlers. Today's 1-click flows redirect only once,
  // before any response, so this is usually empty — it is defence for a batch
  // whose handlers redirect after answering.
  responses: Record<string, JsonResponse>;
}

const storeFlow = (flow: PersistedFlow): void => {
  sessionStorage.setItem(FLOW_STORAGE_KEY, JSON.stringify(flow));
};

const deleteStoredFlow = (): void => {
  sessionStorage.removeItem(FLOW_STORAGE_KEY);
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

const readStoredFlow = (): PersistedFlow | undefined => {
  const json = sessionStorage.getItem(FLOW_STORAGE_KEY);
  if (json === null) {
    return undefined;
  }
  let flow: PersistedFlow;
  try {
    flow = JSON.parse(json) as PersistedFlow;
  } catch {
    deleteStoredFlow();
    return undefined;
  }
  if (Date.now() > flow.timestamp + FLOW_TIMEOUT_MS) {
    deleteStoredFlow();
    return undefined;
  }
  return flow;
};

export class UrlTransport implements Transport {
  async establishChannel(_options?: ChannelOptions): Promise<UrlChannel> {
    const request = readUrlRequest();
    if (request !== undefined) {
      return await this.#establishFresh(request);
    }
    // No request in the hash: this may be the return load of an II-internal
    // redirect (e.g. an OpenID/SSO hop) that unloaded the page mid-flow, so
    // resume a persisted flow if one is still in progress. Any other load has
    // nothing to resume.
    const stored = readStoredFlow();
    if (stored !== undefined) {
      return await this.#rehydrate(stored);
    }
    throw new UrlTransportUnsupportedError(
      "No ICRC-167 request in the URL hash and no flow to resume",
    );
  }

  async #establishFresh(request: UrlRequest): Promise<UrlChannel> {
    // Trust is anchored on the callback: deliver only to a URL the callback's
    // own origin declares, and scope the channel (and therefore the delegation)
    // to that origin. Fails the flow before any handler runs otherwise.
    const origin = new URL(request.callback).origin;
    await matchDeclaredCallback(origin, request.callback);

    // Build the interceptors, generating an ephemeral key per delegation
    // request, and remember the key material so the flow can resume after an
    // II-internal redirect.
    const ephemeralKeys: Record<string, EphemeralJwk> = {};
    const interceptors: DelegationInterceptor[] = [];
    for (const jsonRequest of request.requests) {
      if (!isDelegationRequest(jsonRequest)) {
        interceptors.push(passthrough(jsonRequest));
        continue;
      }
      const ephemeral = await createEphemeralIdentity();
      ephemeralKeys[String(jsonRequest.id)] = await exportEphemeral(ephemeral);
      interceptors.push(buildDelegationInterceptor(jsonRequest, ephemeral));
    }

    storeFlow({
      origin,
      callback: request.callback,
      state: request.state,
      batch: request.batch,
      timestamp: Date.now(),
      requests: request.requests,
      ephemeralKeys,
      responses: {},
    });

    return new UrlChannel({
      origin,
      callback: request.callback,
      state: request.state,
      batch: request.batch,
      interceptors,
      onResponsesChanged: persistResponses,
      onDelivered: deleteStoredFlow,
    });
  }

  async #rehydrate(flow: PersistedFlow): Promise<UrlChannel> {
    // The flow (and its callback) was validated when first stored — this is
    // II's own origin-scoped sessionStorage — so it is not re-fetched here.
    const interceptors = await Promise.all(
      flow.requests.map(async (jsonRequest) => {
        const keys =
          jsonRequest.id !== undefined
            ? flow.ephemeralKeys[String(jsonRequest.id)]
            : undefined;
        return keys !== undefined
          ? buildDelegationInterceptor(jsonRequest, await importEphemeral(keys))
          : passthrough(jsonRequest);
      }),
    );

    return new UrlChannel({
      origin: flow.origin,
      callback: flow.callback,
      state: flow.state,
      batch: flow.batch,
      interceptors,
      responses: flow.responses,
      onResponsesChanged: persistResponses,
      onDelivered: deleteStoredFlow,
    });
  }
}
