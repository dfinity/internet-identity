import { z } from "zod";
import {
  type Channel,
  type ChannelOptions,
  type JsonRequest,
  type JsonResponse,
  type Transport,
  type AuthRequest,
  type AuthResponse,
  DelegationResultSchema,
  AuthRequestCodec,
  DelegationParamsCodec,
  AuthResponseCodec,
} from "$lib/utils/transport/utils";
import { AuthReady } from "$lib/legacy/flows/authorize/postMessageInterface";
import {
  Delegation,
  DelegationChain,
  ECDSAKeyIdentity,
} from "@icp-sdk/core/identity";
import { uint8Equals } from "@icp-sdk/core/candid";
import { Signature } from "@icp-sdk/core/agent";

const ESTABLISH_TIMEOUT_MS = 2000;
const AUTHORIZE_REQUEST_ID = "authorize-client";
const REDIRECT_SESSION_STORAGE_KEY = "ii-legacy-channel-redirect-session";

const RedirectMessageSchema = z.object({
  origin: z.httpUrl(),
  data: z.unknown(),
});

type RedirectMessage = z.infer<typeof RedirectMessageSchema>;

const redirectWithMessage = (
  targetOrigin: string,
  message: RedirectMessage,
) => {
  // Assign to hash to avoid sending message to server (keeps it client side)
  const redirectURL = new URL(targetOrigin);
  redirectURL.pathname = "/authorize";
  const searchParams = new URLSearchParams();
  searchParams.set(
    "redirect_message",
    JSON.stringify(message, (_, value) =>
      typeof value === "bigint" ? value.toString() : value,
    ),
  );
  searchParams.set("redirect_origin", window.location.origin);
  redirectURL.hash = searchParams.toString();

  window.location.replace(redirectURL.href);
};

const getRedirectMessage = (
  trustedOrigins: string[],
): { redirectOrigin: string; message: RedirectMessage } | undefined => {
  // Get message from hash
  const hash = window.location.hash.replace(/^#/, "");
  const params = new URLSearchParams(hash);
  const message = params.get("redirect_message");
  const redirectOrigin = params.get("redirect_origin");

  // Return if there's no message or source origin (not a redirect flow)
  if (message === null || redirectOrigin === null) {
    return;
  }

  // Clean up redirect message from URL for end-user (remove hash)
  const url = new URL(window.location.href);
  url.hash = "";
  window.history.replaceState(undefined, "", url);

  // Check if the redirect origin is a trusted origin (II itself)
  if (!trustedOrigins.includes(redirectOrigin)) {
    throw new Error("Redirect origin is untrusted");
  }
  return {
    redirectOrigin,
    message: RedirectMessageSchema.parse(JSON.parse(message)),
  };
};

const startRedirectSession = async (
  authRequest: AuthRequest,
): Promise<AuthRequest> => {
  const identity = await ECDSAKeyIdentity.generate({ extractable: true });
  const keyPair = identity.getKeyPair();
  const privateJwk = await window.crypto.subtle.exportKey(
    "jwk",
    keyPair.privateKey,
  );
  const publicJwk = await window.crypto.subtle.exportKey(
    "jwk",
    keyPair.publicKey,
  );
  sessionStorage.setItem(
    REDIRECT_SESSION_STORAGE_KEY,
    JSON.stringify({
      privateJwk,
      publicJwk,
      sessionPublicKey: z.util.uint8ArrayToBase64(authRequest.sessionPublicKey),
    }),
  );
  return {
    ...authRequest,
    sessionPublicKey: new Uint8Array(identity.getPublicKey().toDer()),
  };
};

const endRedirectSession = async (
  authResponse: AuthResponse,
): Promise<AuthResponse> => {
  if (authResponse.kind !== "authorize-client-success") {
    return authResponse;
  }
  const json = sessionStorage.getItem(REDIRECT_SESSION_STORAGE_KEY);
  if (json === null) {
    throw new Error("No ongoing redirect session found");
  }
  const {
    privateJwk,
    publicJwk,
    sessionPublicKey: sessionPublicKeyBase64,
  } = JSON.parse(json);
  const privateKey = await crypto.subtle.importKey(
    "jwk",
    privateJwk,
    { name: "ECDSA", namedCurve: "P-256" },
    true,
    ["sign"],
  );
  const publicKey = await crypto.subtle.importKey(
    "jwk",
    publicJwk,
    { name: "ECDSA", namedCurve: "P-256" },
    true,
    ["verify"],
  );
  const identity = await ECDSAKeyIdentity.fromKeyPair({
    privateKey,
    publicKey,
  });
  const responsedelegationChain = DelegationChain.fromDelegations(
    authResponse.delegations.map(({ delegation, signature }) => ({
      delegation: new Delegation(delegation.pubkey, delegation.expiration),
      signature: new Uint8Array(signature) as Signature,
    })),
    authResponse.userPublicKey,
  );
  const sessionPublicKey = z.util.base64ToUint8Array(sessionPublicKeyBase64);
  if (
    !uint8Equals(
      identity.getPublicKey().toDer(),
      responsedelegationChain.delegations[
        responsedelegationChain.delegations.length - 1
      ].delegation.pubkey,
    )
  ) {
    throw new Error(
      "Last public key in delegation chain does not match session identity",
    );
  }
  const sessionDelegationChain = await DelegationChain.create(
    identity,
    { toDer: () => sessionPublicKey },
    undefined,
    { previous: responsedelegationChain },
  );
  return {
    kind: "authorize-client-success",
    delegations: sessionDelegationChain.delegations.map(
      ({ delegation, signature }) => ({
        delegation,
        signature: new Uint8Array(signature),
      }),
    ),
    userPublicKey: new Uint8Array(sessionDelegationChain.publicKey),
    authnMethod: "passkey",
  };
};

class LegacyChannel implements Channel {
  #closed = false;
  #origin: string;
  #intermediateIdentityPromise: Promise<ECDSAKeyIdentity>;
  #redirectOrigin?: string;
  #authRequest?: AuthRequest;
  #closeListeners = new Set<() => void>();

  constructor(
    origin: string,
    authRequest: AuthRequest,
    redirectOrigin?: string,
  ) {
    this.#origin = origin;
    this.#authRequest = authRequest;
    this.#intermediateIdentityPromise = ECDSAKeyIdentity.generate();
    this.#redirectOrigin = redirectOrigin;
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
    if (event === "close") {
      this.#closeListeners.add(listener);
      return () => this.#closeListeners.delete(listener);
    }
    // Replay auth request if it didn't get a response yet
    const authRequest = this.#authRequest;
    if (event === "request" && authRequest !== undefined) {
      (async () => {
        // Sign towards intermediate identity instead in redirect flow
        const publicKey =
          this.#redirectOrigin !== undefined
            ? (await this.#intermediateIdentityPromise).getPublicKey()
            : { toDer: () => authRequest.sessionPublicKey };
        listener({
          id: AUTHORIZE_REQUEST_ID,
          jsonrpc: "2.0",
          method: "icrc34_delegation",
          params: DelegationParamsCodec.encode({
            publicKey,
            maxTimeToLive: authRequest.maxTimeToLive,
            icrc95DerivationOrigin: authRequest.derivationOrigin,
          }),
        });
      })();
    }

    return () => {};
  }

  async send(response: JsonResponse): Promise<void> {
    if (this.#closed) {
      throw new Error("Legacy channel is closed");
    }
    if (response.id !== AUTHORIZE_REQUEST_ID) {
      throw new Error("Legacy channel can only respond to authorize requests");
    }

    if (this.#authRequest === undefined) {
      throw new Error("No authorize request to respond to");
    }
    const requestPublicKey = this.#authRequest.sessionPublicKey;
    this.#authRequest = undefined;

    let data: AuthResponse;
    if ("result" in response) {
      // Extend delegation from intermediate identity to request public key in redirect flow
      const delegationChain = DelegationResultSchema.parse(response.result);
      const sessionDelegationChain =
        this.#redirectOrigin !== undefined
          ? await DelegationChain.create(
              await this.#intermediateIdentityPromise,
              { toDer: () => requestPublicKey },
              undefined,
              { previous: delegationChain },
            )
          : delegationChain;
      data = {
        kind: "authorize-client-success",
        delegations: sessionDelegationChain.delegations.map(
          ({ delegation, signature }) => ({
            delegation,
            signature: new Uint8Array(signature),
          }),
        ),
        userPublicKey: new Uint8Array(sessionDelegationChain.publicKey),
        authnMethod: "passkey",
      };
    } else {
      data = {
        kind: "authorize-client-failure",
        text: response.error.message,
      };
    }

    if (this.#redirectOrigin !== undefined) {
      redirectWithMessage(this.#redirectOrigin, {
        origin: this.#origin,
        data: AuthResponseCodec.encode(data),
      });
      return new Promise(() => {});
    }

    window.opener.postMessage(data, this.#origin);
    return Promise.resolve();
  }

  close(): Promise<void> {
    this.#closed = true;
    this.#closeListeners.forEach((l) => l());
    return Promise.resolve();
  }
}

interface RedirectOptions {
  redirectToOrigin: string;
  trustedOrigins: string[];
}

export class LegacyTransport implements Transport {
  #redirectOptions?: RedirectOptions;

  constructor(redirectOptions?: RedirectOptions) {
    this.#redirectOptions = redirectOptions;
  }

  establishChannel(options: ChannelOptions): Promise<LegacyChannel> {
    if (this.#redirectOptions !== undefined) {
      // Message received from prior redirect (either forwarded request or response)
      const redirectMessage = getRedirectMessage(
        this.#redirectOptions.trustedOrigins,
      );
      if (redirectMessage !== undefined) {
        return this.#processRedirectMessage({
          redirectToOrigin: this.#redirectOptions.redirectToOrigin,
          sourceOrigin: redirectMessage.redirectOrigin,
          targetOrigin: window.location.origin,
          message: redirectMessage.message,
        });
      }
    }

    return this.#establishViaPostMessage(options);
  }

  async #processRedirectMessage(params: {
    redirectToOrigin: string;
    sourceOrigin: string;
    targetOrigin: string;
    message: RedirectMessage;
  }): Promise<LegacyChannel> {
    if (this.#redirectOptions === undefined) {
      throw new Error("Redirect options are missing");
    }

    if (
      // Message sent from legacy origin to primary origin through redirect
      params.sourceOrigin !== params.redirectToOrigin &&
      params.targetOrigin === params.redirectToOrigin
    ) {
      // Assert message to be a request and establish channel with it
      const request = AuthRequestCodec.parse(params.message.data);
      return Promise.resolve(
        new LegacyChannel(params.message.origin, request, params.sourceOrigin),
      );
    } else if (
      // Message sent from primary origin to legacy origin through redirect
      params.sourceOrigin === params.redirectToOrigin &&
      params.targetOrigin !== params.redirectToOrigin
    ) {
      // Assert message to be a response and forward it to the app
      const response = AuthResponseCodec.parse(params.message.data);
      const sessionResponse = await endRedirectSession(response);
      window.opener.postMessage(sessionResponse, params.message.origin);
      // App should immediately close window after receiving the message,
      // so we return an indefinitely pending promise while we wait for it.
      return new Promise(() => {});
    }

    return Promise.reject("Redirect message could not be processed");
  }

  #establishViaPostMessage(options: ChannelOptions): Promise<LegacyChannel> {
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        window.removeEventListener("message", listener);
        reject(new Error("Legacy channel could not be established"));
      }, ESTABLISH_TIMEOUT_MS);

      const listener = async (event: MessageEvent) => {
        if (!this.#isValidAuthRequestEvent(event, options)) {
          return;
        }
        window.removeEventListener("message", listener);
        const parsed = AuthRequestCodec.safeParse(event.data);
        if (!parsed.success) {
          reject(new Error("Invalid legacy auth request"));
          return;
        }
        clearTimeout(timeout);

        // Redirect message to primary origin if we're on another origin
        if (
          this.#redirectOptions !== undefined &&
          this.#redirectOptions.redirectToOrigin !== window.location.origin
        ) {
          const sessionAuthRequest = await startRedirectSession(parsed.data);
          redirectWithMessage(this.#redirectOptions.redirectToOrigin, {
            origin: event.origin,
            data: AuthRequestCodec.encode(sessionAuthRequest),
          });
          return;
        }

        // Else establish channel with auth request
        resolve(new LegacyChannel(event.origin, parsed.data));
      };

      window.addEventListener("message", listener);
      window.opener?.postMessage(AuthReady, "*");
    });
  }

  #isValidAuthRequestEvent(event: MessageEvent, options: ChannelOptions) {
    const isSelf = event.origin === window.location.origin;
    const isOpener = event.source === window.opener;

    const isAuthRequest =
      typeof event.data === "object" &&
      event.data !== null &&
      "kind" in event.data &&
      event.data.kind === "authorize-client";

    const allowedOrigin =
      options?.allowedOrigin === undefined ||
      event.origin === options.allowedOrigin;

    return !isSelf && isOpener && isAuthRequest && allowedOrigin;
  }
}
