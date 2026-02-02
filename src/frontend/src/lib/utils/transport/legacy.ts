import {
  type Channel,
  type ChannelOptions,
  type JsonRequest,
  type JsonResponse,
  type Transport,
  DelegationResultSchema,
  AuthRequestToDelegationParamsCodec,
  DelegationParamsSchema,
} from "$lib/utils/transport/utils";
import {
  AuthReady,
  AuthRequest,
  AuthResponse,
} from "$lib/legacy/flows/authorize/postMessageInterface";
import { canisterConfig, getPrimaryOrigin } from "$lib/globals";

const ESTABLISH_TIMEOUT_MS = 2000;
const AUTHORIZE_REQUEST_ID = "authorize-client";

class LegacyChannel implements Channel {
  #closed = false;
  #origin: string;
  #redirect_uri?: string;
  #authRequest: AuthRequest | undefined;
  #closeListeners = new Set<() => void>();

  constructor(origin: string, authRequest: AuthRequest, redirect_uri?: string) {
    this.#origin = origin;
    this.#authRequest = authRequest;
    this.#redirect_uri = redirect_uri;
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
        // Replay authorize request if that didn't get a response yet
        if (this.#authRequest !== undefined) {
          const params = DelegationParamsSchema.encode(
            AuthRequestToDelegationParamsCodec.decode(
              AuthRequest.encode(this.#authRequest),
            ),
          );
          listener({
            id: AUTHORIZE_REQUEST_ID,
            jsonrpc: "2.0",
            method: "icrc34_delegation",
            params,
          });
        }
        return () => {};
      }
    }
  }

  send(response: JsonResponse): Promise<void> {
    if (this.#closed) {
      throw new Error("Legacy channel is closed");
    }
    if (response.id !== AUTHORIZE_REQUEST_ID) {
      throw new Error("Legacy channel can only respond to authorize requests");
    }
    // Remove authorize request after it has been given a response
    this.#authRequest = undefined;
    if (this.#redirect_uri !== undefined) {
      const redirectURL = new URL(this.#redirect_uri);
      if ("result" in response) {
        redirectURL.searchParams.set(
          "redirect_delegation_response",
          JSON.stringify({
            origin: this.#origin,
            result: response.result,
          }),
        );
      } else if ("error" in response) {
        redirectURL.searchParams.set(
          "redirect_delegation_response",
          JSON.stringify({
            origin: this.#origin,
            error: response.error,
          }),
        );
      }
      const a = document.createElement("a");
      a.href = redirectURL.href;
      a.referrerPolicy = "origin";
      a.click();
      return new Promise(() => {});
    } else {
      if ("result" in response) {
        const delegationChain = DelegationResultSchema.parse(response.result);
        window.opener.postMessage(
          {
            kind: "authorize-client-success",
            delegations: delegationChain.delegations,
            userPublicKey: delegationChain.publicKey,
            authnMethod: "passkey",
          } satisfies AuthResponse,
          this.#origin,
        );
      } else if ("error" in response) {
        window.opener.postMessage(
          {
            kind: "authorize-client-failure",
            text: response.error.message,
          } satisfies AuthResponse,
          this.#origin,
        );
      }
    }
    return Promise.resolve();
  }

  close(): Promise<void> {
    this.#closed = true;
    this.#closeListeners.forEach((listener) => listener());
    return Promise.resolve();
  }
}

export class LegacyTransport implements Transport {
  establishChannel(options: ChannelOptions): Promise<LegacyChannel> {
    const searchParams = new URLSearchParams(window.location.search);
    const redirectDelegationRequest = searchParams.get(
      "redirect_delegation_request",
    );
    const redirectDelegationResponse = searchParams.get(
      "redirect_delegation_response",
    );
    const cleanedURL = new URL(window.location.href);
    cleanedURL.searchParams.delete("redirect_delegation_request");
    cleanedURL.searchParams.delete("redirect_delegation_response");
    window.history.replaceState(undefined, "", cleanedURL);
    if (redirectDelegationRequest !== null && document.referrer.length > 0) {
      const referrer = new URL(document.referrer);
      if (
        !(canisterConfig.related_origins[0]?.includes(referrer.origin) ?? false)
      ) {
        throw new Error("Referrer origin is untrusted");
      }
      const { redirect_uri, origin, params } = JSON.parse(
        redirectDelegationRequest,
      );
      const authRequest = AuthRequest.parse(
        AuthRequestToDelegationParamsCodec.encode(
          DelegationParamsSchema.parse(params),
        ),
      );
      return Promise.resolve(
        new LegacyChannel(origin, authRequest, redirect_uri),
      );
    }
    if (redirectDelegationResponse !== null && document.referrer.length > 0) {
      const referrer = new URL(document.referrer);
      if (
        !(canisterConfig.related_origins[0]?.includes(referrer.origin) ?? false)
      ) {
        throw new Error("Referrer origin is untrusted");
      }
      const response = JSON.parse(redirectDelegationResponse);
      if ("result" in response) {
        const delegationChain = DelegationResultSchema.parse(response.result);
        window.opener.postMessage(
          {
            kind: "authorize-client-success",
            delegations: delegationChain.delegations,
            userPublicKey: delegationChain.publicKey,
            authnMethod: "passkey",
          } satisfies AuthResponse,
          response.origin,
        );
      } else if ("error" in response) {
        window.opener.postMessage(
          {
            kind: "authorize-client-failure",
            text: response.error.message,
          } satisfies AuthResponse,
          response.origin,
        );
      }
      return new Promise(() => {});
    }
    return new Promise((resolve, reject) => {
      const establishTimeout = setTimeout(() => {
        reject(new Error("Legacy channel could not be established"));
      }, ESTABLISH_TIMEOUT_MS);
      const listener = (event: MessageEvent) => {
        const isSelf = event.origin === window.location.origin;
        const isOpener = event.source === window.opener;
        const isAuthRequest =
          typeof event.data === "object" &&
          event.data !== null &&
          "kind" in event.data &&
          event.data.kind === "authorize-client";
        const isAllowedOrigin =
          options?.allowedOrigin === undefined ||
          event.origin === options?.allowedOrigin;
        const primaryOrigin = getPrimaryOrigin();

        if (isSelf || !isOpener || !isAuthRequest || !isAllowedOrigin) {
          return;
        }
        const result = AuthRequest.safeParse(event.data);
        if (!result.success) {
          reject(new Error("Invalid legacy auth request"));
          return;
        }
        if (
          primaryOrigin !== undefined &&
          window.location.origin !== primaryOrigin
        ) {
          const redirectURL = new URL(primaryOrigin);
          redirectURL.pathname = "/authorize";
          redirectURL.searchParams.set(
            "redirect_delegation_request",
            JSON.stringify({
              redirect_uri: window.location.origin + "/authorize",
              origin: event.origin,
              params: DelegationParamsSchema.encode(
                AuthRequestToDelegationParamsCodec.decode(
                  AuthRequest.encode(result.data),
                ),
              ),
            }),
          );
          const a = document.createElement("a");
          a.href = redirectURL.href;
          a.referrerPolicy = "origin";
          a.click();
          return;
        }
        const channel = new LegacyChannel(event.origin, result.data);
        clearTimeout(establishTimeout);
        resolve(channel);
      };
      window.addEventListener("message", listener);
      window.opener?.postMessage(AuthReady, "*");
    });
  }
}
