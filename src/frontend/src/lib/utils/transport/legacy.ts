import {
  type Channel,
  type ChannelOptions,
  type JsonRequest,
  type JsonResponse,
  type Transport,
  DelegationResultSchema,
} from "$lib/utils/transport/utils";
import {
  AuthReady,
  AuthRequest,
  AuthResponse,
} from "$lib/legacy/flows/authorize/postMessageInterface";
import { toBase64 } from "$lib/utils/utils";

const ESTABLISH_TIMEOUT_MS = 2000;
const AUTHORIZE_REQUEST_ID = "authorize-client";

class LegacyChannel implements Channel {
  #closed = false;
  #origin: string;
  #authRequest: AuthRequest | undefined;
  #closeListeners = new Set<() => void>();

  constructor(origin: string, authRequest: AuthRequest) {
    this.#origin = origin;
    this.#authRequest = authRequest;
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
          listener({
            id: AUTHORIZE_REQUEST_ID,
            jsonrpc: "2.0",
            method: "icrc34_delegation",
            params: {
              publicKey: toBase64(this.#authRequest.sessionPublicKey),
              maxTimeToLive: this.#authRequest.maxTimeToLive?.toString(),
              icrc95DerivationOrigin: this.#authRequest.derivationOrigin,
            },
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
    if ("result" in response) {
      const delegationChain = DelegationResultSchema.parse(response.result);
      console.log("delegationChain", delegationChain);
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

        if (isSelf || !isOpener || !isAuthRequest || !isAllowedOrigin) {
          return;
        }
        const result = AuthRequest.safeParse(event.data);
        if (!result.success) {
          reject(new Error("Invalid legacy auth request"));
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
