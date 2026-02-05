import { z } from "zod";
import {
  type Channel,
  type ChannelOptions,
  type JsonRequest,
  type JsonResponse,
  type Transport,
  DelegationResultSchema,
  OriginSchema,
  AuthRequestCodec,
  AuthRequest,
  DelegationParamsCodec,
} from "$lib/utils/transport/utils";
import {
  AuthReady,
  type AuthResponse,
} from "$lib/legacy/flows/authorize/postMessageInterface";
import { canisterConfig, getPrimaryOrigin } from "$lib/globals";

const ESTABLISH_TIMEOUT_MS = 2000;
const AUTHORIZE_REQUEST_ID = "authorize-client";
const RedirectMessageSchema = z.object({
  origin: OriginSchema,
  data: z.unknown(),
});

type RedirectMessage = z.infer<typeof RedirectMessageSchema>;

const cleanRedirectHash = () => {
  if (window.location.hash === "") {
    return;
  }

  const url = new URL(window.location.href);
  url.hash = "";
  window.history.replaceState(undefined, "", url);
};

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
  redirectURL.hash = searchParams.toString();

  // Use an anchor so that we can override referrer policy
  const a = document.createElement("a");
  a.href = redirectURL.href;
  a.referrerPolicy = "origin";
  a.click();
};

const getRedirectMessage = ():
  | { sourceOrigin: string; message: RedirectMessage }
  | undefined => {
  // Get message from hash
  const hash = window.location.hash.replace(/^#/, "");
  const params = new URLSearchParams(hash);
  const message = params.get("redirect_message");

  // Return if there's no message
  if (message === null) {
    return;
  }

  // Check if the referrer is a trusted origin (II itself)
  const referrer = new URL(document.referrer);
  const trusted =
    canisterConfig.related_origins[0]?.includes(referrer.origin) ?? false;
  if (!trusted) {
    throw new Error("Referrer origin is untrusted");
  }
  return {
    sourceOrigin: referrer.origin,
    message: RedirectMessageSchema.parse(JSON.parse(message)),
  };
};

class LegacyChannel implements Channel {
  #closed = false;
  #origin: string;
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

    const authRequest = this.#authRequest;
    if (event === "request" && authRequest !== undefined) {
      // Replay auth request if it didn't get a response yet
      listener({
        id: AUTHORIZE_REQUEST_ID,
        jsonrpc: "2.0",
        method: "icrc34_delegation",
        params: DelegationParamsCodec.encode({
          publicKey: { toDer: () => authRequest.sessionPublicKey },
          maxTimeToLive: authRequest.maxTimeToLive,
          icrc95DerivationOrigin: authRequest.derivationOrigin,
        }),
      });
    }

    return () => {};
  }

  send(response: JsonResponse): Promise<void> {
    if (this.#closed) {
      throw new Error("Legacy channel is closed");
    }

    if (response.id !== AUTHORIZE_REQUEST_ID) {
      throw new Error("Legacy channel can only respond to authorize requests");
    }

    this.#authRequest = undefined;
    let data: AuthResponse;
    if ("result" in response) {
      const delegationChain = DelegationResultSchema.parse(response.result);
      data = {
        kind: "authorize-client-success",
        delegations: delegationChain.delegations,
        userPublicKey: delegationChain.publicKey,
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
        data,
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

export class LegacyTransport implements Transport {
  establishChannel(options: ChannelOptions): Promise<LegacyChannel> {
    const primaryOrigin = getPrimaryOrigin();
    const redirectMessage = getRedirectMessage();

    if (primaryOrigin === undefined || redirectMessage === undefined) {
      return this.#establishViaPostMessage(options);
    }

    cleanRedirectHash();
    if (window.location.origin === primaryOrigin) {
      // Establish channel with request from legacy origin
      return Promise.resolve(
        new LegacyChannel(
          redirectMessage.message.origin,
          AuthRequestCodec.parse(redirectMessage.message.data),
          redirectMessage.sourceOrigin,
        ),
      );
    } else {
      // Forward response from primary origin as-is
      window.opener.postMessage(
        redirectMessage.message.data,
        redirectMessage.message.origin,
      );
      return new Promise(() => {});
    }
  }

  #establishViaPostMessage(options: ChannelOptions): Promise<LegacyChannel> {
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        window.removeEventListener("message", listener);
        reject(new Error("Legacy channel could not be established"));
      }, ESTABLISH_TIMEOUT_MS);

      const listener = (event: MessageEvent) => {
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
        const primaryOrigin = getPrimaryOrigin();
        if (
          primaryOrigin !== undefined &&
          window.location.origin !== primaryOrigin
        ) {
          redirectWithMessage(primaryOrigin, {
            origin: event.origin,
            data: AuthRequestCodec.encode(parsed.data),
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
