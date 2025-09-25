import { AuthRequest } from "$lib/legacy/flows/authorize/postMessageInterface";
import { type SignedDelegation as FrontendSignedDelegation } from "@dfinity/identity";
import {
  isJsonRpcRequest,
  type SupportedStandard,
  type PermissionScope,
  type JsonRequest,
  type DelegationRequest,
} from "@slide-computer/signer";
import {
  AuthorizeClientEvents,
  authorizeClientFunnel,
} from "./analytics/authorizeClientFunnel";
import { isNullish, nonNullish } from "@dfinity/utils";
import { fromBase64, toBase64 } from "$lib/utils/utils";
import {
  AuthenticationV2Events,
  authenticationV2Funnel,
} from "$lib/utils/analytics/authenticationV2Funnel";

interface HeartbeatServerOptions {
  /**
   * Callback when first heartbeat has been received
   */
  onEstablish: (origin: string, source: MessageEventSource) => void;
  /**
   * Reasonable time in milliseconds in which the communication channel needs to be established
   * @default 2000
   */
  establishTimeout?: number;
  /**
   * Callback when no heartbeats have been received for {@link establishTimeout} milliseconds
   */
  onEstablishTimeout: () => void;
  /**
   * Time in milliseconds of not receiving heartbeat requests after which the communication channel is disconnected
   * @default 2000
   */
  disconnectTimeout?: number;
  /**
   * Callback when no heartbeats have been received for {@link disconnectTimeout} milliseconds
   */
  onDisconnect: () => void;
  /**
   * Signer window, used to listen for incoming message events
   * @default globalThis.window
   */
  window?: Window;
}

// Copied from signer-js
class HeartbeatServer {
  readonly #options: Required<HeartbeatServerOptions>;

  constructor(options: HeartbeatServerOptions) {
    this.#options = {
      establishTimeout: 10000,
      disconnectTimeout: 2000,
      window: globalThis.window,
      ...options,
    };

    this.#establish();
  }

  #establish(): void {
    // Establish communication channel if a request is received
    const listener = this.#receiveStatusRequest((request) => {
      if (!request.source) {
        return;
      }
      listener();
      clearTimeout(timeout);

      this.#options.onEstablish(request.origin, request.source);
      this.#maintain(request.origin, request.source);
    });

    // Init timeout
    const timeout = setTimeout(() => {
      listener();

      this.#options.onEstablishTimeout();
    }, this.#options.establishTimeout);
  }

  #maintain(origin: string, source: MessageEventSource): void {
    let timeout: ReturnType<typeof setTimeout>;

    // Clear existing timeout (if any) and create a new one
    const resetTimeout = () => {
      clearTimeout(timeout);
      timeout = setTimeout(() => {
        listener();

        this.#options.onDisconnect();
      }, this.#options.disconnectTimeout);
    };

    // Init timeout and start sending messages
    resetTimeout();

    // Reset disconnect timeout and send response if a request is received
    const listener = this.#receiveStatusRequest((request) => {
      if (
        request.origin === origin &&
        request.source === source &&
        request.data.id !== undefined
      ) {
        resetTimeout();
        this.#sendReadyResponse(request.data.id, request.source);
      }
    });
  }

  #receiveStatusRequest(
    handler: (event: MessageEvent<JsonRequest<"icrc29_status">>) => void,
  ): () => void {
    const listener = (event: MessageEvent) => {
      if (
        isJsonRpcRequest(event.data) &&
        event.data.method === "icrc29_status"
      ) {
        handler(event);
      }
    };
    this.#options.window.addEventListener("message", listener);
    return () => this.#options.window.removeEventListener("message", listener);
  }

  #sendReadyResponse(id: string | number, source: MessageEventSource): void {
    source.postMessage(
      { jsonrpc: "2.0", id, result: "ready" },
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-expect-error
      "*",
    );
  }
}

const supportedStandards: SupportedStandard[] = [
  {
    name: "ICRC-25",
    url: "https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_25_signer_interaction_standard.md",
  },
  {
    name: "ICRC-29",
    url: "https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_29_window_post_message_transport.md",
  },
  {
    name: "ICRC-34",
    url: "https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_34_delegation.md",
  },
  {
    name: "ICRC-95",
    url: "https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_95_derivationorigin.md",
  },
];
const scopes: PermissionScope[] = [
  {
    method: "icrc34_delegation",
  },
];

/**
 * The postMessage-based authentication protocol.
 */
export function rpcAuthenticationProtocol({
  authenticate,
  onProgress,
}: {
  /** The callback used to get auth data (i.e. select or create anchor) */
  authenticate: (authContext: {
    authRequest: AuthRequest;
    requestOrigin: string;
  }) => Promise<
    | {
        kind: "success";
        delegations: FrontendSignedDelegation[];
        userPublicKey: Uint8Array;
        authnMethod: "pin" | "passkey" | "recovery";
      }
    | { kind: "failure"; text: string }
    | { kind: "unverified-origin"; text: string }
  >;
  /* Progress update messages to let the user know what's happening. */
  onProgress: (state: "waiting" | "validating") => void;
}): Promise<
  "orphan" | "closed" | "invalid" | "success" | "failure" | "unverified-origin"
> {
  return new Promise((resolve) => {
    authorizeClientFunnel.init();
    onProgress("waiting");
    new HeartbeatServer({
      onDisconnect(): void {
        resolve("closed");
      },
      onEstablish(origin: string, source: MessageEventSource): void {
        window.addEventListener("message", async (event) => {
          console.log("event", event);
          // Ignore invalid requests
          if (
            event.origin !== origin ||
            event.source !== source ||
            !isJsonRpcRequest(event.data)
          ) {
            return;
          }
          // ICRC-25
          if (event.data.method === "icrc25_supported_standards") {
            source.postMessage({
              id: event.data.id,
              jsonrpc: "2.0",
              result: {
                supportedStandards,
              },
            });
            return;
          }
          if (
            event.data.method === "icrc25_permissions" ||
            event.data.method === "icrc25_request_permissions"
          ) {
            source.postMessage({
              id: event.data.id,
              jsonrpc: "2.0",
              result: {
                scopes: scopes.map((scope) => ({
                  scope,
                  state: "granted",
                })),
              },
            });
            return;
          }
          // ICRC-34
          if (event.data.method === "icrc34_delegation") {
            const delegationRequest = event.data as DelegationRequest;
            // Ignore if params are missing
            if (isNullish(delegationRequest.params)) {
              return;
            }
            // Create context
            const derivationOrigin =
              "icrc95DerivationOrigin" in delegationRequest.params &&
              nonNullish(delegationRequest.params.icrc95DerivationOrigin) &&
              typeof delegationRequest.params.icrc95DerivationOrigin ===
                "string"
                ? delegationRequest.params.icrc95DerivationOrigin
                : undefined;
            const requestOrigin = derivationOrigin ?? origin;

            onProgress("validating");

            let authenticateResult;
            // This should not fail, but there is a big drop-off in the funnel here.
            // It most probably means users closing the window, but we should investigate.
            try {
              authenticateResult = await authenticate({
                authRequest: {
                  kind: "authorize-client",
                  sessionPublicKey: new Uint8Array(
                    fromBase64(delegationRequest.params.publicKey),
                  ),
                  maxTimeToLive: nonNullish(
                    delegationRequest.params.maxTimeToLive,
                  )
                    ? BigInt(delegationRequest.params.maxTimeToLive)
                    : undefined,
                  derivationOrigin,
                },
                requestOrigin,
              });
              authorizeClientFunnel.trigger(AuthorizeClientEvents.Authenticate);
              authorizeClientFunnel.close();
              authenticationV2Funnel.trigger(
                AuthenticationV2Events.AuthSuccess,
              );
              authenticationV2Funnel.close();
            } catch (error: unknown) {
              console.error("Unexpected error during authentication", error);
              authenticateResult = {
                kind: "failure" as const,
                text: "There was an unexpected error, please try again.",
              };
            }

            if (
              authenticateResult.kind === "failure" ||
              authenticateResult.kind === "unverified-origin"
            ) {
              authorizeClientFunnel.trigger(
                AuthorizeClientEvents.AuthenticateError,
                {
                  origin: requestOrigin,
                  failureReason: authenticateResult.text,
                },
              );
              source.postMessage(
                {
                  id: event.data.id,
                  jsonrpc: "2.0",
                  error: {
                    code: 1000,
                    message: "Generic error",
                    description: authenticateResult.text,
                  },
                },
                // eslint-disable-next-line @typescript-eslint/ban-ts-comment
                // @ts-expect-error
                "*",
              );
              resolve(authenticateResult.kind);
              return;
            }
            void (authenticateResult.kind satisfies "success");
            authorizeClientFunnel.trigger(
              AuthorizeClientEvents.AuthenticateSuccess,
            );
            source.postMessage(
              {
                id: event.data.id,
                jsonrpc: "2.0",
                result: {
                  publicKey: toBase64(authenticateResult.userPublicKey.buffer),
                  signerDelegation: authenticateResult.delegations.map(
                    (delegation) => ({
                      delegation: {
                        pubkey: toBase64(delegation.delegation.pubkey),
                        expiration:
                          delegation.delegation.expiration.toString(10),
                      },
                      signature: toBase64(delegation.signature),
                    }),
                  ),
                },
              },
              // eslint-disable-next-line @typescript-eslint/ban-ts-comment
              // @ts-expect-error
              "*",
            );
            resolve("success");
          }
        });
      },
      onEstablishTimeout(): void {
        resolve("closed");
      },
    });
  });
}
