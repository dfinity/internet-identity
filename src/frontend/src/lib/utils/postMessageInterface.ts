import { AuthRequest } from "$lib/legacy/flows/authorize/postMessageInterface";
import { type SignedDelegation as FrontendSignedDelegation } from "@dfinity/identity";
import {
  isJsonRpcRequest,
  type SupportedStandard,
  type PermissionScope,
  type DelegationRequest,
} from "@slide-computer/signer";
import { HeartbeatServer } from "@slide-computer/signer-web";
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
            // Get derivation origin
            const derivationOrigin =
              "icrc95DerivationOrigin" in delegationRequest.params &&
              nonNullish(delegationRequest.params.icrc95DerivationOrigin) &&
              typeof delegationRequest.params.icrc95DerivationOrigin ===
                "string"
                ? delegationRequest.params.icrc95DerivationOrigin
                : undefined;
            const authRequest: AuthRequest = {
              kind: "authorize-client",
              sessionPublicKey: new Uint8Array(
                fromBase64(delegationRequest.params.publicKey),
              ),
              maxTimeToLive: nonNullish(delegationRequest.params.maxTimeToLive)
                ? BigInt(delegationRequest.params.maxTimeToLive)
                : undefined,
              derivationOrigin,
            };
            const requestOrigin = derivationOrigin ?? origin;

            onProgress("validating");

            let authenticateResult;
            // This should not fail, but there is a big drop-off in the funnel here.
            // It most probably means users closing the window, but we should investigate.
            try {
              authenticateResult = await authenticate({
                authRequest,
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
              (source as WindowProxy).postMessage(
                {
                  id: event.data.id,
                  jsonrpc: "2.0",
                  error: {
                    code: 1000,
                    message: "Generic error",
                    description: authenticateResult.text,
                  },
                },
                origin,
              );
              resolve(authenticateResult.kind);
              return;
            }
            void (authenticateResult.kind satisfies "success");
            authorizeClientFunnel.trigger(
              AuthorizeClientEvents.AuthenticateSuccess,
            );
            (source as WindowProxy).postMessage(
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
              origin,
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
