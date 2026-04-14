/**
 * Channel store — owns the full lifecycle of the post-message channel between
 * Internet Identity and the relying party (dapp).
 *
 * Responsibilities:
 *  - Establishing the channel via PostMessage or Legacy transport.
 *  - Responding to ICRC-25 standard requests (supported standards, permissions).
 *  - Handling ICRC-34 delegation requests: waits for the user to authenticate
 *    and authorize, validates the derivation origin, creates the delegation
 *    chain, and sends the response.
 *  - Exposing error and idle state for the UI to react to.
 */
import { derived, get, type Readable, writable } from "svelte/store";
import type {
  Channel,
  ChannelOptions,
  JsonRequest,
  Transport,
} from "$lib/utils/transport/utils";
import {
  AttributesParamsSchema,
  DelegationParamsCodec,
  DelegationResultSchema,
  Icrc3AttributesParamsSchema,
  INVALID_PARAMS_ERROR_CODE,
} from "$lib/utils/transport/utils";
import { PostMessageTransport } from "$lib/utils/transport/postMessage";
import { PostMessageUnsupportedError } from "$lib/utils/transport/postMessage";
import { LegacyTransport } from "$lib/utils/transport/legacy";
import { frontendCanisterConfig, getPrimaryOrigin } from "$lib/globals";
import { authorizationStore } from "$lib/stores/authorization.store";
import { findConfig } from "$lib/utils/openID";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import { remapToLegacyDomain } from "$lib/utils/iiConnection";
import {
  type Authenticated,
  authenticationStore,
} from "$lib/stores/authentication.store";
import { DelegationChain } from "@icp-sdk/core/identity";
import {
  retryFor,
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import { z } from "zod";
import { goto } from "$app/navigation";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

type ChannelError =
  | "unable-to-connect"
  | "connection-closed"
  | "invalid-request"
  | "unverified-origin"
  | "delegation-failed";

type ChannelStore = Readable<Channel | undefined> & {
  establish: (options?: ChannelOptions) => void;
};

// ---------------------------------------------------------------------------
// ICRC-25 / ICRC-34 constants
// ---------------------------------------------------------------------------

const supportedStandards = [
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

const scopes = [{ method: "icrc34_delegation" }];

// ---------------------------------------------------------------------------
// Transports
// ---------------------------------------------------------------------------

const getTransports = (): Transport[] => {
  const primaryOrigin = getPrimaryOrigin();
  return [
    new PostMessageTransport(),
    new LegacyTransport(
      primaryOrigin !== undefined
        ? {
            redirectToOrigin: primaryOrigin,
            trustedOrigins: frontendCanisterConfig.related_origins[0] ?? [],
          }
        : undefined,
    ),
  ];
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Resolves the next time `authenticationStore` holds a value. */
const waitForAuthentication = (): Promise<Authenticated> =>
  new Promise((resolve) => {
    const unsubscribe = authenticationStore.subscribe((state) => {
      if (state !== undefined) {
        unsubscribe();
        resolve(state);
      }
    });
  });

/** Resolves when the user authorizes (picks an account). */
const waitForAuthorization = (): Promise<{
  accountNumber: bigint | undefined;
}> =>
  new Promise((resolve) => {
    const unsubscribe = authorizationStore.subscribe((state) => {
      if (state?.authorized === true) {
        unsubscribe();
        resolve({ accountNumber: state.accountNumber });
      }
    });
  });

// ---------------------------------------------------------------------------
// Request handlers (registered as channel event listeners)
// ---------------------------------------------------------------------------

/** ICRC-25: respond with the list of supported standards. */
const handleSupportedStandards =
  (channel: Channel) => (request: JsonRequest) => {
    if (
      request.id === undefined ||
      request.method !== "icrc25_supported_standards"
    ) {
      return;
    }
    void channel.send({
      jsonrpc: "2.0",
      id: request.id,
      result: { supportedStandards },
    });
  };

/** ICRC-25: respond with granted permission scopes. */
const handlePermissions = (channel: Channel) => (request: JsonRequest) => {
  if (
    request.id === undefined ||
    (request.method !== "icrc25_permissions" &&
      request.method !== "icrc25_request_permissions")
  ) {
    return;
  }
  void channel.send({
    jsonrpc: "2.0",
    id: request.id,
    result: {
      scopes: scopes.map((scope) => ({ scope, state: "granted" })),
    },
  });
};

/**
 * ICRC-34: handle a delegation request from the relying party.
 *
 * Drives the authorization flow by validating the request, exposing context
 * to the UI, waiting for the user to authorize, and sending the delegation back.
 */
const handleDelegationRequest =
  (channel: Channel) => async (request: JsonRequest) => {
    if (request.id === undefined || request.method !== "icrc34_delegation") {
      return;
    }

    channelIdleStore.set(false);

    const result = DelegationParamsCodec.safeParse(request.params);
    if (!result.success) {
      await channel.send({
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: z.prettifyError(result.error),
        },
      });
      channelErrorStore.set("invalid-request");
      return;
    }

    try {
      const params = result.data;

      const validationResult = await validateDerivationOrigin({
        requestOrigin: channel.origin,
        derivationOrigin: params.icrc95DerivationOrigin,
      });
      if (validationResult.result === "invalid") {
        await channel.send({
          jsonrpc: "2.0",
          id: request.id,
          error: {
            code: INVALID_PARAMS_ERROR_CODE,
            message: "Unverified derivation origin",
          },
        });
        channelErrorStore.set("unverified-origin");
        return;
      }

      // Compute effective origin (derivation origin if provided, else channel
      // origin) and remap *.icp0.io to *.ic0.app for legacy compatibility.
      // Setting the context triggers the authorization UI to render.
      const effectiveOrigin = remapToLegacyDomain(
        params.icrc95DerivationOrigin ?? channel.origin,
      );
      authorizationStore.setContext(effectiveOrigin);

      // Authorization is the commit point — the user may switch identities
      // freely before this. Once authorized, the UI is no longer needed.
      const { accountNumber } = await waitForAuthorization();
      channelIdleStore.set(true);

      // Read the identity *after* authorization so we capture whichever
      // identity the user settled on (they may have switched mid-flow).
      const { identityNumber, actor } = await waitForAuthentication();

      const sessionPublicKey = new Uint8Array(params.publicKey.toDer());

      const { user_key, expiration } = await actor
        .prepare_account_delegation(
          identityNumber,
          effectiveOrigin,
          accountNumber !== undefined ? [accountNumber] : [],
          sessionPublicKey,
          params.maxTimeToLive !== undefined ? [params.maxTimeToLive] : [],
        )
        .then(throwCanisterError);

      const delegationChain = await retryFor(5, () =>
        actor
          .get_account_delegation(
            identityNumber,
            effectiveOrigin,
            accountNumber !== undefined ? [accountNumber] : [],
            sessionPublicKey,
            expiration,
          )
          .then(throwCanisterError)
          .then(transformSignedDelegation)
          .then((delegation) =>
            DelegationChain.fromDelegations(
              [delegation],
              new Uint8Array(user_key),
            ),
          ),
      );

      await channel.send({
        jsonrpc: "2.0",
        id: request.id,
        result: DelegationResultSchema.encode(delegationChain),
      });
    } catch (error) {
      console.error(error);
      await channel.send({
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: error instanceof Error ? error.message : "Delegation failed",
        },
      });
      channelErrorStore.set("delegation-failed");
    }
  };

/**
 * Resolves the config issuer string from the current authentication state.
 * Returns `undefined` if the user did not authenticate via OpenID.
 */
const getConfigIssuer = (authenticated: Authenticated): string | undefined => {
  if (!("openid" in authenticated.authMethod)) {
    return undefined;
  }
  const { iss, metadata } = authenticated.authMethod.openid;
  return findConfig(iss, metadata)?.issuer;
};

/** Filters attribute keys to only those the user implicitly consents to. */
const filterImplicitConsentKeys = (
  keys: string[],
  configIssuer: string,
): string[] =>
  keys.filter(
    (key) =>
      key === `openid:${configIssuer}:name` ||
      key === `openid:${configIssuer}:email` ||
      key === `openid:${configIssuer}:verified_email`,
  );

/**
 * Handle legacy `ii_attributes` requests.
 *
 * Only responds if the user authenticated via OpenID and the dapp supports
 * certified attributes. Filters requested attributes to the implicit consent
 * set, prepares them via the canister, and sends the certified result.
 */
const handleLegacyAttributes =
  (channel: Channel) => async (request: JsonRequest) => {
    if (request.id === undefined || request.method !== "ii_attributes") {
      return;
    }

    const paramsResult = AttributesParamsSchema.safeParse(request.params);
    if (!paramsResult.success) {
      await channel.send({
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: z.prettifyError(paramsResult.error),
        },
      });
      return;
    }

    // Wait for the user to authorize before serving attributes.
    await waitForAuthorization();

    // Only OpenID users have attributes — bail if not OpenID.
    const authenticated = await waitForAuthentication();
    const configIssuer = getConfigIssuer(authenticated);
    if (configIssuer === undefined) {
      return;
    }

    // Validate the derivation origin if provided, same as delegation handler.
    const validationResult = await validateDerivationOrigin({
      requestOrigin: channel.origin,
      derivationOrigin: paramsResult.data.icrc95DerivationOrigin,
    });
    if (validationResult.result === "invalid") {
      await channel.send({
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: "Unverified derivation origin",
        },
      });
      return;
    }

    // Use the request's derivation origin if provided, else channel origin,
    // remapped to legacy domain for backwards compatibility.
    const origin = remapToLegacyDomain(
      paramsResult.data.icrc95DerivationOrigin ?? channel.origin,
    );

    // Only serve attributes the user implicitly consents to (name, email,
    // verified_email scoped to the config issuer).
    const implicitConsentKeys = filterImplicitConsentKeys(
      paramsResult.data.attributes,
      configIssuer,
    );

    try {
      // Prepare and certify attributes via the canister (two-step: prepare + get).
      const { attributes, issued_at_timestamp_ns } = await authenticated.actor
        .prepare_attributes({
          origin,
          attribute_keys: implicitConsentKeys,
          account_number: [],
          identity_number: authenticated.identityNumber,
        })
        .then(throwCanisterError);

      const { certified_attributes, expires_at_timestamp_ns } = await retryFor(
        5,
        () =>
          authenticated.actor
            .get_attributes({
              origin,
              account_number: [],
              identity_number: authenticated.identityNumber,
              attributes,
              issued_at_timestamp_ns,
            })
            .then(throwCanisterError),
      );

      await channel.send({
        jsonrpc: "2.0",
        id: request.id,
        result: {
          attributes: Object.fromEntries(
            certified_attributes.map((attribute) => [
              attribute.key,
              {
                value: z.util.uint8ArrayToBase64(
                  new Uint8Array(attribute.value),
                ),
                signature: z.util.uint8ArrayToBase64(
                  new Uint8Array(attribute.signature),
                ),
                expiration: expires_at_timestamp_ns.toString(),
              },
            ]),
          ),
        },
      });
    } catch (error) {
      console.error(error);
      await channel.send({
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: 1000,
          message:
            "Encountered an internal error while processing the request.",
        },
      });
    }
  };

/**
 * Handle `ii-icrc3-attributes` requests.
 *
 * Same as `handleLegacyAttributes` but uses the ICRC-3 attribute protocol
 * with nonce-based signatures instead of per-attribute signatures.
 */
const handleIcrc3Attributes =
  (channel: Channel) => async (request: JsonRequest) => {
    if (request.id === undefined || request.method !== "ii-icrc3-attributes") {
      return;
    }

    const paramsResult = Icrc3AttributesParamsSchema.safeParse(request.params);
    if (!paramsResult.success) {
      await channel.send({
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: z.prettifyError(paramsResult.error),
        },
      });
      return;
    }

    // Wait for the user to authorize before serving attributes.
    await waitForAuthorization();

    // Only OpenID users have attributes — bail if not OpenID.
    const authenticated = await waitForAuthentication();
    const configIssuer = getConfigIssuer(authenticated);
    if (configIssuer === undefined) {
      return;
    }

    // Validate the derivation origin if provided, same as delegation handler.
    const validationResult = await validateDerivationOrigin({
      requestOrigin: channel.origin,
      derivationOrigin: paramsResult.data.icrc95DerivationOrigin,
    });
    if (validationResult.result === "invalid") {
      await channel.send({
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: "Unverified derivation origin",
        },
      });
      return;
    }

    // Use the request's derivation origin if provided, else channel origin,
    // remapped to legacy domain for backwards compatibility.
    const origin = remapToLegacyDomain(
      paramsResult.data.icrc95DerivationOrigin ?? channel.origin,
    );

    // Only serve attributes the user implicitly consents to (name, email,
    // verified_email scoped to the config issuer).
    const implicitConsentKeys = filterImplicitConsentKeys(
      paramsResult.data.keys,
      configIssuer,
    );

    try {
      // Prepare and certify attributes via the canister (two-step: prepare + get).
      // ICRC-3 uses a nonce for a single combined signature rather than
      // per-attribute signatures.
      const { message } = await authenticated.actor
        .prepare_icrc3_attributes({
          origin,
          account_number: [],
          identity_number: authenticated.identityNumber,
          attributes: implicitConsentKeys.map((key) => ({
            key,
            value: [],
            omit_scope: false,
          })),
          nonce: z.util.base64ToUint8Array(paramsResult.data.nonce),
        })
        .then(throwCanisterError);

      const { signature } = await retryFor(5, () =>
        authenticated.actor
          .get_icrc3_attributes({
            origin,
            account_number: [],
            identity_number: authenticated.identityNumber,
            message,
          })
          .then(throwCanisterError),
      );

      await channel.send({
        jsonrpc: "2.0",
        id: request.id,
        result: {
          data: z.util.uint8ArrayToBase64(new Uint8Array(message)),
          signature: z.util.uint8ArrayToBase64(new Uint8Array(signature)),
        },
      });
    } catch (error) {
      console.error(error);
      await channel.send({
        jsonrpc: "2.0",
        id: request.id,
        error: {
          code: 1000,
          message:
            "Encountered an internal error while processing the request.",
        },
      });
    }
  };

// ---------------------------------------------------------------------------
// Stores
// ---------------------------------------------------------------------------

/** Set when the channel encounters an unrecoverable error. */
export const channelErrorStore = writable<ChannelError | undefined>();

/**
 * `true` when there is no active request that needs user interaction.
 * The UI renders the authorization screen when `false`, and a "redirecting"
 * screen when `true`. Resets to `false` when a new request comes in.
 */
export const channelIdleStore = writable(false);

const channelInternalStore = writable<Channel | undefined>();

export const channelStore: ChannelStore = {
  subscribe: channelInternalStore.subscribe,
  establish: (options) => {
    (async () => {
      const existingChannel = get(channelInternalStore);
      if (existingChannel?.closed === false) {
        return;
      }

      const channel = await Promise.any(
        getTransports().map((transport) => transport.establishChannel(options)),
      );
      channelInternalStore.set(channel);

      channel.addEventListener("request", handleSupportedStandards(channel));
      channel.addEventListener("request", handlePermissions(channel));
      channel.addEventListener("request", handleDelegationRequest(channel));
      channel.addEventListener("request", handleLegacyAttributes(channel));
      channel.addEventListener("request", handleIcrc3Attributes(channel));
      channel.addEventListener("close", () =>
        channelErrorStore.set("connection-closed"),
      );
    })().catch((error) => {
      console.error(error);
      // Promise.any wraps errors in AggregateError
      const errors = error instanceof AggregateError ? error.errors : [error];
      if (
        errors.some((e: unknown) => e instanceof PostMessageUnsupportedError)
      ) {
        void goto("/unsupported");
        return;
      }
      channelErrorStore.set("unable-to-connect");
    });
  },
};

/** Convenience derived store that throws when the channel is not yet established. */
export const establishedChannelStore: Readable<Channel> = derived(
  channelStore,
  (channel) => {
    if (channel === undefined) {
      throw new Error("Channel has not been established yet");
    }
    return channel;
  },
);
