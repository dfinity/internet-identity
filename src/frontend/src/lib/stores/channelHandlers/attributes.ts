import type { Channel, JsonRequest } from "$lib/utils/transport/utils";
import {
  AttributesParamsSchema,
  Icrc3AttributesParamsSchema,
  INVALID_PARAMS_ERROR_CODE,
} from "$lib/utils/transport/utils";
import { frontendCanisterConfig } from "$lib/globals";
import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import { remapToLegacyDomain } from "$lib/utils/iiConnection";
import {
  type Authenticated,
  authenticationStore,
} from "$lib/stores/authentication.store";
import {
  type Authorized,
  authorizationStore,
  authorizedStore,
} from "$lib/stores/authorization.store";
import { getMetadataString } from "$lib/utils/openID";
import { retryFor, throwCanisterError, waitForStore } from "$lib/utils/utils";
import { z } from "zod";
import type { ChannelError } from "$lib/stores/channelStore";
import {
  type AttributeGroup,
  type AvailableAttribute,
  attributeConsentResultStore,
  attributeConsentStore,
} from "$lib/stores/attributeConsent.store";

/** Extract the attribute name from a fully scoped key.
 *  e.g., "openid:https://accounts.google.com:email" → "email" */
export const extractAttributeName = (key: string): string => {
  const lastColon = key.lastIndexOf(":");
  return lastColon >= 0 ? key.slice(lastColon + 1) : key;
};

/** Extract the scope from a fully scoped key.
 *  e.g., "openid:https://accounts.google.com:email" → "openid:https://accounts.google.com" */
export const extractScope = (key: string): string | undefined => {
  const lastColon = key.lastIndexOf(":");
  return lastColon >= 0 ? key.slice(0, lastColon) : undefined;
};

/** Serialize ICRC-3 consent requests so the user only ever sees one
 *  consent screen at a time — if a (possibly malicious) dapp sends several
 *  in parallel, each `setContext` would otherwise overwrite the previous
 *  one and the user could click through on one request while a different
 *  one silently receives their approval. The legacy and 1-click OpenID
 *  handlers don't touch `attributeConsentStore`, so only the consent
 *  handler needs this lock. */
let consentQueueTail: Promise<unknown> = Promise.resolve();
const serializeConsentRequest = <T>(fn: () => Promise<T>): Promise<T> => {
  const prev = consentQueueTail;
  const next = prev.then(fn);
  // Don't let a rejection from an earlier call block the queue.
  consentQueueTail = next.catch(() => {});
  return next;
};

/** Whether a scoped key is in the 1-click OpenID auto-approve allowlist
 *  for the given issuer — these keys skip the consent screen because the
 *  user already proved possession of that issuer's account during the
 *  1-click flow. */
const isOneClickOpenIdKey = (key: string, configIssuer: string): boolean =>
  key === `openid:${configIssuer}:name` ||
  key === `openid:${configIssuer}:email` ||
  key === `openid:${configIssuer}:verified_email`;

/** SSO equivalent of {@link isOneClickOpenIdKey}.
 *  `verified_email` is intentionally excluded — the canister doesn't yet
 *  support `sso:<domain>:verified_email` (see PR #3805 silent-drop
 *  behaviour), so auto-approving it would be a no-op that hides a
 *  consent the user should still see for any other requested key. */
const isOneClickSsoKey = (key: string, domain: string): boolean =>
  key === `sso:${domain}:name` || key === `sso:${domain}:email`;

/** Resolve a single requested key against available attributes.
 *  A scoped request (e.g. `openid:google:email`) matches only the exact
 *  wire row. An unscoped request (e.g. `email`) fans out across every
 *  source on the anchor: scoped rows from OIDC/SSO whose attribute-name
 *  suffix matches, plus unscoped wire rows from verified emails (where
 *  multiple rows can share the same `email` key with different
 *  addresses). */
const resolveKey = (
  requestedKey: string,
  available: Array<[string, Uint8Array | number[]]>,
  decoder: TextDecoder,
): AvailableAttribute[] => {
  const decodeRow = (
    [key, value]: [string, Uint8Array | number[]],
    omitScope: boolean,
  ): AvailableAttribute => {
    const rawValue = new Uint8Array(value);
    return {
      key,
      displayValue: decoder.decode(rawValue),
      rawValue,
      omitScope,
    };
  };

  // Scoped request (e.g. "openid:google:email"): exact match only.
  const isScopedRequest = requestedKey.includes(":");
  if (isScopedRequest) {
    const exact = available.find(([key]) => key === requestedKey);
    return exact === undefined ? [] : [decodeRow(exact, false)];
  }

  // Unscoped request (e.g. "email"): fan out across every source.
  const unscopedRows = available.filter(([key]) => key === requestedKey);
  const scopedRows = available.filter(([key]) =>
    key.endsWith(`:${requestedKey}`),
  );
  return [...unscopedRows, ...scopedRows].map((row) => decodeRow(row, true));
};

/**
 * Resolve requested attribute keys against the canister's available attributes.
 * Groups results by attribute name for UI rendering.
 * Attributes with no available values are omitted.
 */
export const resolveAttributeGroups = (
  requestedKeys: string[],
  available: Array<[string, Uint8Array | number[]]>,
): AttributeGroup[] => {
  const decoder = new TextDecoder();
  const groups: AttributeGroup[] = [];

  for (const requestedKey of requestedKeys) {
    const options = resolveKey(requestedKey, available, decoder);
    if (options.length > 0) {
      groups.push({ name: extractAttributeName(requestedKey), options });
    }
  }

  return groups;
};

/**
 * Handle `ii_attributes` requests (the pre-ICRC-3 attribute sharing method
 * that produces per-attribute signatures).
 *
 * Only responds if the user authenticated via OpenID and the dapp supports
 * certified attributes. Filters requested attributes to the implicit consent
 * set, prepares them via the canister, and sends the certified result.
 */
export const handleLegacyAttributes =
  (channel: Channel, onError: (error: ChannelError) => void) =>
  async (request: JsonRequest) => {
    if (request.id === undefined || request.method !== "ii_attributes") {
      return;
    }
    // Capture the narrowed id so the serialized closure doesn't have to
    // re-narrow across the `await` boundary.
    const requestId = request.id;

    const paramsResult = AttributesParamsSchema.safeParse(request.params);
    if (!paramsResult.success) {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: z.prettifyError(paramsResult.error),
        },
      });
      return;
    }

    // Only serve attributes to known dapps that have opted into certified
    // attributes, or when running in a dev environment (fetch_root_key enabled).
    const dapp = getDapps().find((d) => d.hasOrigin(channel.origin));
    if (
      dapp?.certifiedAttributes !== true &&
      frontendCanisterConfig.fetch_root_key[0] !== true
    ) {
      return;
    }

    // Attributes are sourced from OpenID credential metadata — non-OpenID
    // flows have nothing to serve, so bail.
    const flow = await waitForStore(authorizationStore, (ctx) => ctx?.flow);
    if (flow.type !== "1-click-openid") {
      return;
    }
    const configIssuer = flow.issuer;

    // Wait for the user to authorize before serving attributes.
    await waitForStore(authorizedStore);
    const authenticated = await waitForStore(authenticationStore);

    // Confirm `icrc95DerivationOrigin` is an alternate origin the channel's
    // origin is permitted to use — prevents dapp A from certifying
    // attributes under dapp B's origin by claiming B as its derivation.
    const validationResult = await validateDerivationOrigin({
      requestOrigin: channel.origin,
      derivationOrigin: paramsResult.data.icrc95DerivationOrigin,
    });
    if (validationResult.result === "invalid") {
      onError("unverified-origin");
      return;
    }

    // Use the request's derivation origin if provided, else channel origin,
    // remapped to legacy domain for backwards compatibility.
    const origin = remapToLegacyDomain(
      paramsResult.data.icrc95DerivationOrigin ?? channel.origin,
    );

    // Legacy attributes only certify the 1-click OpenID allowlist.
    const oneClickKeys = paramsResult.data.attributes.filter((key) =>
      isOneClickOpenIdKey(key, configIssuer),
    );

    try {
      // Two-phase certification: `prepare_attributes` produces the message
      // to certify, `get_attributes` returns the signature once the message
      // has been certified (retried because certification lags by a few
      // heartbeats).
      const { attributes, issued_at_timestamp_ns } = await authenticated.actor
        .prepare_attributes({
          origin,
          attribute_keys: oneClickKeys,
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
        id: requestId,
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
      onError("attributes-failed");
    }
  };

/**
 * Everything the consent handler needs to drive the consent UI and, once
 * the user agrees, certify + send. Built by `resolveConsentPipeline`.
 *
 * `origin` is the legacy-remapped value used for principal derivation.
 * `unmappedOrigin` is the dapp's actual origin (icp0.io, custom domain,
 * etc.) and is what the canister will certify as `implicit:origin`.
 */
type ConsentPipeline = {
  accountNumberPromise: Promise<bigint | undefined>;
  authenticated: Authenticated;
  authorized: Authorized;
  origin: string;
  unmappedOrigin: string;
  groups: AttributeGroup[];
  recoveryAddresses: string[];
  verifiedAddresses: string[];
  openidAddresses: string[];
};

/**
 * Resolve everything the consent handler needs before it can either show
 * the consent UI or certify an empty set: wait for auth, validate the
 * derivation origin, ask the canister for available attributes, and shape
 * the groups for the UI. Errors are swallowed and reported via `onError`
 * — returning `null` tells the caller to give up without throwing (Svelte's
 * `{#await}` doesn't render rejected promises).
 */
const resolveConsentPipeline = async (params: {
  channel: Channel;
  onError: (error: ChannelError) => void;
  derivationOrigin: string | undefined;
  requestedKeys: string[];
}): Promise<ConsentPipeline | null> => {
  const { channel, onError, derivationOrigin, requestedKeys } = params;
  try {
    const authorized = await waitForStore(authorizedStore);
    const { accountNumberPromise } = authorized;
    const authenticated = await waitForStore(authenticationStore);

    const validationResult = await validateDerivationOrigin({
      requestOrigin: channel.origin,
      derivationOrigin,
    });
    if (validationResult.result === "invalid") {
      onError("unverified-origin");
      return null;
    }

    const unmappedOrigin = derivationOrigin ?? channel.origin;
    const origin = remapToLegacyDomain(unmappedOrigin);

    // TODO: pass `[requestedKeys]` once the canister silently drops unknown
    // keys. Today it errors on anything it doesn't recognise, which would
    // reject mixed `["email", "favorite_color"]` requests outright — so for
    // now we ask for everything available on the anchor and filter below.
    const availablePromise: Promise<Array<[string, Uint8Array | number[]]>> =
      requestedKeys.length > 0
        ? authenticated.actor
            .list_available_attributes({
              identity_number: authenticated.identityNumber,
              attributes: [],
            })
            .then(throwCanisterError)
        : Promise.resolve([]);
    const identityInfoPromise = authenticated.actor
      .identity_info(authenticated.identityNumber)
      .then(throwCanisterError);
    const [available, identityInfo] = await Promise.all([
      availablePromise,
      identityInfoPromise,
    ]);
    const recoveryAddresses = (identityInfo.email_recovery[0] ?? []).map(
      (c: { address: string }) => c.address,
    );
    const verifiedAddresses = (identityInfo.verified_emails[0] ?? []).map(
      (e: { address: string }) => e.address,
    );
    const openidAddresses = (identityInfo.openid_credentials[0] ?? [])
      .map((c) => getMetadataString(c.metadata, "email"))
      .filter((e): e is string => e !== undefined);

    return {
      accountNumberPromise,
      authenticated,
      authorized,
      origin,
      unmappedOrigin,
      groups: resolveAttributeGroups(requestedKeys, available),
      recoveryAddresses,
      verifiedAddresses,
      openidAddresses,
    };
  } catch (error) {
    console.error(error);
    onError("attributes-failed");
    return null;
  }
};

/**
 * Drive the canister's two-phase ICRC-3 certification and hand the result
 * back to the relying party: `prepare_icrc3_attributes` returns a message
 * to certify, `get_icrc3_attributes` returns the signature once the
 * message has been certified (retried because certification lags the
 * prepare call by a few heartbeats). Shared by both ICRC-3 handlers.
 */
const certifyAndSend = async (params: {
  channel: Channel;
  onError: (error: ChannelError) => void;
  requestId: string | number;
  nonce: string;
  authenticated: { identityNumber: bigint; actor: Authenticated["actor"] };
  accountNumber: bigint | undefined;
  origin: string;
  unmappedOrigin: string;
  attributeSpecs: Array<{
    key: string;
    value: [] | [Uint8Array];
    omit_scope: boolean;
  }>;
}): Promise<void> => {
  const {
    channel,
    onError,
    requestId,
    nonce,
    authenticated,
    accountNumber,
    origin,
    unmappedOrigin,
    attributeSpecs,
  } = params;
  try {
    const { message } = await authenticated.actor
      .prepare_icrc3_attributes({
        origin,
        unmapped_origin: [unmappedOrigin],
        account_number: accountNumber !== undefined ? [accountNumber] : [],
        identity_number: authenticated.identityNumber,
        attributes: attributeSpecs,
        nonce: z.util.base64ToUint8Array(nonce),
      })
      .then(throwCanisterError);

    const { signature } = await retryFor(5, () =>
      authenticated.actor
        .get_icrc3_attributes({
          origin,
          account_number: accountNumber !== undefined ? [accountNumber] : [],
          identity_number: authenticated.identityNumber,
          message,
        })
        .then(throwCanisterError),
    );

    await channel.send({
      jsonrpc: "2.0",
      id: requestId,
      result: {
        data: z.util.uint8ArrayToBase64(new Uint8Array(message)),
        signature: z.util.uint8ArrayToBase64(new Uint8Array(signature)),
      },
    });
  } catch (error) {
    console.error(error);
    onError("attributes-failed");
  }
};

/**
 * Handle `ii-icrc3-attributes` requests where the user has signed in via the
 * 1-click OpenID flow and every requested key is in that issuer's
 * auto-approve allowlist (no consent UI needed).
 *
 * Waits for the authorization flow — set eagerly by the authorize page.
 * Returns early for non-OpenID flows; the consent handler takes those.
 */
export const handleIcrc3OneClickOpenIdAttributes =
  (channel: Channel, onError: (error: ChannelError) => void) =>
  async (request: JsonRequest) => {
    if (request.id === undefined || request.method !== "ii-icrc3-attributes") {
      return;
    }
    const requestId = request.id;

    const paramsResult = Icrc3AttributesParamsSchema.safeParse(request.params);
    if (!paramsResult.success) {
      // The consent handler is always registered alongside this one and
      // reports the parse failure to the RP; staying silent here avoids a
      // duplicate JSON-RPC error response on the channel.
      return;
    }

    const requestedKeys = paramsResult.data.keys;
    if (requestedKeys.length === 0) {
      return;
    }

    const flow = await waitForStore(authorizationStore, (ctx) => ctx?.flow);
    if (flow.type !== "1-click-openid") {
      return;
    }
    const configIssuer = flow.issuer;
    if (!requestedKeys.every((key) => isOneClickOpenIdKey(key, configIssuer))) {
      return;
    }

    try {
      const { accountNumberPromise } = await waitForStore(authorizedStore);
      const authenticated = await waitForStore(authenticationStore);

      const validationResult = await validateDerivationOrigin({
        requestOrigin: channel.origin,
        derivationOrigin: paramsResult.data.icrc95DerivationOrigin,
      });
      if (validationResult.result === "invalid") {
        onError("unverified-origin");
        return;
      }

      const unmappedOrigin =
        paramsResult.data.icrc95DerivationOrigin ?? channel.origin;
      const origin = remapToLegacyDomain(unmappedOrigin);

      // Filter to keys the canister actually has — the user may not have
      // granted every claim in the allowlist (e.g. missing verified_email).
      // TODO: pass `[requestedKeys]` once the canister silently drops unknown
      // keys; today it errors on unknown names, so fetch everything and
      // filter via `availableKeys` below.
      const available = await authenticated.actor
        .list_available_attributes({
          identity_number: authenticated.identityNumber,
          attributes: [],
        })
        .then(throwCanisterError);
      const availableKeys = new Set(available.map(([key]) => key));
      const attributeSpecs = requestedKeys
        .filter((key) => availableKeys.has(key))
        .map((key) => ({
          key,
          value: [] as [],
          omit_scope: false,
        }));

      const accountNumber = await accountNumberPromise;
      await certifyAndSend({
        channel,
        onError,
        requestId,
        nonce: paramsResult.data.nonce,
        authenticated,
        accountNumber,
        origin,
        unmappedOrigin,
        attributeSpecs,
      });
    } catch (error) {
      console.error(error);
      onError("attributes-failed");
    }
  };

/**
 * SSO equivalent of {@link handleIcrc3OneClickOpenIdAttributes}. When the app
 * requested SSO sign-in via the `?sso=<domain>` 1-click entry and every
 * requested key is in that domain's auto-approve allowlist, certify and send
 * without showing the consent UI.
 */
export const handleIcrc3OneClickSsoAttributes =
  (channel: Channel, onError: (error: ChannelError) => void) =>
  async (request: JsonRequest) => {
    if (request.id === undefined || request.method !== "ii-icrc3-attributes") {
      return;
    }
    const requestId = request.id;

    const paramsResult = Icrc3AttributesParamsSchema.safeParse(request.params);
    if (!paramsResult.success) {
      // The consent handler reports parse failures; staying silent here
      // avoids a duplicate JSON-RPC error.
      return;
    }

    const requestedKeys = paramsResult.data.keys;
    if (requestedKeys.length === 0) {
      return;
    }

    try {
      const flow = await waitForStore(authorizationStore, (ctx) => ctx?.flow);
      // Auto-approve only the `?sso=<domain>` 1-click entry, not the manual wizard or a passkey/OpenID session.
      if (flow.type !== "1-click-sso") {
        return;
      }
      if (!requestedKeys.every((key) => isOneClickSsoKey(key, flow.domain))) {
        return;
      }
      const authenticated = await waitForStore(authenticationStore);
      const { accountNumberPromise } = await waitForStore(authorizedStore);

      const validationResult = await validateDerivationOrigin({
        requestOrigin: channel.origin,
        derivationOrigin: paramsResult.data.icrc95DerivationOrigin,
      });
      if (validationResult.result === "invalid") {
        onError("unverified-origin");
        return;
      }

      const unmappedOrigin =
        paramsResult.data.icrc95DerivationOrigin ?? channel.origin;
      const origin = remapToLegacyDomain(unmappedOrigin);

      // Filter to keys the canister actually has — the user may have
      // signed in with a subset of the allowlist (e.g. no email claim).
      // `list_available_attributes` covers both `openid:<issuer>` and
      // `sso:<domain>` scopes (`verified_email` is omitted under
      // `sso:` because the canister can't certify it there).
      const available = await authenticated.actor
        .list_available_attributes({
          identity_number: authenticated.identityNumber,
          attributes: [],
        })
        .then(throwCanisterError);
      const availableKeys = new Set(available.map(([key]) => key));
      const attributeSpecs = requestedKeys
        .filter((key) => availableKeys.has(key))
        .map((key) => ({
          key,
          value: [] as [],
          omit_scope: false,
        }));

      const accountNumber = await accountNumberPromise;
      await certifyAndSend({
        channel,
        onError,
        requestId,
        nonce: paramsResult.data.nonce,
        authenticated,
        accountNumber,
        origin,
        unmappedOrigin,
        attributeSpecs,
      });
    } catch (error) {
      console.error(error);
      onError("attributes-failed");
    }
  };

/**
 * Handle `ii-icrc3-attributes` requests that require explicit consent (or
 * have nothing to certify). Pairs with `handleIcrc3OneClickOpenIdAttributes`
 * and `handleIcrc3OneClickSsoAttributes`: returns early when either of those
 * handlers will take the request.
 */
export const handleIcrc3ConsentAttributes =
  (channel: Channel, onError: (error: ChannelError) => void) =>
  async (request: JsonRequest) => {
    if (request.id === undefined || request.method !== "ii-icrc3-attributes") {
      return;
    }
    const requestId = request.id;

    const paramsResult = Icrc3AttributesParamsSchema.safeParse(request.params);
    if (!paramsResult.success) {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: z.prettifyError(paramsResult.error),
        },
      });
      return;
    }

    const requestedKeys = paramsResult.data.keys;

    await serializeConsentRequest(async () => {
      try {
        // Bail out as soon as we know one of the 1-click handlers will take this request.
        const flow = await waitForStore(authorizationStore, (ctx) => ctx?.flow);
        const oneClickHandlerWillHandle =
          requestedKeys.length > 0 &&
          ((flow.type === "1-click-openid" &&
            requestedKeys.every((key) =>
              isOneClickOpenIdKey(key, flow.issuer),
            )) ||
            (flow.type === "1-click-sso" &&
              requestedKeys.every((key) =>
                isOneClickSsoKey(key, flow.domain),
              )));
        if (oneClickHandlerWillHandle) {
          return;
        }

        // Only unscoped email/verified_email; scoped keys are pinned to
        // a source that the inline verify wizard can't satisfy.
        const emailRequested = requestedKeys.some((key) => {
          if (extractScope(key) !== undefined) return false;
          const name = extractAttributeName(key);
          return name === "email" || name === "verified_email";
        });

        for (;;) {
          // Kick off the pipeline but don't await it yet — we want to set the
          // consent context synchronously from the (still-pending) promise so
          // the consent view can paint its loading skeleton while auth
          // resolves and `list_available_attributes` runs in the background.
          const pipelinePromise = resolveConsentPipeline({
            channel,
            onError,
            derivationOrigin: paramsResult.data.icrc95DerivationOrigin,
            requestedKeys,
          });

          attributeConsentStore.setContext(
            pipelinePromise.then((pipeline) => ({
              groups: pipeline?.groups ?? [],
              effectiveOrigin: pipeline?.origin ?? "",
              requestedKeys,
              recoveryAddresses: pipeline?.recoveryAddresses ?? [],
              verifiedAddresses: pipeline?.verifiedAddresses ?? [],
              openidAddresses: pipeline?.openidAddresses ?? [],
            })),
          );

          const pipeline = await pipelinePromise;
          if (pipeline === null) {
            // Error (or invalid origin) already reported — resolve the consent
            // result so the UI transitions away from the loading state instead
            // of hanging.
            attributeConsentStore.setConsent({ attributes: [] });
            return;
          }

          let attributeSpecs: Array<{
            key: string;
            value: [] | [Uint8Array];
            omit_scope: boolean;
          }>;

          if (pipeline.groups.length === 0 && !emailRequested) {
            // Nothing to share and no inline "Verify an email" affordance —
            // certify an empty set so the UI skips the empty picker view.
            attributeSpecs = [];
            attributeConsentStore.setConsent({ attributes: [] });
          } else {
            const outcome = await Promise.race([
              waitForStore(attributeConsentResultStore).then((consent) => ({
                type: "consent" as const,
                consent,
              })),
              waitForStore(authorizedStore, (authorized) =>
                authorized !== pipeline.authorized
                  ? { type: "restart" as const }
                  : undefined,
              ),
            ]);
            if (outcome.type === "restart") {
              continue;
            }
            attributeSpecs = outcome.consent.attributes.map((attr) => ({
              key: attr.key,
              value: [new Uint8Array(attr.rawValue)] as [Uint8Array],
              omit_scope: attr.omitScope,
            }));
          }

          const accountNumber = await pipeline.accountNumberPromise;
          await certifyAndSend({
            channel,
            onError,
            requestId,
            nonce: paramsResult.data.nonce,
            authenticated: pipeline.authenticated,
            accountNumber,
            origin: pipeline.origin,
            unmappedOrigin: pipeline.unmappedOrigin,
            attributeSpecs,
          });
          return;
        }
      } finally {
        // Always reset consent state so the next request on this channel
        // starts from a clean slate (no leftover context/result from us).
        attributeConsentStore.clear();
      }
    });
  };
