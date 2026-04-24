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
  authorizationStore,
  authorizedStore,
} from "$lib/stores/authorization.store";
import { retryFor, throwCanisterError, waitForStore } from "$lib/utils/utils";
import { z } from "zod";
import type { ChannelError } from "$lib/stores/channelStore";
import {
  type AttributeGroup,
  type AvailableAttribute,
  attributeConsentResultStore,
  attributeConsentStore,
  extractAttributeName,
} from "$lib/stores/attributeConsent.store";

/** Check if a scoped key is an implicit consent key for the given issuer. */
const isImplicitConsentKey = (key: string, configIssuer: string): boolean =>
  key === `openid:${configIssuer}:name` ||
  key === `openid:${configIssuer}:email` ||
  key === `openid:${configIssuer}:verified_email`;

/** Filters attribute keys to only those the user implicitly consents to. */
const filterImplicitConsentKeys = (
  keys: string[],
  configIssuer: string,
): string[] => keys.filter((key) => isImplicitConsentKey(key, configIssuer));

/** Resolve a single requested key against available attributes. */
const resolveKey = (
  requestedKey: string,
  available: Array<[string, Uint8Array | number[]]>,
  decoder: TextDecoder,
): AvailableAttribute[] => {
  const availableKeys = available.map(([key]) => key);
  const isScoped = availableKeys.includes(requestedKey);

  if (isScoped) {
    const entry = available.find(([key]) => key === requestedKey);
    if (entry === undefined) {
      return [];
    }
    const rawValue = new Uint8Array(entry[1]);
    return [
      {
        key: requestedKey,
        displayValue: decoder.decode(rawValue),
        rawValue,
        omitScope: false,
      },
    ];
  }

  // Unscoped key — find all available keys ending with :requestedKey
  return available
    .filter(([key]) => key.endsWith(`:${requestedKey}`))
    .map(([key, value]) => {
      const rawValue = new Uint8Array(value);
      return {
        key,
        displayValue: decoder.decode(rawValue),
        rawValue,
        omitScope: true,
      };
    });
};

/**
 * Resolve requested attribute keys against the canister's available attributes.
 * Groups results by attribute name for UI rendering.
 * Attributes with no available values are omitted.
 */
const resolveAttributeGroups = (
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

    // Only serve attributes to known dapps that have opted into certified
    // attributes, or when running in a dev environment (fetch_root_key enabled).
    const dapp = getDapps().find((d) => d.hasOrigin(channel.origin));
    if (
      dapp?.certifiedAttributes !== true &&
      frontendCanisterConfig.fetch_root_key[0] !== true
    ) {
      return;
    }

    // Only OpenID flows have attributes — bail if not OpenID.
    const flow = await waitForStore(authorizationStore, (ctx) => ctx?.flow);
    if (flow.type !== "1-click-openid") {
      return;
    }
    const configIssuer = flow.issuer;

    // Wait for the user to authorize before serving attributes.
    await waitForStore(authorizedStore);
    const authenticated = await waitForStore(authenticationStore);

    // Validate the derivation origin if provided, same as delegation handler.
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

    // Legacy attributes use implicit consent only.
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
      onError("attributes-failed");
    }
  };

/**
 * Certify and send the selected attribute specs to the relying party.
 * Shared between the implicit and consent handlers.
 */
const certifyAndSend = async (
  channel: Channel,
  onError: (error: ChannelError) => void,
  request: JsonRequest & { id: string | number },
  params: { nonce: string },
  authenticated: { identityNumber: bigint; actor: Authenticated["actor"] },
  accountNumber: bigint | undefined,
  origin: string,
  attributeSpecs: Array<{
    key: string;
    value: [] | [Uint8Array];
    omit_scope: boolean;
  }>,
): Promise<void> => {
  try {
    const { message } = await authenticated.actor
      .prepare_icrc3_attributes({
        origin,
        account_number: accountNumber !== undefined ? [accountNumber] : [],
        identity_number: authenticated.identityNumber,
        attributes: attributeSpecs,
        nonce: z.util.base64ToUint8Array(params.nonce),
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
      id: request.id,
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
 * Handle `ii-icrc3-attributes` requests where the user has signed in via an
 * OpenID provider and every requested key is an implicit consent claim for
 * that issuer (no consent UI needed).
 *
 * Waits for the authorization flow — set eagerly by the authorize page.
 * Returns early for non-OpenID flows; the consent handler takes those.
 */
export const handleIcrc3ImplicitAttributes =
  (channel: Channel, onError: (error: ChannelError) => void) =>
  async (request: JsonRequest) => {
    if (request.id === undefined || request.method !== "ii-icrc3-attributes") {
      return;
    }

    const paramsResult = Icrc3AttributesParamsSchema.safeParse(request.params);
    if (!paramsResult.success) {
      // The consent handler reports the error response — stay silent here.
      return;
    }

    const dapp = getDapps().find((d) => d.hasOrigin(channel.origin));
    if (
      dapp?.certifiedAttributes !== true &&
      frontendCanisterConfig.fetch_root_key[0] !== true
    ) {
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
    if (
      !requestedKeys.every((key) => isImplicitConsentKey(key, configIssuer))
    ) {
      return;
    }

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

    const origin = remapToLegacyDomain(
      paramsResult.data.icrc95DerivationOrigin ?? channel.origin,
    );

    // Filter to keys the canister actually has — the user may not have
    // granted every implicit claim (e.g. missing verified_email).
    const available = await authenticated.actor
      .list_available_attributes({
        identity_number: authenticated.identityNumber,
        attributes: [requestedKeys],
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
    await certifyAndSend(
      channel,
      onError,
      { ...request, id: request.id },
      paramsResult.data,
      authenticated,
      accountNumber,
      origin,
      attributeSpecs,
    );
  };

/**
 * Handle `ii-icrc3-attributes` requests that require explicit consent (or
 * have nothing to certify). Pairs with `handleIcrc3ImplicitAttributes`:
 * returns early when that handler will take the request.
 */
export const handleIcrc3ConsentAttributes =
  (channel: Channel, onError: (error: ChannelError) => void) =>
  async (request: JsonRequest) => {
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

    const dapp = getDapps().find((d) => d.hasOrigin(channel.origin));
    if (
      dapp?.certifiedAttributes !== true &&
      frontendCanisterConfig.fetch_root_key[0] !== true
    ) {
      return;
    }

    const requestedKeys = paramsResult.data.keys;

    // Wait for the flow — set eagerly by the authorize page — so we can
    // bail out as soon as we know the implicit handler will take this.
    const flow = await waitForStore(authorizationStore, (ctx) => ctx?.flow);
    const implicitHandlerWillHandle =
      flow.type === "1-click-openid" &&
      requestedKeys.length > 0 &&
      requestedKeys.every((key) => isImplicitConsentKey(key, flow.issuer));
    if (implicitHandlerWillHandle) {
      return;
    }

    // Kick off all remaining async work inside a single promise — we can
    // set the consent context immediately so the loading screen appears
    // while auth resolves and the canister call runs. Errors are caught
    // inside the promise so the consent view always sees a resolved value
    // (never a rejection that Svelte's `{#await}` can't render).
    const pipelinePromise = (async () => {
      try {
        const { accountNumberPromise } = await waitForStore(authorizedStore);
        const authenticated = await waitForStore(authenticationStore);

        const validationResult = await validateDerivationOrigin({
          requestOrigin: channel.origin,
          derivationOrigin: paramsResult.data.icrc95DerivationOrigin,
        });
        if (validationResult.result === "invalid") {
          onError("unverified-origin");
          return null;
        }

        const origin = remapToLegacyDomain(
          paramsResult.data.icrc95DerivationOrigin ?? channel.origin,
        );

        const available =
          requestedKeys.length > 0
            ? await authenticated.actor
                .list_available_attributes({
                  identity_number: authenticated.identityNumber,
                  attributes: [requestedKeys],
                })
                .then(throwCanisterError)
            : [];

        return {
          accountNumberPromise,
          authenticated,
          origin,
          groups: resolveAttributeGroups(requestedKeys, available),
        };
      } catch (error) {
        console.error(error);
        onError("attributes-failed");
        return null;
      }
    })();

    attributeConsentStore.setContext(
      pipelinePromise.then((pipeline) => ({
        groups: pipeline?.groups ?? [],
        effectiveOrigin: pipeline?.origin ?? "",
      })),
    );

    const pipeline = await pipelinePromise;
    if (pipeline === null) {
      // Error (or invalid origin) already reported — resolve the consent
      // result so the UI transitions away from the loading state instead of
      // hanging.
      attributeConsentStore.setConsent({ attributes: [] });
      return;
    }

    let attributeSpecs: Array<{
      key: string;
      value: [] | [Uint8Array];
      omit_scope: boolean;
    }>;

    if (pipeline.groups.length === 0) {
      // Nothing the dapp requested is available — certify an empty set and
      // auto-resolve consent so the UI skips the empty picker view.
      attributeSpecs = [];
      attributeConsentStore.setConsent({ attributes: [] });
    } else {
      const consent = await waitForStore(attributeConsentResultStore);
      attributeSpecs = consent.attributes.map((attr) => ({
        key: attr.key,
        value: [new Uint8Array(attr.rawValue)] as [Uint8Array],
        omit_scope: attr.omitScope,
      }));
    }

    const accountNumber = await pipeline.accountNumberPromise;
    await certifyAndSend(
      channel,
      onError,
      { ...request, id: request.id },
      paramsResult.data,
      pipeline.authenticated,
      accountNumber,
      origin,
      attributeSpecs,
    );
  };
