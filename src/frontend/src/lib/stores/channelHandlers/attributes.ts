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
  authenticationStore,
  pendingOpenIdIssuerStore,
} from "$lib/stores/authentication.store";
import { authorizedStore } from "$lib/stores/authorization.store";
import { retryFor, throwCanisterError, waitForStore } from "$lib/utils/utils";
import { z } from "zod";
import type { ChannelError } from "$lib/stores/channelStore";
import { get } from "svelte/store";
import {
  type AttributeConsentContext,
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

/** Check if a key structurally looks like it could be an implicit consent key. */
const couldBeImplicitKey = (key: string): boolean => key.startsWith("openid:");

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
    const configIssuer = get(pendingOpenIdIssuerStore);
    if (configIssuer === undefined) {
      return;
    }

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
 * Handle `ii-icrc3-attributes` requests.
 *
 * Resolves requested attribute keys against available attributes, shows a
 * consent UI when needed, and certifies the consented attributes.
 */
export const handleIcrc3Attributes =
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

    // Only serve attributes to known dapps that have opted into certified
    // attributes, or when running in a dev environment (fetch_root_key enabled).
    const dapp = getDapps().find((d) => d.hasOrigin(channel.origin));
    if (
      dapp?.certifiedAttributes !== true &&
      frontendCanisterConfig.fetch_root_key[0] !== true
    ) {
      return;
    }

    const requestedKeys = paramsResult.data.keys;

    // Build the attribute specs to certify — either from implicit consent
    // or from explicit user consent through the UI.
    let attributeSpecs: Array<{
      key: string;
      value: [] | [Uint8Array];
      omit_scope: boolean;
    }>;

    // Determine if all keys are implicit consent keys.
    // First do a cheap structural check — if any key doesn't look implicit,
    // we know consent is needed without waiting for the issuer.
    // If all keys look implicit, wait for the pending OpenID issuer to
    // confirm against the actual provider.
    const allCouldBeImplicit =
      requestedKeys.length > 0 &&
      requestedKeys.every((key) => couldBeImplicitKey(key));

    let allImplicit = false;
    if (allCouldBeImplicit) {
      const pendingIssuer = await waitForStore(pendingOpenIdIssuerStore);
      allImplicit = requestedKeys.every((key) =>
        isImplicitConsentKey(key, pendingIssuer),
      );
    }

    if (requestedKeys.length === 0 || allImplicit) {
      // No consent UI needed — filter to available keys and use directly.
      if (allImplicit) {
        await waitForStore(authorizedStore);
        const authenticated = await waitForStore(authenticationStore);

        const validationResult = await validateDerivationOrigin({
          requestOrigin: channel.origin,
          derivationOrigin: paramsResult.data.icrc95DerivationOrigin,
        });
        if (validationResult.result === "invalid") {
          onError("unverified-origin");
          return;
        }

        const available = await authenticated.actor
          .list_available_attributes({
            identity_number: authenticated.identityNumber,
            attributes: [requestedKeys],
          })
          .then(throwCanisterError);
        const availableKeys = new Set(available.map(([key]) => key));
        attributeSpecs = requestedKeys
          .filter((key) => availableKeys.has(key))
          .map((key) => ({
            key,
            value: [] as [],
            omit_scope: false,
          }));
      } else {
        attributeSpecs = [];
      }
    } else {
      // Consent is needed — set a context promise so the UI can show
      // the consent view with a loading state while we wait for auth
      // and resolve available attributes.
      const contextPromise = (async (): Promise<AttributeConsentContext> => {
        await waitForStore(authorizedStore);
        const authenticated = await waitForStore(authenticationStore);

        const validationResult = await validateDerivationOrigin({
          requestOrigin: channel.origin,
          derivationOrigin: paramsResult.data.icrc95DerivationOrigin,
        });
        if (validationResult.result === "invalid") {
          onError("unverified-origin");
          return { groups: [], effectiveOrigin: "" };
        }

        const available = await authenticated.actor
          .list_available_attributes({
            identity_number: authenticated.identityNumber,
            attributes: [requestedKeys],
          })
          .then(throwCanisterError);

        return {
          groups: resolveAttributeGroups(requestedKeys, available),
          effectiveOrigin: remapToLegacyDomain(
            paramsResult.data.icrc95DerivationOrigin ?? channel.origin,
          ),
        };
      })();

      attributeConsentStore.setContext(contextPromise);
      const context = await contextPromise;

      if (context.groups.length === 0) {
        attributeSpecs = [];
      } else {
        const consent = await waitForStore(attributeConsentResultStore);
        attributeSpecs = consent.attributes.map((attr) => ({
          key: attr.key,
          value: [new Uint8Array(attr.rawValue)] as [Uint8Array],
          omit_scope: attr.omitScope,
        }));
      }
    }

    const { accountNumberPromise } = await waitForStore(authorizedStore);
    const authenticated = await waitForStore(authenticationStore);

    const origin = remapToLegacyDomain(
      paramsResult.data.icrc95DerivationOrigin ?? channel.origin,
    );

    try {
      const accountNumber = await accountNumberPromise;
      const { message } = await authenticated.actor
        .prepare_icrc3_attributes({
          origin,
          account_number: accountNumber !== undefined ? [accountNumber] : [],
          identity_number: authenticated.identityNumber,
          attributes: attributeSpecs,
          nonce: z.util.base64ToUint8Array(paramsResult.data.nonce),
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
