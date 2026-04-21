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
      onError("attributes-failed");
    }
  };

/**
 * Handle `ii-icrc3-attributes` requests.
 *
 * Same as `handleLegacyAttributes` but uses the ICRC-3 attribute protocol
 * with nonce-based signatures instead of per-attribute signatures.
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

    // Filter to implicit consent keys if this is an OpenID flow.
    // Non-OpenID flows get an empty attribute list (only implicit entries).
    const configIssuer = get(pendingOpenIdIssuerStore);
    const implicitKeys =
      configIssuer !== undefined
        ? filterImplicitConsentKeys(paramsResult.data.keys, configIssuer)
        : [];

    // Filter to only keys that are actually available.
    const available =
      implicitKeys.length > 0
        ? await authenticated.actor
            .list_available_attributes({
              identity_number: authenticated.identityNumber,
              attributes: [implicitKeys],
            })
            .then(throwCanisterError)
        : [];
    const availableKeys = new Set(available.map(([key]) => key));
    const attributeKeys = implicitKeys.filter((key) => availableKeys.has(key));

    try {
      const { message } = await authenticated.actor
        .prepare_icrc3_attributes({
          origin,
          account_number: [],
          identity_number: authenticated.identityNumber,
          attributes: attributeKeys.map((key) => ({
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
      onError("attributes-failed");
    }
  };
