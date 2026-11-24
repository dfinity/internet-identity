import { toPermissionsArg } from "$lib/utils/accessLevel";
import type { Channel, JsonRequest } from "$lib/utils/transport/utils";
import {
  DelegationParamsCodec,
  DelegationResultSchema,
  INVALID_PARAMS_ERROR_CODE,
} from "$lib/utils/transport/utils";
import {
  authorizationStore,
  authorizedStore,
  type Authorized,
} from "$lib/stores/authorization.store";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import { remapToLegacyDomain } from "$lib/utils/iiConnection";
import { DelegationChain } from "@icp-sdk/core/identity";
import {
  retryFor,
  throwCanisterError,
  transformSignedDelegation,
  waitForStore,
} from "$lib/utils/utils";
import { z } from "zod";
import type { ChannelError } from "$lib/stores/channelStore";
import { authenticationStore } from "$lib/stores/authentication.store";
import {
  attributeConsentResultStore,
  attributeConsentStore,
} from "$lib/stores/attributeConsent.store";
import { get } from "svelte/store";
import { serializeSignInRequest } from "$lib/stores/channelHandlers/serialize";
import { grantedTimeToLive } from "$lib/utils/sessionDuration";
import { repairNotificationPath } from "$lib/stores/channelHandlers/notificationRepair";
import { MIN_REMAINING_LIFETIME_MS } from "$lib/stores/channelHandlers/credentialLifetime";
import { Principal } from "@icp-sdk/core/principal";

/**
 * ICRC-34: handle a delegation request from the relying party.
 *
 * Drives the authorization flow by validating the request, exposing context
 * to the UI, waiting for the user to authorize, and sending the delegation back.
 */
export const handleDelegationRequest =
  (channel: Channel, onError: (error: ChannelError) => void) =>
  async (request: JsonRequest) => {
    if (request.id === undefined || request.method !== "icrc34_delegation") {
      return;
    }
    const requestId = request.id;

    const result = DelegationParamsCodec.safeParse(request.params);
    if (!result.success) {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: z.prettifyError(result.error),
        },
      });
      onError("invalid-request");
      return;
    }

    await serializeSignInRequest(async () => {
      try {
        const params = result.data;

        const validationResult = await validateDerivationOrigin({
          requestOrigin: channel.origin,
          derivationOrigin: params.icrc95DerivationOrigin,
        });
        if (validationResult.result === "invalid") {
          onError("unverified-origin");
          return;
        }

        // Compute effective origin (derivation origin if provided, else
        // channel origin) and remap *.icp0.io to *.ic0.app for legacy
        // compatibility. Setting the context triggers the authorization UI
        // to render.
        const effectiveOrigin = remapToLegacyDomain(
          params.icrc95DerivationOrigin ?? channel.origin,
        );
        // Set the effective origin (which makes the sign-in UI render) and the
        // app's requested session duration together, so the sign-in screen
        // always sees the requested duration — the picker's ceiling — from its
        // first render. `undefined` when the app didn't specify one, in which
        // case the backend applies its default.
        authorizationStore.setRequestContext(
          effectiveOrigin,
          params.maxTimeToLive,
        );

        // The authorization a sign-in that was given up on was minted for, so the next
        // turn waits for another one.
        let previousAuthorization: Authorized | undefined;
        for (;;) {
          let authorized = await waitForStore(authorizedStore, (current) =>
            current !== previousAuthorization ? current : undefined,
          );
          while (
            get(attributeConsentStore) !== undefined &&
            get(attributeConsentResultStore) === undefined
          ) {
            const outcome = await Promise.race([
              waitForStore(attributeConsentResultStore).then(
                () => "settled" as const,
              ),
              waitForStore(authorizedStore, (current) =>
                current !== authorized ? ("switched" as const) : undefined,
              ),
            ]);
            if (outcome === "settled") {
              break;
            }
            authorized = await waitForStore(authorizedStore);
          }

          // Read the identity *after* authorization so we capture whichever
          // identity the user settled on (they may have switched mid-flow).
          const [accountNumber, { identityNumber, actor, authMethod }] =
            await Promise.all([
              authorized.accountNumberPromise,
              waitForStore(authenticationStore),
            ]);
          const ssoSessionMaxAgeNs =
            "openid" in authMethod
              ? authMethod.openid.ssoSessionMaxAgeNs
              : undefined;

          const sessionPublicKey = new Uint8Array(params.publicKey.toDer());

          // When the user chose "Questions only" during authorization, the
          // delegation is restricted to query calls via its `permissions`
          // field, which the IC enforces (update calls are rejected).
          //
          // The restricted delegation is now carried back to the relying party
          // intact: the encoded ICRC-34 result includes the `permissions` field
          // (see `DelegationResultSchema`), and `@icp-sdk/core` (>= 6) can
          // represent it on a `Delegation` instance. `permissions` is a
          // non-standard ICRC-34 extension, though, so the relying party's own
          // signer/client must also read it out of the delegation result and
          // pass it into the `Delegation` it reconstructs; only then does it
          // recompute the same canister-signed hash and the signature verify.
          // Send an explicit value rather than relying on the backend's
          // omitted-arg default.
          const permissions = toPermissionsArg(authorized.accessLevel);

          // An SSO organization caps how long its sign-ins stay valid, and the
          // delegation must not outlive that, so an SSO session sends a duration
          // even when neither the picker nor the app asked for one.
          const maxTimeToLive = grantedTimeToLive({
            authorized: authorized.maxTimeToLive,
            requested: params.maxTimeToLive,
            ssoSessionMaxAgeNs,
          });

          const mint = async () => {
            const { user_key, expiration } = await actor
              .prepare_account_delegation(
                identityNumber,
                effectiveOrigin,
                accountNumber !== undefined ? [accountNumber] : [],
                sessionPublicKey,
                maxTimeToLive !== undefined ? [maxTimeToLive] : [],
                permissions,
              )
              .then(throwCanisterError);
            const delegationChain = await retryFor(8, () =>
              actor
                .get_account_delegation(
                  identityNumber,
                  effectiveOrigin,
                  accountNumber !== undefined ? [accountNumber] : [],
                  sessionPublicKey,
                  expiration,
                  permissions,
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
            return { user_key, expiration, delegationChain };
          };
          const mintedDelegation = await mint();

          const repairOutcome = await repairNotificationPath({
            identityNumber,
            origin: effectiveOrigin,
            frontendOrigin: channel.origin,
            accountPrincipal: Principal.selfAuthenticating(
              new Uint8Array(mintedDelegation.user_key),
            ),
            actor,
          });
          // What was minted is for an identity the user has moved away from: the app gets
          // the one the user authorizes next, minted in this same request.
          if (repairOutcome === "identity-changed") {
            previousAuthorization = authorized;
            continue;
          }
          // Minted again, with the same authorization and no second check, where the
          // allow screen kept it open for most of its lifetime.
          const expiresAtMillis = Number(
            mintedDelegation.expiration / BigInt(1_000_000),
          );
          const { delegationChain } =
            expiresAtMillis - Date.now() < MIN_REMAINING_LIFETIME_MS
              ? await mint()
              : mintedDelegation;
          await channel.send({
            jsonrpc: "2.0",
            id: requestId,
            result: DelegationResultSchema.encode(delegationChain),
          });
          return;
        }
      } catch (error) {
        console.error(error);
        onError("delegation-failed");
      }
    });
  };
