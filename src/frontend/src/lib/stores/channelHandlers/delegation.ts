import type { Channel, JsonRequest } from "$lib/utils/transport/utils";
import {
  DelegationParamsCodec,
  DelegationResultSchema,
  INVALID_PARAMS_ERROR_CODE,
} from "$lib/utils/transport/utils";
import {
  authorizationStore,
  authorizedStore,
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
      onError("invalid-request");
      return;
    }

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

      // Compute effective origin (derivation origin if provided, else channel
      // origin) and remap *.icp0.io to *.ic0.app for legacy compatibility.
      // Setting the context triggers the authorization UI to render.
      const effectiveOrigin = remapToLegacyDomain(
        params.icrc95DerivationOrigin ?? channel.origin,
      );
      authorizationStore.setContext(effectiveOrigin);

      // Authorization is the commit point — the user may switch identities
      // freely before this. Once authorized, the UI is no longer needed.
      const { accountNumber } = await waitForStore(authorizedStore);

      // Read the identity *after* authorization so we capture whichever
      // identity the user settled on (they may have switched mid-flow).
      const { identityNumber, actor } = await waitForStore(authenticationStore);

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
      onError("delegation-failed");
    }
  };
