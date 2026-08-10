import type { Channel, JsonRequest } from "$lib/utils/transport/utils";
import {
  ForgetDelegationParamsSchema,
  GENERIC_ERROR_CODE,
  INVALID_PARAMS_ERROR_CODE,
} from "$lib/utils/transport/utils";
import { forgetAppDelegations } from "$lib/stores/app-delegation.store";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import { remapToLegacyDomain } from "$lib/utils/iiConnection";
import { z } from "zod";

/**
 * Forgets the delegation Internet Identity holds for the calling app, so its
 * next sign-in needs a passkey again.
 *
 * Called by an app when it signs the user out. Named "forget" rather than
 * "revoke" because nothing is invalidated: the delegation the app is holding
 * stays valid until it expires and is the app's own to clear (`signOut()` on the
 * client). All this drops is Internet Identity's ability to issue another one
 * without asking the user.
 *
 * Nothing enforces who may call it. Forgetting only ever costs a ceremony, so
 * the worst an origin can do by asking is inconvenience itself.
 */
export const handleForgetDelegationRequest =
  (channel: Channel) => async (request: JsonRequest) => {
    if (request.id === undefined || request.method !== "ii-forget-delegation") {
      return;
    }
    const requestId = request.id;

    const result = ForgetDelegationParamsSchema.safeParse(request.params ?? {});
    if (!result.success) {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: z.prettifyError(result.error),
        },
      });
      return;
    }

    const { icrc95DerivationOrigin } = result.data;

    // Same check the delegation handler runs, so an origin can only reach the
    // records of a derivation origin that has listed it.
    const validationResult = await validateDerivationOrigin({
      requestOrigin: channel.origin,
      derivationOrigin: icrc95DerivationOrigin,
    });
    if (validationResult.result === "invalid") {
      // Answered on the channel and not reported to `channelErrorStore`: this
      // request arrives while an app is signing the user out, and taking over
      // the page with an error view at that moment would be worse than letting
      // the app handle a failed call.
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: GENERIC_ERROR_CODE,
          message: "Derivation origin could not be verified",
        },
      });
      return;
    }

    await forgetAppDelegations(
      remapToLegacyDomain(icrc95DerivationOrigin ?? channel.origin),
    );

    await channel.send({ jsonrpc: "2.0", id: requestId, result: null });
  };
