import type { Channel, JsonRequest } from "$lib/utils/transport/utils";
import {
  INTERACTION_REQUIRED_ERROR_CODE,
  INVALID_PARAMS_ERROR_CODE,
  OriginSchema,
} from "$lib/utils/transport/utils";
import {
  authorizationPromptStore,
  authorizationStore,
  authorizedStore,
} from "$lib/stores/authorization.store";
import { authenticationStore } from "$lib/stores/authentication.store";
import {
  notificationConsentSettledStore,
  notificationConsentStore,
} from "$lib/stores/notificationConsent.store";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import { remapToLegacyDomain } from "$lib/utils/urlUtils";
import { waitForStore } from "$lib/utils/utils";
import { serializeAuthorizationRequest } from "$lib/stores/channelHandlers/serialize";
import { get } from "svelte/store";
import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
import { z } from "zod";
import type { ChannelError } from "$lib/stores/channelStore";

export const NOTIFICATION_CONSENT_METHOD = "ii_notification_consent";

const NotificationConsentParamsCodec = z.object({
  icrc95DerivationOrigin: z.optional(OriginSchema),
});

/**
 * Asks the user whether this app may notify them, and registers this browser
 * for Web Push if they agree.
 *
 * A method of its own rather than a flag on the sign-in request: the app can
 * then ask at a moment the user can make sense of, instead of deciding before
 * it knows anything about them. The cost is that an app on the legacy transport
 * cannot reach this, since that transport emits one `icrc34_delegation` request
 * under a fixed id and rejects a response to any other.
 */
export const handleNotificationConsentRequest =
  (channel: Channel, onError: (error: ChannelError) => void) =>
  async (request: JsonRequest) => {
    if (
      request.id === undefined ||
      request.method !== NOTIFICATION_CONSENT_METHOD ||
      !get(PUSH_NOTIFICATIONS)
    ) {
      return;
    }
    const requestId = request.id;

    const isSilent = get(authorizationPromptStore).prompt === "none";
    const deny = async () => {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: INTERACTION_REQUIRED_ERROR_CODE,
          message: "Interaction required",
        },
      });
    };

    const parsed = NotificationConsentParamsCodec.safeParse(request.params);
    if (!parsed.success) {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: z.prettifyError(parsed.error),
        },
      });
      // A malformed request is a protocol error rather than a denial, so the
      // code stays INVALID_PARAMS. What the silent path must not do is render:
      // it was asked to answer without showing anything, and that holds however
      // it fails.
      if (!isSilent) {
        onError("invalid-request");
      }
      return;
    }

    // There is nothing to hand back without asking — consent is the user's
    // answer, not a cached artifact — so a request that may not paint is
    // refused before anything else happens.
    if (isSilent) {
      await deny();
      return;
    }

    await serializeAuthorizationRequest(async () => {
      try {
        const params = parsed.data;
        const validation = await validateDerivationOrigin({
          requestOrigin: channel.origin,
          derivationOrigin: params.icrc95DerivationOrigin,
        });
        if (validation.result === "invalid") {
          onError("unverified-origin");
          return;
        }

        const effectiveOrigin = remapToLegacyDomain(
          params.icrc95DerivationOrigin ?? channel.origin,
        );

        const granted = await runConsentCeremony(effectiveOrigin);

        await channel.send({
          jsonrpc: "2.0",
          id: requestId,
          result: { granted },
        });
      } catch (error) {
        console.error(error);
        onError("notification-consent-failed");
      } finally {
        notificationConsentStore.clear();
      }
    });
  };

/**
 * Signs the user in if they are not already, runs the consent screen, then asks
 * the canister what was actually recorded. Reading the outcome back rather than
 * reporting what the screen thinks keeps the answer to the app and the state in
 * the canister from drifting apart, and covers an app that was already allowed.
 */
const runConsentCeremony = async (
  effectiveOrigin: string,
): Promise<boolean> => {
  authorizationStore.setRequestContext(effectiveOrigin, undefined);
  const authorized = await waitForStore(authorizedStore);
  const { identityNumber, actor } = await waitForStore(authenticationStore);

  notificationConsentStore.setContext({
    effectiveOrigin,
    appName: undefined,
    identityNumber,
    accountNumber: authorized.accountNumberPromise,
    resolveActor: () => Promise.resolve(actor),
  });
  await waitForStore(notificationConsentSettledStore);

  return actor.notification_consent_status(identityNumber, effectiveOrigin);
};
