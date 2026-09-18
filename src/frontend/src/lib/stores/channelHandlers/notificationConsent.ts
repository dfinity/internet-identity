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
 * Asks the user whether this app may notify them, and registers this browser for Web
 * Push if they agree.
 *
 * A method of its own rather than a flag on the sign-in request, so an app can ask at a
 * moment the user can make sense of. Unreachable on the legacy transport, which emits
 * one `icrc34_delegation` request under a fixed id and rejects any other response.
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

    // Every member is optional, so an app with nothing to pass sends no `params`
    // at all. The codec rejects `undefined` but accepts `{}`.
    const parsed = NotificationConsentParamsCodec.safeParse(
      request.params ?? {},
    );
    if (!parsed.success) {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: INVALID_PARAMS_ERROR_CODE,
          message: z.prettifyError(parsed.error),
        },
      });
      // A malformed request is a protocol error rather than a denial, so the code
      // stays INVALID_PARAMS. A silent request still must not render, however it fails.
      if (!isSilent) {
        onError("invalid-request");
      }
      return;
    }

    // Consent is the user's answer, not a cached artifact, so a request that may not
    // paint is refused before anything else happens.
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
 * Authenticates the identity, runs the consent screen, then asks the canister what was
 * actually recorded, which keeps the answer to the app from drifting from the stored
 * state and covers an app that was already allowed.
 *
 * `authorizedStore` is not evidence of a session at the app: two of the three paths that
 * set it write nothing to the canister. The grant handles that itself, signing in when
 * the canister reports there is nowhere to record a consent.
 */
const runConsentCeremony = async (
  effectiveOrigin: string,
): Promise<boolean> => {
  authorizationStore.setRequestContext(effectiveOrigin, undefined);
  // Awaited for its ordering and not its value: the user has to have chosen an identity
  // before a screen can ask them about notifying it.
  await waitForStore(authorizedStore);
  const { identityNumber, actor } = await waitForStore(authenticationStore);

  notificationConsentStore.setContext({
    effectiveOrigin,
    appName: undefined,
    identityNumber,
    resolveActor: () => Promise.resolve(actor),
  });
  await waitForStore(notificationConsentSettledStore);

  return actor.notification_consent_status(identityNumber, effectiveOrigin);
};
