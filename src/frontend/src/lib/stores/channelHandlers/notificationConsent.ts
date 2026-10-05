import type { Channel, JsonRequest } from "$lib/utils/transport/utils";
import {
  INTERACTION_REQUIRED_ERROR_CODE,
  INVALID_PARAMS_ERROR_CODE,
  METHOD_NOT_FOUND_ERROR_CODE,
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
import {
  notificationsUnavailableHere,
  readBrowserPushState,
  readGranted,
  resolveOptIn,
  type BrowserPushState,
} from "$lib/utils/notifications/notificationState";
import { claimScreen } from "$lib/stores/pendingScreen.store";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import { remapToLegacyDomain } from "$lib/utils/urlUtils";
import { waitForStore } from "$lib/utils/utils";
import {
  serializeAuthorizationRequest,
  signInWaitingStore,
} from "$lib/stores/channelHandlers/serialize";
import { get } from "svelte/store";
import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
import { z } from "zod";
import type { ChannelError } from "$lib/stores/channelStore";

export const NOTIFICATION_CONSENT_METHOD = "ii_notification_consent";

/**
 * Ceremonies that have run.
 *
 * A request probes this browser as soon as it is accepted, which is a head start and
 * not a cache: a ceremony ahead of it in the queue can subscribe the browser its probe
 * found without a subscription, and the screen would then ask for a device that is
 * already set up. Counting them is what tells the two apart. Only a ceremony can come
 * between a probe and its use, because the queue runs them one at a time.
 */
let ceremoniesRun = 0;

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
      request.method !== NOTIFICATION_CONSENT_METHOD
    ) {
      return;
    }
    const requestId = request.id;

    // Answered rather than dropped. This deployment notifies for nothing at all, so
    // the method is one it does not have, which is what the code says; an app that
    // asked anyway gets that back instead of waiting on a reply that never comes.
    // Ahead of the silent check, because a request that cannot be served is not a
    // request that needed the user.
    if (!get(PUSH_NOTIFICATIONS)) {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: METHOD_NOT_FOUND_ERROR_CODE,
          message: "This Internet Identity does not send notifications",
        },
      });
      return;
    }

    const isSilent = get(authorizationPromptStore).prompt === "none";

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
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        error: {
          code: INTERACTION_REQUIRED_ERROR_CODE,
          message: "Interaction required",
        },
      });
      return;
    }

    // Started here rather than from the screen: it needs no identity, so it runs
    // while this request waits its turn behind a sign-in instead of after one.
    // Answers rather than rejects, so a request that returns below only drops it.
    const browser = readBrowserPushState();

    const probedAt = ceremoniesRun;

    // Held from here until this request has answered for itself, so authorizing does
    // not take the screen the user is on before we know whether we need it.
    const releaseScreen = claimScreen();

    // A sign-in queued behind this request is handed the turn, so the request is
    // queued again until it has answered.
    for (;;) {
      const outcome = await serializeAuthorizationRequest(async () => {
        const outcome = await answerConsent(
          channel,
          onError,
          requestId,
          parsed.data,
          browser,
          probedAt,
          releaseScreen,
        );
        if (outcome === "answered") {
          releaseScreen();
          notificationConsentStore.clear();
        }
        return outcome;
      });
      if (outcome === "answered") {
        return;
      }
    }
  };

/** Whether a turn answered the app, or handed the queue to a sign-in. */
type TurnOutcome = "answered" | "yielded";

const answerConsent = async (
  channel: Channel,
  onError: (error: ChannelError) => void,
  requestId: NonNullable<JsonRequest["id"]>,
  params: z.infer<typeof NotificationConsentParamsCodec>,
  browser: Promise<BrowserPushState | undefined>,
  probedAt: number,
  releaseScreen: () => void,
): Promise<TurnOutcome> => {
  try {
    const validation = await validateDerivationOrigin({
      requestOrigin: channel.origin,
      derivationOrigin: params.icrc95DerivationOrigin,
    });
    if (validation.result === "invalid") {
      onError("unverified-origin");
      return "answered";
    }

    const effectiveOrigin = remapToLegacyDomain(
      params.icrc95DerivationOrigin ?? channel.origin,
    );

    // No notifications on iOS yet, so nothing is offered and the app is told
    // plainly that it may not notify here rather than being refused outright:
    // the method exists, this browser just has no answer but no.
    if (notificationsUnavailableHere()) {
      await channel.send({
        jsonrpc: "2.0",
        id: requestId,
        result: { granted: false },
      });
      return "answered";
    }

    const granted = await runConsentCeremony(
      effectiveOrigin,
      browser,
      probedAt,
      releaseScreen,
    );
    if (granted === "yielded") {
      return "yielded";
    }

    await channel.send({
      jsonrpc: "2.0",
      id: requestId,
      result: { granted },
    });
    return "answered";
  } catch (error) {
    console.error(error);
    onError("notification-consent-failed");
    return "answered";
  }
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
  probedBrowser: Promise<BrowserPushState | undefined>,
  probedAt: number,
  releaseScreen: () => void,
): Promise<boolean | "yielded"> => {
  // The probe stands only where no ceremony has run since it was taken. Read again
  // rather than ask about a browser one of them may have set up in the meantime.
  const browser =
    probedAt === ceremoniesRun ? probedBrowser : readBrowserPushState();

  authorizationStore.setRequestOrigin(effectiveOrigin);
  try {
    return await askUntilSettled(effectiveOrigin, browser, releaseScreen);
  } finally {
    // Whatever came of it, a ceremony that has run is one that may have subscribed
    // this browser, so every probe taken before now is suspect.
    ceremoniesRun += 1;
  }
};

/** Opens the screen for each identity the user settles on, until one answers. */
const askUntilSettled = async (
  effectiveOrigin: string,
  browser: Promise<BrowserPushState | undefined>,
  /** Called as soon as this request will put nothing more on screen, which is
   *  before the answer is read back: the redirect is what belongs on screen for
   *  that, not the screen the user came from. */
  releaseScreen: () => void,
): Promise<boolean | "yielded"> => {
  for (;;) {
    // Awaited for its ordering and not its value: the user has to have chosen an
    // identity before a screen can ask them about notifying it. A sign-in waiting
    // behind this takes the turn instead: it carries the app's requested session
    // duration for the screen the user signs in on, and it registers this browser,
    // which allowing needs.
    const turn = await Promise.race([
      waitForStore(authorizedStore).then(() => "authorized" as const),
      waitForStore(signInWaitingStore, (waiting) =>
        waiting ? ("yielded" as const) : undefined,
      ),
    ]);
    if (turn === "yielded" || get(signInWaitingStore)) {
      return "yielded";
    }
    const authenticated = await waitForStore(authenticationStore);

    // Resolved before the context is set, so the screen opens on the question it
    // will ask. Nothing left to ask answers from what this already read, and puts
    // no screen between the sign-in and the app.
    const resolution = await resolveOptIn({
      identityNumber: authenticated.identityNumber,
      origin: effectiveOrigin,
      actor: authenticated.actor,
      browser,
    });
    if (resolution.screen === "skip") {
      releaseScreen();
      return resolution.granted;
    }
    // Signed in already, but the sign-in that registers this browser has yet to run.
    if (get(signInWaitingStore)) {
      return "yielded";
    }

    notificationConsentStore.setContext({
      effectiveOrigin,
      identityNumber: authenticated.identityNumber,
      actor: authenticated.actor,
      device: resolution.state,
      consented: resolution.consented,
    });

    // The header keeps the identity switcher up for this screen, and switching
    // leaves it holding the identity it opened for, so a grant would land on one
    // identity while the answer was read for another. Start it again instead.
    const outcome = await Promise.race([
      waitForStore(notificationConsentSettledStore).then(
        () => "settled" as const,
      ),
      waitForStore(authenticationStore, (current) =>
        current?.identityNumber !== authenticated.identityNumber
          ? ("switched" as const)
          : undefined,
      ),
      // A sign-in that arrives while the screen waits still registers this browser
      // for the allowing to land on, so the screen steps aside until it has.
      waitForStore(signInWaitingStore, (waiting) =>
        waiting ? ("yielded" as const) : undefined,
      ),
    ]);
    if (outcome === "switched") {
      continue;
    }
    if (outcome === "yielded") {
      notificationConsentStore.clear();
      return "yielded";
    }
    releaseScreen();

    // What the app is told is read back rather than reported from the screen, and it
    // is both halves: the consent this identity holds, and a browser that delivers.
    return readGranted({
      identityNumber: authenticated.identityNumber,
      origin: effectiveOrigin,
      actor: authenticated.actor,
    });
  }
};
