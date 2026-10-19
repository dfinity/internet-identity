import { get } from "svelte/store";
import { z } from "zod";
import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
import { establishedChannelStore } from "$lib/stores/channelStore";
import { sessionStore } from "$lib/stores/session.store";
import {
  DirectOpenIdEvents,
  directOpenIdFunnel,
} from "$lib/utils/analytics/DirectOpenIdFunnel";
import { createRedirectURL } from "$lib/utils/openID";
import {
  clearOpenIdRedirectMarkers,
  OPENID_AUTHORIZE_STATE_KEY,
  OPENID_PENDING_REQUESTS_KEY,
} from "$lib/utils/openIdRedirectMarkers";
import {
  type JsonRequest,
  JsonRequestSchema,
} from "$lib/utils/transport/utils";

/**
 * Leave the authorize page for the OpenID provider in this tab.
 *
 * The canister's POST /callback landing page brings the tab back to
 * `/authorize?flow=openid-resume`, where the page picks the sign-in up again
 * (`resumeOpenId` in `+page.svelte`) and the layout re-establishes the
 * channel to the relying party from the origin and resume token stashed
 * here.
 *
 * Shared by the 1-click entry (`?openid=<issuer>`) and the wizard's same-tab
 * path (Sign in with Apple on WebKit, see `openIdSameTabRedirect.ts`): both
 * come back through the same 1-click resume, which authorizes the identity's
 * primary account with full access once signed in.
 *
 * The two entries differ in one way that this function papers over. On the
 * 1-click entry the channel is `pending` from the start, so the dapp's client
 * has not yet sent its delegation request (it queues it) and keeps the channel
 * open across the round trip; on return the queued request is flushed to the
 * resumed channel. On the wizard entry the channel was established normally:
 * the client has already sent its request and is awaiting a response on a
 * channel it will neither re-handshake nor re-send over, and whose short
 * ready-timeout would otherwise close the popup mid-redirect. So when such a
 * request is in flight, stash it to replay on return (see
 * {@link consumePendingAuthorizeRequests}) and switch the channel to `pending`
 * first, so the client waits for the round trip instead of giving up.
 */
export const redirectToOpenIdForAuthorize = (
  config: OpenIdConfig,
): Promise<void> => {
  directOpenIdFunnel.trigger(DirectOpenIdEvents.RedirectToOpenId, {
    openid_issuer: config.issuer,
  });
  const channel = get(establishedChannelStore);
  const next = createRedirectURL(
    {
      clientId: config.client_id,
      authURL: config.auth_uri,
      authScope: config.auth_scope.join(" "),
    },
    {
      nonce: get(sessionStore).nonce,
      mediation: "required",
    },
  );
  // Read before clearing markers (which navigation state is independent of) so
  // an in-flight request is carried over, not dropped.
  const pendingRequests = channel.pendingRequests;
  // One flow at a time: whatever another flow left behind would otherwise
  // misroute this one's callback.
  clearOpenIdRedirectMarkers();
  sessionStorage.setItem("ii-pending-channel-origin", channel.origin);
  // Stash the established channel's resume token so the transport can prove,
  // on the return load, that the resume belongs to this flow (see the URL
  // transport's resume gate).
  sessionStorage.setItem(
    "ii-pending-channel-resume-token",
    channel.resumeToken,
  );
  sessionStorage.setItem(
    OPENID_AUTHORIZE_STATE_KEY,
    next.searchParams.get("state")!,
  );
  // The 1-click entry has no request in flight (the client is still queuing it),
  // so it keeps its exact behavior: navigate straight away, synchronously. Only
  // the wizard entry on a normally-established channel has one — stash it to
  // replay on return and hold the client open across the round trip before
  // navigating. The synchronous work above (which can throw) runs for both so
  // the caller's own error handling still sees it; only this tail is deferred.
  if (pendingRequests.length === 0) {
    window.location.assign(next);
    return Promise.resolve();
  }
  stashPendingAuthorizeRequests(pendingRequests);
  return (
    channel
      .markPending()
      // Best effort: navigating anyway only risks the pre-fix behavior.
      .catch((error: unknown) => console.error(error))
      .then(() => window.location.assign(next))
  );
};

/**
 * Stash the relying-party requests that were in flight when the authorize
 * wizard left for the IdP, so the channel re-established on return can replay
 * them to the handlers (the dapp's client won't re-send them itself).
 */
const stashPendingAuthorizeRequests = (requests: JsonRequest[]): void => {
  sessionStorage.setItem(OPENID_PENDING_REQUESTS_KEY, JSON.stringify(requests));
};

/**
 * Take the stashed in-flight authorize request(s), if any, to seed the resumed
 * channel with. Single-use: removed before it is parsed, so a malformed or
 * stale entry can't be replayed on a later load. Returns an empty list when
 * there is nothing to resume (the common case, including every 1-click flow).
 */
export const consumePendingAuthorizeRequests = (): JsonRequest[] => {
  const raw = sessionStorage.getItem(OPENID_PENDING_REQUESTS_KEY);
  sessionStorage.removeItem(OPENID_PENDING_REQUESTS_KEY);
  if (raw === null) {
    return [];
  }
  try {
    const result = z.array(JsonRequestSchema).safeParse(JSON.parse(raw));
    return result.success ? result.data : [];
  } catch {
    return [];
  }
};
