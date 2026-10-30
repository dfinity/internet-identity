import { get } from "svelte/store";
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
} from "$lib/utils/openIdRedirectMarkers";

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
 */
export const redirectToOpenIdForAuthorize = (config: OpenIdConfig): void => {
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
  window.location.assign(next);
};
