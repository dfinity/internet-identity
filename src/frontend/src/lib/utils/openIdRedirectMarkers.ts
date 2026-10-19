/**
 * sessionStorage keys shared between the frontend and the frontend canister's
 * POST /callback landing page (`src/internet_identity_frontend/src/callback.rs`).
 *
 * The landing page routes on them: when a same-tab marker is present it
 * stashes the IdP's payload under {@link OPENID_CALLBACK_DATA_KEY} and
 * navigates the tab back into the app; otherwise it delivers the payload to
 * the tab that opened it (the popup flow) and closes.
 *
 * Kept in a leaf module so both the popup helper (`openID.ts`) and the
 * same-tab redirect (`openIdSameTabRedirect.ts`) can import them without a
 * cycle.
 */

/** The IdP's callback payload, stashed by the landing page for a same-tab resume. */
export const OPENID_CALLBACK_DATA_KEY = "ii-openid-callback-data";

/**
 * Same-tab marker: the in-app path the landing page navigates back to. Set by
 * the wizard's same-tab redirect (`openIdSameTabRedirect.ts`).
 */
export const OPENID_RETURN_PATH_KEY = "ii-openid-return-path";

/**
 * Same-tab marker of the authorize page's 1-click flow (the expected CSRF
 * `state`). The landing page keeps honoring it on its own, returning to
 * `/authorize?flow=openid-resume`, so a flow started before a deploy still
 * lands.
 */
export const OPENID_AUTHORIZE_STATE_KEY = "ii-openid-authorize-state";

/** The wizard's own same-tab resume record (see `openIdSameTabRedirect.ts`). */
export const OPENID_WIZARD_RESUME_KEY = "ii-openid-wizard-resume";

/**
 * The relying-party request(s) in flight when the authorize wizard left for the
 * IdP in the same tab, stashed so the channel re-established on return can
 * replay them (the RP's client does not re-send them). Set by
 * `redirectToOpenIdForAuthorize`, consumed by the authorize layout's resume.
 */
export const OPENID_PENDING_REQUESTS_KEY = "ii-pending-authorize-requests";

/**
 * Remove every same-tab marker from this tab's sessionStorage.
 *
 * Called before a flow starts, popup or same-tab, so the landing page only
 * ever sees the marker of the flow that is actually in progress. This matters
 * for the popup flow in particular: a popup inherits a copy of its opener's
 * sessionStorage, so a marker left behind by an abandoned same-tab sign-in
 * would make the popup's landing page navigate into the app instead of
 * delivering the payload to the opener.
 */
export const clearOpenIdRedirectMarkers = (): void => {
  try {
    sessionStorage.removeItem(OPENID_RETURN_PATH_KEY);
    sessionStorage.removeItem(OPENID_AUTHORIZE_STATE_KEY);
    sessionStorage.removeItem(OPENID_WIZARD_RESUME_KEY);
    sessionStorage.removeItem(OPENID_CALLBACK_DATA_KEY);
    sessionStorage.removeItem(OPENID_PENDING_REQUESTS_KEY);
  } catch {
    // sessionStorage can be unavailable (e.g. storage access denied); the
    // markers can't have been written either, so there is nothing to clear.
  }
};
