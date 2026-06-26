import { get } from "svelte/store";
import { goto } from "$app/navigation";
import {
  authenticationStore,
  type AuthenticationResult,
} from "$lib/stores/authentication.store";
import { sessionStore } from "$lib/stores/session.store";
import { type LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
import { HANDOFF_HASH_KEY, sendAuthToOpenedTab } from "$lib/utils/auth-handoff";

/**
 * Opens an authenticated management page (e.g. /manage, /manage/settings) in a
 * new tab and hands the current session to it, so the user doesn't have to sign
 * in again over there. Mirrors the authorize header's "Manage identity" flow:
 *
 *  - If the selected identity isn't authenticated yet, run the full sign-in
 *    ceremony first.
 *  - Because that passkey/IdP prompt consumes the click's transient activation
 *    on Safari (and strict Firefox), a follow-up `window.open()` would be
 *    silently blocked. In that case we surface a confirmation step whose own
 *    button click supplies fresh activation (`pending` -> `confirm`). When the
 *    user was already signed in, the popup goes straight through.
 *  - The opened tab adopts the session via `receiveAuthFromOpener` (run from
 *    the /manage authenticated layout load).
 *  - Fallbacks (auth produced nothing, or the popup was blocked) navigate the
 *    current tab to the target instead, where the user can sign in normally.
 */
export class ManageHandoffFlow {
  #authFlow = new AuthLastUsedFlow();
  #handoff: { cancel: () => void } | undefined;

  isAuthenticating = $state(false);
  // Set when the user must confirm opening the tab after authenticating, so the
  // confirm click supplies fresh transient activation for `window.open`.
  pending = $state<{ auth: AuthenticationResult; targetPath: string }>();

  // Backdrop hint while an OpenID/SSO provider overlay is showing.
  get systemOverlay(): boolean {
    return this.#authFlow.systemOverlay;
  }

  start = async (
    targetPath: string,
    selected: LastUsedIdentity,
  ): Promise<void> => {
    this.#handoff?.cancel();
    this.#handoff = undefined;
    try {
      this.isAuthenticating = true;
      const needsAuth =
        get(authenticationStore)?.identityNumber !== selected.identityNumber;
      if (needsAuth) {
        sessionStore.reset();
        this.#authFlow.init([selected.identityNumber]);
        await this.#authFlow.authenticate(selected);
      }
      const auth = get(authenticationStore);
      if (auth === undefined) {
        await goto(targetPath);
        return;
      }
      if (needsAuth) {
        // The just-completed prompt consumed this click's activation; defer the
        // popup to a fresh click on the confirmation step.
        this.pending = { auth, targetPath };
        return;
      }
      this.#openTab(targetPath, auth);
    } finally {
      this.isAuthenticating = false;
    }
  };

  confirm = (): void => {
    const pending = this.pending;
    this.pending = undefined;
    if (pending === undefined) {
      return;
    }
    this.#openTab(pending.targetPath, pending.auth);
  };

  dismiss = (): void => {
    this.#handoff?.cancel();
    this.#handoff = undefined;
    this.pending = undefined;
  };

  #openTab(targetPath: string, auth: AuthenticationResult): void {
    const w = window.open(`${targetPath}#${HANDOFF_HASH_KEY}`, "_blank");
    if (w === null) {
      void goto(targetPath);
      return;
    }
    this.#handoff = sendAuthToOpenedTab(w, auth);
  }
}
