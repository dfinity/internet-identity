const BROADCAST_CHANNEL = "redirect_callback";
export const REDIRECT_CALLBACK_PATH = "/callback";

export class CallbackPopupClosedError extends Error {}

/**
 * Payload the canister's POST /callback landing page delivers to the
 * frontend — via `BroadcastChannel` in the popup flow, via the
 * `ii-openid-callback-data` sessionStorage entry in the same-tab flow.
 * Mirrors what the OAuth callback fragment used to carry: either the token
 * or the IdP's RFC 6749 error report, plus the CSRF `state` in both cases.
 */
export type CallbackPayload =
  | { id_token: string; state: string }
  | { error: string; error_description?: string; state: string };

/**
 * Read a string property off an unknown-shaped value without type casts.
 * Returns `undefined` when the property is absent or not a string.
 */
export const getStringProp = (
  value: object,
  key: string,
): string | undefined => {
  const prop: unknown = Reflect.get(value, key);
  return typeof prop === "string" ? prop : undefined;
};

/**
 * Whether a value posted on the callback channel (or parsed from the
 * sessionStorage entry) is a {@link CallbackPayload}. Unrelated messages
 * (e.g. the legacy callback page posting a URL string) don't match.
 */
const isCallbackPayload = (value: unknown): boolean =>
  typeof value === "object" &&
  value !== null &&
  getStringProp(value, "state") !== undefined &&
  (getStringProp(value, "id_token") !== undefined ||
    getStringProp(value, "error") !== undefined);

/**
 * Open a popup that round-trips through an OAuth provider and resolves with
 * the callback payload delivered by the canister's POST /callback page.
 *
 * Accepts either:
 * - A `string` URL: navigated to immediately. Used by the synchronous flows
 *   where the redirect URL is known at click time.
 * - A `Promise<string>`: the popup is opened to `about:blank` first
 *   (synchronously, to consume the user-activation token before any
 *   `await` — Safari blocks `window.open` after an awaited Promise),
 *   then navigated once the URL resolves. Used for flows that need an
 *   async step (e.g. SSO two-hop discovery) before the redirect URL is
 *   known. If the promise rejects, the popup is closed and the outer
 *   promise rejects with the same error.
 *
 * The payload is resolved as `unknown`: it crosses a BroadcastChannel, so
 * the consumer (`extractIdTokenFromCallback`) revalidates its shape along
 * with the CSRF state check.
 */
export const redirectInPopup = (
  url: string | Promise<string>,
): Promise<unknown> => {
  const width = 500;
  const height = 600;
  const left = (window.innerWidth - width) / 2 + window.screenX;
  const top = (window.innerHeight - height) / 2 + window.screenY;
  // For deferred URLs, open about:blank synchronously so we don't lose
  // the user-activation token — same-origin (inherited), so we can later
  // navigate via `redirectWindow.location.href = ...` even though we'll
  // end up on a cross-origin IdP.
  const initialUrl = typeof url === "string" ? url : "about:blank";
  const redirectWindow = window.open(
    initialUrl,
    "_blank",
    `width=${width},height=${height},left=${left},top=${top}`,
  );
  if (redirectWindow === null) {
    throw new CallbackPopupClosedError();
  }

  return new Promise<unknown>((resolve, reject) => {
    const cleanup = () => {
      clearInterval(closeInterval);
      channel.close();
      redirectWindow?.close();
      window.focus();
    };
    // Periodically check if popup was closed by the user.
    // We can't listen for close events due to cross-origin restrictions,
    // so we poll every 500ms to detect closure. The interval balances
    // responsiveness with resource consumption.
    const closeInterval = setInterval(() => {
      if (redirectWindow.closed === true) {
        cleanup();
        reject(new CallbackPopupClosedError());
      }
    }, 500);
    // Listen to the popup, we expect a message with the payload of the
    // callback, after receiving it we can close the popup and resolve the
    // promise.
    const channel = new BroadcastChannel(BROADCAST_CHANNEL);
    channel.addEventListener("message", (event) => {
      const data: unknown = event.data;
      if (!isCallbackPayload(data)) {
        return;
      }
      cleanup();
      resolve(data);
    });

    if (typeof url !== "string") {
      url.then(
        (resolvedUrl) => {
          // The user may have closed the popup or the close-poller may have
          // already rejected during the await — `closed` covers both.
          if (redirectWindow.closed) {
            return;
          }
          redirectWindow.location.href = resolvedUrl;
        },
        (error: unknown) => {
          cleanup();
          reject(error);
        },
      );
    }
  });
};

export const sendUrlToOpener = (): void => {
  const channel = new BroadcastChannel(BROADCAST_CHANNEL);
  channel.postMessage(window.location.href);
  channel.close();
};
