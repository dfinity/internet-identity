import type { Reroute } from "@sveltejs/kit";
import { WEBAUTHN_IFRAME_PATH } from "../../routes/iframe/webauthn/utils";
import { DISCOVERABLE_PASSKEY_FLOW } from "$lib/state/featureFlags";
import { get } from "svelte/store";
import { building } from "$app/environment";
import { goto, replaceState } from "$app/navigation";

export const reroute: Reroute = ({ url }) => {
  if (url.hash === WEBAUTHN_IFRAME_PATH) {
    return "/iframe/webauthn";
  }
  if (url.pathname.startsWith("/vc-flow")) {
    return "/vc-flow/index";
  }
  if (url.hash === "#authorize") {
    return get(DISCOVERABLE_PASSKEY_FLOW) ? "/authorize" : "/legacy/authorize";
  }
  if (url.pathname === "/") {
    return get(DISCOVERABLE_PASSKEY_FLOW) || building ? "/" : "/legacy";
  }
};

// SSG routes don't reroute by default anymore, so this method is used
// there to manually add the rerouting back after the page has loaded.
export const manuallyReroute = async (): Promise<boolean> => {
  const next = await reroute({
    url: new URL(window.location.href),
    fetch: window.fetch,
  });
  if (next !== undefined) {
    // Capture current URL
    const currentURL = new URL(window.location.href);
    const nextURL = new URL(next, window.location.origin);
    // Copy over the current query params
    nextURL.search = currentURL.search;
    // Reroute to destination
    await goto(nextURL, { replaceState: true });
    // After rerouting, change the URL back to what the user expects to see
    replaceState(currentURL, {});
  }
  return next !== undefined;
};
