import type { Reroute } from "@sveltejs/kit";
import { WEBAUTHN_IFRAME_PATH } from "$lib/legacy/flows/iframeWebAuthn";
import { getAddDeviceAnchor } from "$lib/utils/addDeviceLink";
import { nonNullish } from "@dfinity/utils";
import { DISCOVERABLE_PASSKEY_FLOW } from "$lib/state/featureFlags";
import { get } from "svelte/store";
import { building } from "$app/environment";
import { goto, replaceState } from "$app/navigation";

export const reroute: Reroute = ({ url }) => {
  if (nonNullish(getAddDeviceAnchor(url))) {
    return "/register/device";
  }
  if (url.hash === WEBAUTHN_IFRAME_PATH) {
    return "/iframe/webauthn";
  }
  if (url.pathname.startsWith("/vc-flow")) {
    return "/vc-flow/index";
  }
  if (url.hash === "#authorize") {
    return get(DISCOVERABLE_PASSKEY_FLOW) ? "/authorize" : "/legacy/authorize";
  }
  // Load direct authorization page instead if openid param is present
  if (url.pathname === "/authorize" && url.searchParams.has("openid")) {
    return `/direct-authorize-protocol`;
  }
  if (url.pathname === "/") {
    return get(DISCOVERABLE_PASSKEY_FLOW) || building ? "/" : "/legacy";
  }
};

// SSG routes don't reroute by default anymore, so this method is used
// there to manually add the rerouting back after the page has loaded.
export const manuallyReroute = async () => {
  if (!building) {
    const next = await reroute({
      url: new URL(window.location.href),
      fetch: window.fetch,
    });
    if (nonNullish(next)) {
      // Capture current URL
      const currentURL = new URL(window.location.href);
      // Cast to string since `nonNullish` doesn't exclude `void` type
      const nextURL = new URL(next as string, window.location.origin);
      // Copy over the current query params
      nextURL.search = currentURL.search;
      // Reroute to destination
      await goto(nextURL, { replaceState: true });
      // After rerouting, change the URL back to what the user expects to see
      replaceState(currentURL, {});
    }
  }
};
