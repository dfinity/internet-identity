import { Reroute } from "@sveltejs/kit";
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
    return get(DISCOVERABLE_PASSKEY_FLOW)
      ? "/legacy-authorize-protocol"
      : "/legacy/authorize";
  }
  // Load direct authorization page instead if openid param is present
  if (url.pathname === "/authorize" && url.searchParams.has("openid")) {
    return `/direct-authorize-protocol`;
  }
  if (url.pathname === "/") {
    return get(DISCOVERABLE_PASSKEY_FLOW) || building ? "/" : "/legacy";
  }
};

// SSG routes don't reroute by default anymore, so this method is used in
// these routes to manually add the rerouting back as early as possible,
// before hydration has even started (so intentionally outside onMount).
export const manuallyReroute = async () => {
  if (!building) {
    const next = await reroute({
      url: new URL(window.location.href),
      fetch: window.fetch,
    });
    if (nonNullish(next)) {
      const before = window.location.href;
      // Reroute to destination, `nonNullish` doesn't exclude `void` so we cast
      await goto(next as string, { replaceState: true });
      // After rerouting, change the URL back to what the user expects to see
      replaceState(before, {});
    }
  }
};
