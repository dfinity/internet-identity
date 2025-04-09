import { Reroute } from "@sveltejs/kit";
import { WEBAUTHN_IFRAME_PATH } from "$lib/flows/iframeWebAuthn";
import { getAddDeviceAnchor } from "$lib/utils/addDeviceLink";
import { nonNullish } from "@dfinity/utils";
import { DISCOVERABLE_PASSKEY_FLOW } from "$lib/state/featureFlags";
import { get } from "svelte/store";

export const reroute: Reroute = ({ url }) => {
  if (nonNullish(getAddDeviceAnchor(url))) {
    return "/register/device";
  }
  if (url.hash === "#authorize") {
    return "/authorize";
  }
  if (url.hash === WEBAUTHN_IFRAME_PATH) {
    return "/iframe/webauthn";
  }
  if (url.pathname.startsWith("/vc-flow")) {
    return "/vc-flow/index";
  }
  if (url.pathname === "/" && get(DISCOVERABLE_PASSKEY_FLOW)) {
    return "/authenticate";
  }
};
