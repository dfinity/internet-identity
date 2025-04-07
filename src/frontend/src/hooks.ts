import { Reroute } from "@sveltejs/kit";
import { WEBAUTHN_IFRAME_PATH } from "$lib/flows/iframeWebAuthn";
import { getAddDeviceAnchor } from "$lib/utils/addDeviceLink";
import { isNullish, nonNullish } from "@dfinity/utils";
import { DISCOVERABLE_PASSKEY_FLOW } from "$lib/utils/featureFlags";

export const reroute: Reroute = ({ url }) => {
  if (isNullish(globalThis.window)) {
    // Only reroute client side
    return;
  }
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
  if (url.pathname === "/" && DISCOVERABLE_PASSKEY_FLOW.isEnabled()) {
    return "/authenticate";
  }
};
