import { Reroute } from "@sveltejs/kit";
import { WEBAUTHN_IFRAME_PATH } from "$lib/flows/iframeWebAuthn";
import { getAddDeviceAnchor } from "$lib/utils/addDeviceLink";
import { isNullish, nonNullish } from "@dfinity/utils";
import { DISCOVERABLE_PASSKEY_FLOW } from "$lib/state/featureFlags";
import { get } from "svelte/store";

export const reroute: Reroute = ({ url }) => {
  if (isNullish(globalThis.window)) return;

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
  // TODO: this is out of scope for this release
  // if (url.pathname === "/" && get(DISCOVERABLE_PASSKEY_FLOW)) {
  //   return "/new-authenticate";
  // }
  if (url.hash === "#authorize") {
    return get(DISCOVERABLE_PASSKEY_FLOW) ? "/new-authorize" : "/authorize";
  }
};
