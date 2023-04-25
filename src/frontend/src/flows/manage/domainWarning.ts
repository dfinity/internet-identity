import { DeviceData } from "$generated/internet_identity_types";
import { LEGACY_II_URL } from "$src/config";
import { isRecoveryPhrase } from "$src/utils/recoveryDevice";
import { isNullish } from "@dfinity/utils";
import { html, TemplateResult } from "lit-html";

// Show a domain-related warning, if necessary.
export const domainWarning = (
  device: DeviceData
): TemplateResult | undefined => {
  // Recovery phrases are not FIDO devices, meaning they are not tied to a particular origin (unless most authenticators like TouchID, etc, and e.g. recovery _devices_ in the case of YubiKeys and the like)
  if (isRecoveryPhrase(device)) {
    return undefined;
  }

  // XXX: work around didc-generated oddities in types
  const deviceOrigin =
    device.origin.length === 0 ? undefined : device.origin[0];

  // If this is the _old_ II (ic0.app) and no origin was recorded, then we can't infer much and don't show a warning.
  if (window.origin === LEGACY_II_URL && isNullish(deviceOrigin)) {
    return undefined;
  }

  // If this is the _old_ II (ic0.app) and the device has an origin that is _not_ ic0.app, then the device was probably migrated and can't be used on ic0.app anymore.
  if (window.origin === LEGACY_II_URL && deviceOrigin !== window.origin) {
    return html`This device may not be usable on the current URL
    (${window.origin})`;
  }

  // In general, if this is _not_ the _old_ II, then it's most likely the _new_ II, meaning all devices should have an origin attached.
  if (isNullish(deviceOrigin)) {
    return html`This device may not be usable on the current URL
    (${window.origin})`;
  }

  // Finally, in general if the device has an origin but this is not _this_ origin, we issue a warning
  if (deviceOrigin !== window.origin) {
    return html`This device may not be usable on the current URL
    (${window.origin})`;
  }

  return undefined;
};
