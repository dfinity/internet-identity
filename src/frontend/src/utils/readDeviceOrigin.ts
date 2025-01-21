import { isNullish } from "@dfinity/utils";

// Reads the "origin" used to infer what domain a FIDO device is available on.
// The canister only allow for 50 characters, so for long domains we don't attach an origin
// (those long domains are most likely a testnet with URL like <canister id>.large03.testnet.dfinity.network, and we basically only care about identity.ic0.app & identity.internetcomputer.org).
//
// The return type is odd but that's what our didc version expects.
export const readDeviceOrigin = (): string | undefined => {
  if (isNullish(window?.origin) || window.origin.length > 50) {
    return undefined;
  }

  return window.origin;
};
