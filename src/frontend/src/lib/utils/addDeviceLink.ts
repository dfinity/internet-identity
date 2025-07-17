/** A link in the form /?action=add-device&anchor=...
 * We use a query parameter instead of a route (e.g. /add-device) to avoid canister changes
 * and allow more flexibility */
export const addDeviceLink = ({
  userNumber,
  origin,
}: {
  userNumber: bigint;
  origin: string;
}): string => {
  return origin + "/?action=add-passkey&ii=" + userNumber;
};

/** When called from an "add device" URL, returns the anchor. Otherwise returns undefined */
export const getAddDeviceAnchor = (url: URL): bigint | undefined => {
  if (url.pathname === "/") {
    const action = url.searchParams.get("action");
    if (action !== "add-passkey") {
      return undefined;
    }

    const anchor = url.searchParams.get("ii");
    if (anchor === null) {
      return undefined;
    }

    return BigInt(anchor);
  }
};
