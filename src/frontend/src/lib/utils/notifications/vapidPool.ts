// Where a relay endpoint points, which is all the browser still needs to know about
// VAPID: Internet Identity signs its own wake-ups with a threshold key.

/** `scheme://host[:port]` of a relay endpoint. */
export const relayOriginOf = (endpoint: string): string =>
  new URL(endpoint).origin;
