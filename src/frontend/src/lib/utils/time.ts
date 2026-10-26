const NANOS_PER_MILLI = BigInt(1_000_000);

export const nanosToMillis = (nanos: bigint): number =>
  Number(nanos / NANOS_PER_MILLI);

/** The canister keeps times in nanoseconds and `Date.now` answers in milliseconds. */
export const millisToNanos = (millis: number): bigint =>
  BigInt(millis) * NANOS_PER_MILLI;
