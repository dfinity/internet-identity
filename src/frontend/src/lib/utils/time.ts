export const nanosToMillis = (nanos: bigint): number =>
  Number(nanos / BigInt(1_000_000));
