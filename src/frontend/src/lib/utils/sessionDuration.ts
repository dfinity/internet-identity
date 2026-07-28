/**
 * Session-duration helpers shared by the sign-in ("Continue") screen and the
 * MCP connect screen.
 *
 * Durations are handled in **seconds** here — the unit the pickers work in —
 * and converted to the **nanoseconds** the canister expects (`maxTimeToLive`)
 * only at the boundary via {@link sessionDurationToNanos}.
 */

const MINUTE = 60;
const HOUR = 60 * MINUTE;
const DAY = 24 * HOUR;

/** The longest session II grants, in seconds. Matches the backend's 30-day
 *  session-delegation cap (`MAX_SESSION_DELEGATION_TTL_NS`), which is also the
 *  default when a request omits `maxTimeToLive`. Flows use it as the ceiling
 *  when they place no tighter limit of their own. */
export const MAX_SESSION_DURATION_SECONDS = 30 * DAY;

const NANOS_PER_SECOND = BigInt(1_000_000_000);
const MAX_SESSION_DURATION_NANOS =
  BigInt(MAX_SESSION_DURATION_SECONDS) * NANOS_PER_SECOND;

/** The units an off-preset duration is broken down into, largest first. */
export type SessionDurationUnit = "day" | "hour" | "minute" | "second";

/** One unit of a duration, e.g. `{ unit: "minute", value: 20 }`. */
export interface SessionDurationPart {
  unit: SessionDurationUnit;
  value: number;
}

/**
 * An off-preset duration split into its non-zero units, largest first, e.g.
 * `7200 -> [{ hour, 2 }]`, `90_060 -> [{ day, 1 }, { hour, 1 }, { minute, 1 }]`.
 * The caller turns each part into a localized, spelled-out label (see
 * `SessionDurationSelect`) so an off-preset value reads like the named presets
 * ("20 minutes") instead of a compact "20m". A zero duration yields a single
 * zero-second part, so a label is never empty.
 */
export const sessionDurationParts = (
  seconds: number,
): SessionDurationPart[] => {
  const parts: SessionDurationPart[] = [];
  const day = Math.floor(seconds / DAY);
  const hour = Math.floor((seconds % DAY) / HOUR);
  const minute = Math.floor((seconds % HOUR) / MINUTE);
  const second = seconds % MINUTE;
  if (day > 0) parts.push({ unit: "day", value: day });
  if (hour > 0) parts.push({ unit: "hour", value: hour });
  if (minute > 0) parts.push({ unit: "minute", value: minute });
  if (second > 0) parts.push({ unit: "second", value: second });
  return parts.length > 0 ? parts : [{ unit: "second", value: 0 }];
};

/**
 * The selectable durations (seconds, ascending) for a picker capped at
 * `maxSeconds`: every preset up to and including the cap, plus the cap itself
 * when it isn't already a preset — so the full allowed duration always stays
 * pickable (e.g. a 2-hour cap yields `[10min, 1h, 2h]`).
 */
export const cappedSessionDurations = (
  maxSeconds: number,
  presetValues: number[],
): number[] => {
  const values = presetValues.filter((value) => value <= maxSeconds);
  if (!values.includes(maxSeconds)) {
    values.push(maxSeconds);
  }
  return values.sort((a, b) => a - b);
};

/**
 * The ceiling (seconds) a sign-in picker should offer, given the app's
 * requested `maxTimeToLive` (nanoseconds): the request capped at the 30-day
 * maximum, or the maximum itself when the app didn't request a duration (which
 * matches the backend's default, so a plain "Continue" behaves as it did before
 * the picker existed). Capping in nanoseconds first avoids precision loss in the
 * conversion to seconds.
 */
export const sessionDurationCeilingSeconds = (
  requestedMaxTimeToLive: bigint | undefined,
): number => {
  // A missing request — or a malformed/hostile non-positive one (the transport
  // decodes any bigint, negatives included) — falls back to the maximum, the
  // same duration an omitted request yields, rather than producing a negative
  // or zero ceiling that would break the picker or send a nonsensical TTL on.
  if (
    requestedMaxTimeToLive === undefined ||
    requestedMaxTimeToLive <= BigInt(0)
  ) {
    return MAX_SESSION_DURATION_SECONDS;
  }
  const capped =
    requestedMaxTimeToLive < MAX_SESSION_DURATION_NANOS
      ? requestedMaxTimeToLive
      : MAX_SESSION_DURATION_NANOS;
  // At least one second, so a sub-second request can't collapse to a 0-second
  // ceiling (and thus an empty/"0s" picker).
  return Math.max(1, Number(capped / NANOS_PER_SECOND));
};

/** Convert a duration in seconds to the nanoseconds the canister expects. */
export const sessionDurationToNanos = (seconds: number): bigint =>
  BigInt(seconds) * NANOS_PER_SECOND;
