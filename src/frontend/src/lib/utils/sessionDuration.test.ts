import {
  cappedSessionDurations,
  MAX_SESSION_DURATION_SECONDS,
  sessionDurationBreakdown,
  sessionDurationCeilingSeconds,
  sessionDurationToNanos,
} from "./sessionDuration";

const MINUTE = 60;
const HOUR = 60 * MINUTE;
const DAY = 24 * HOUR;
const WEEK = 7 * DAY;
const NANOS_PER_SECOND = BigInt(1_000_000_000);

const PRESETS = [10 * MINUTE, HOUR, 8 * HOUR, DAY, WEEK, 30 * DAY];

describe("sessionDurationBreakdown", () => {
  it("breaks out a single unit", () => {
    expect(sessionDurationBreakdown(2 * HOUR)).toEqual({ hour: 2 });
    expect(sessionDurationBreakdown(10 * MINUTE)).toEqual({ minute: 10 });
    expect(sessionDurationBreakdown(DAY)).toEqual({ day: 1 });
  });

  it("breaks out every non-zero unit", () => {
    expect(sessionDurationBreakdown(DAY + HOUR + MINUTE + 1)).toEqual({
      day: 1,
      hour: 1,
      minute: 1,
      second: 1,
    });
    expect(sessionDurationBreakdown(DAY + MINUTE)).toEqual({
      day: 1,
      minute: 1,
    });
  });

  it("keeps a zero duration as zero seconds", () => {
    expect(sessionDurationBreakdown(0)).toEqual({ second: 0 });
  });
});

describe("cappedSessionDurations", () => {
  it("keeps only presets up to a preset cap", () => {
    expect(cappedSessionDurations(8 * HOUR, PRESETS)).toEqual([
      10 * MINUTE,
      HOUR,
      8 * HOUR,
    ]);
  });

  it("adds an off-preset cap as the top option", () => {
    expect(cappedSessionDurations(2 * HOUR, PRESETS)).toEqual([
      10 * MINUTE,
      HOUR,
      2 * HOUR,
    ]);
  });

  it("offers only the cap when it's below the smallest preset", () => {
    expect(cappedSessionDurations(MINUTE, PRESETS)).toEqual([MINUTE]);
  });

  it("offers every preset when the cap is the maximum", () => {
    expect(cappedSessionDurations(30 * DAY, PRESETS)).toEqual(PRESETS);
  });

  it("returns the values sorted ascending", () => {
    const result = cappedSessionDurations(WEEK, PRESETS);
    expect(result).toEqual([...result].sort((a, b) => a - b));
  });
});

describe("sessionDurationCeilingSeconds", () => {
  it("defaults to the maximum when no duration was requested", () => {
    expect(sessionDurationCeilingSeconds(undefined)).toBe(
      MAX_SESSION_DURATION_SECONDS,
    );
  });

  it("converts a requested duration from nanoseconds to seconds", () => {
    expect(
      sessionDurationCeilingSeconds(BigInt(2 * HOUR) * NANOS_PER_SECOND),
    ).toBe(2 * HOUR);
  });

  it("caps a request larger than the maximum at the maximum", () => {
    // 60 days requested -> clamped to the 30-day maximum.
    expect(
      sessionDurationCeilingSeconds(BigInt(60 * DAY) * NANOS_PER_SECOND),
    ).toBe(MAX_SESSION_DURATION_SECONDS);
  });

  it("honours a request exactly at the maximum", () => {
    expect(
      sessionDurationCeilingSeconds(BigInt(30 * DAY) * NANOS_PER_SECOND),
    ).toBe(MAX_SESSION_DURATION_SECONDS);
  });

  it("falls back to the maximum for a non-positive (malformed) request", () => {
    expect(sessionDurationCeilingSeconds(BigInt(0))).toBe(
      MAX_SESSION_DURATION_SECONDS,
    );
    expect(sessionDurationCeilingSeconds(BigInt(-1) * NANOS_PER_SECOND)).toBe(
      MAX_SESSION_DURATION_SECONDS,
    );
  });

  it("never returns a ceiling below one second for a sub-second request", () => {
    // 500ms requested: too small to be a whole second, but must not collapse
    // to a 0-second ceiling.
    expect(sessionDurationCeilingSeconds(BigInt(500_000_000))).toBe(1);
  });
});

describe("sessionDurationToNanos", () => {
  it("converts seconds to nanoseconds", () => {
    expect(sessionDurationToNanos(HOUR)).toBe(BigInt(HOUR) * NANOS_PER_SECOND);
  });

  it("round-trips with sessionDurationCeilingSeconds", () => {
    const seconds = sessionDurationCeilingSeconds(
      sessionDurationToNanos(8 * HOUR),
    );
    expect(seconds).toBe(8 * HOUR);
  });
});
